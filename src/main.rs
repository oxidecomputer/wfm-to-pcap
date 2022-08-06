use anyhow::{anyhow, Context, Result};
use env_logger::Env;
use log::{info, warn};

mod codegroup;
mod wfm;

use codegroup::Codegroup;
use wfm::parse_wfm;

/// Finds all sign transitions in the data
fn get_crossings(pts: &[bool]) -> Vec<usize> {
    pts.iter()
        .zip(&pts[1..])
        .enumerate()
        .filter(|(_, (a, b))| a != b)
        .map(|(i, _)| i)
        .collect()
}

/// Finds comma characters, given as indexes of the first clock transition in
/// the comma.
fn get_commas(
    pts: &[bool],
    crossings: &[usize],
    samples_per_clock: usize,
) -> Vec<usize> {
    // We detect a potential comma if there are > 4.5 bit lengths between
    // transitions, since a comma has 5 identical bits in a row.
    let comma_length = samples_per_clock * 9 / 2;
    crossings
        .iter()
        .zip(&crossings[1..])
        .filter(|&(a, b)| b - a >= comma_length)
        .map(|(a, _b)| *a - samples_per_clock * 2)
        .filter(|a| {
            // A comma is either 001111 or 110000.  We detected the crossing
            // at the beginning of the 1111 or 0000 run, so we backed up by
            // two clock periods (in the `map` above).  Now, we advance by by
            // 1/2 clock period to land in the middle of the bit.
            let i = a + samples_per_clock / 2;
            let mut value = 0;
            for j in 0..6 {
                value <<= 1;
                if pts[i + j * samples_per_clock] {
                    value |= 1;
                }
            }
            value == 0b110000 || value == 0b001111
        })
        .collect()
}

/// Extracts 8b/10b codegroups
fn get_codegroups(
    pts: &[bool],
    crossings: &[usize],
    commas: &[usize],
    samples_per_clock: usize,
) -> Vec<Codegroup> {
    // The comma iterator points to the bit transition at the beginning of the
    // comma codegroup.
    let mut iter_comma = commas.iter().cloned().peekable();

    // Index at which to sample the data.  We start at a half-cycle offset from
    // the bit transition at the beginning of the comma character.
    let mut i = iter_comma.next().unwrap() + samples_per_clock / 2;

    // The zero-crossing iterator points to bit transitions, so we can stay in
    // sync even if it's been a long time since the last comma character.
    let mut iter_cross =
        crossings.iter().cloned().filter(move |c| *c > i).peekable();

    let mut bit = 0;
    let mut value = 0;
    let mut cgs = vec![];
    while i < pts.len() {
        // Resynchronize based on bit transitions
        if let Some(&j) = iter_cross.peek() {
            if i > j {
                i = j + samples_per_clock / 2;
                iter_cross.next();
            }
        }
        // Resynchronize and reset on comma characters
        if let Some(&j) = iter_comma.peek() {
            if i > j {
                i = j + samples_per_clock / 2;
                if bit != 0 {
                    warn!("Off-sync comma character at {j} ({})", cgs.len());
                }
                bit = 0;
                value = 0;
                iter_comma.next();
            }
        }

        value = (value << 1) | (pts[i] as u16);
        bit += 1;
        i += samples_per_clock;
        if bit == 10 {
            let cg = Codegroup::try_from(value).unwrap();
            cgs.push(cg);
            bit = 0;
            value = 0;
        }
    }
    cgs
}

/// Split interleavec codegroups into four ports
fn split_ports(cgs: &[Codegroup]) -> [Vec<Codegroup>; 4] {
    // Split the codegroups into separate streams for each channel
    let mut channel: Option<usize> = None;
    let mut streams: [Vec<Codegroup>; 4] = Default::default();
    for (i, mut cg) in cgs.iter().cloned().enumerate() {
        // Detect the K28.1 sync character
        if cg == Codegroup::K28_1 {
            if channel.map(|c| c != 0).unwrap_or(false) {
                warn!("Out of sync K28.1 at codegroup {}", i);
            }
            channel = Some(0);
            cg = Codegroup::K28_5;
        }
        if let Some(c) = channel.as_mut() {
            streams[*c].push(cg);
            *c = (*c + 1) % 4;
        }
    }
    streams
}

/// Decodes a set of codegroups into PCS-level control and data
fn decode(cgs: &[Codegroup]) -> Vec<Pcs> {
    // Decode a single channel into packets
    let mut cg_iter = cgs.iter().cloned().enumerate();
    let mut pcs = vec![];
    while let Some((i, cg)) = cg_iter.next() {
        // Table 36–3
        let s = match cg {
            Codegroup::K28_5 => match read_k28_5(&mut cg_iter) {
                Some(c) => Pcs::OrderedSet(c),
                None => break,
            },
            Codegroup::K23_7 => Pcs::OrderedSet(OrderedSet::CarrierExtend),
            Codegroup::K27_7 => Pcs::OrderedSet(OrderedSet::StartOfPacket),
            Codegroup::K29_7 => Pcs::OrderedSet(OrderedSet::EndOfPacket),
            Codegroup::K30_7 => Pcs::OrderedSet(OrderedSet::ErrorPropagation),
            d => {
                if !d.is_data() {
                    warn!("Unexpected special codegroup at {i}: {:?}", d);
                    continue;
                }
                Pcs::Data(u8::from(d))
            }
        };
        pcs.push(s);
    }
    pcs
}

/// Splits a stream of PCS data into individual packets, using `/S/` and `/T/`
/// ordered sets as delimiters.
fn get_packets(pcs: &[Pcs]) -> Vec<Vec<u8>> {
    let mut packet = vec![];
    let mut packets: Vec<Vec<u8>> = vec![];
    for (i, o) in pcs.iter().enumerate() {
        let packet_done = match o {
            Pcs::OrderedSet(OrderedSet::StartOfPacket) => {
                if !packet.is_empty() {
                    warn!("Got /S/ without /T/ at ordered set {i}");
                }
                true
            }
            Pcs::OrderedSet(OrderedSet::EndOfPacket) => {
                if packet.is_empty() {
                    warn!("Got /T/ with an empty packet at ordered set {i}");
                }
                true
            }
            Pcs::Data(d) => {
                packet.push(*d);
                false
            }
            _ => false,
        };
        if packet_done {
            let p: Vec<u8> = packet.drain(..).step_by(10).collect();
            if p.len() < 8 {
                warn!("Skipping short packet {:?}", p);
            } else {
                packets.push(p);
            }
            packet.clear();
        }
    }
    if !packet.is_empty() {
        info!("Discarding {} code-groups of partial packet", packet.len());
    }
    packets
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::Builder::from_env(Env::default().default_filter_or("info"))
        .init();

    let mut args = std::env::args();
    args.next().unwrap();
    let filename = args.next().expect("Missing filename");
    info!("Opening {}", filename);

    let data = std::fs::read(filename)?;

    let (_, t) = parse_wfm(data.as_slice())
        .map_err(|e| anyhow!("Failed to parse curve: {}", e))?;
    info!("Loaded {} samples", t.pts.len());

    const QSGMII_CLOCK_RATE: f64 = 5e9;
    const SECS_PER_CLOCK: f64 = 1.0 / QSGMII_CLOCK_RATE;

    // Convert to digital values
    let pts: Vec<bool> = t.pts.iter().map(|p| *p < 0).collect();

    let crossings = get_crossings(&pts);
    info!("Found {} crossings", crossings.len());

    let samples_per_clock = (SECS_PER_CLOCK / t.scale).round() as usize;
    info!("Using {} samples per QSGMII clock", samples_per_clock);

    let commas = get_commas(&pts, &crossings, samples_per_clock);
    info!("Found {} commas", commas.len());

    let cgs = get_codegroups(&pts, &crossings, &commas, samples_per_clock);
    info!("Found {} code-groups", cgs.len());

    let streams = split_ports(&cgs);

    // Ethernet frame preamble and start frame delimiter
    const PREAMBLE_SFD: [u8; 8] =
        [0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0xD5];

    for i in 0..4 {
        let pcs = decode(&streams[0]);
        let packets = get_packets(&pcs);

        let filename = format!("out.pcap.{}", i);
        let cap = pcap::Capture::dead(pcap::Linktype::ETHERNET)?;
        let mut pcap_out = cap.savefile(&filename)?;

        for p in &packets {
            if !p.starts_with(&PREAMBLE_SFD) {
                warn!("Skipping packet without preamble: {:?}", p);
                continue;
            }

            // Use the current time for packet headers
            let t = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .context("Time went backwards")?;
            let packet_header = pcap::PacketHeader {
                ts: libc::timeval {
                    tv_sec: i64::try_from(t.as_secs()).unwrap(),
                    tv_usec: i32::try_from(t.subsec_micros()).unwrap(),
                },
                caplen: u32::try_from(p.len()).unwrap(),
                len: u32::try_from(p.len()).unwrap(),
            };
            let packet = pcap::Packet {
                header: &packet_header,
                data: &p[PREAMBLE_SFD.len()..],
            };
            pcap_out.write(&packet);
        }
        info!(
            "Wrote {} packets from port {i} to {filename}",
            packets.len()
        );
    }

    Ok(())
}

/// Reads the data that follows a K28.5 codegroup, forming an ordered set
///
/// Returns `None` if the stream terminates.
fn read_k28_5(
    mut iter: impl Iterator<Item = (usize, Codegroup)>,
) -> Option<OrderedSet> {
    let out = match iter.next()? {
        (_, Codegroup::D21_5 | Codegroup::D2_2) => {
            let (j1, d1) = iter.next()?;
            let (j2, d2) = iter.next()?;
            if !d1.is_data() {
                warn!("Unexpected special codegroup at {j1}: {:?}", d1);
            }
            if !d2.is_data() {
                warn!("Unexpected special codegroup at {j2}: {:?}", d2);
            }
            OrderedSet::Configuration(d1, d2)
        }

        (_, Codegroup::D5_6 | Codegroup::D16_2) => OrderedSet::Idle,
        (_, Codegroup::D6_5 | Codegroup::D26_4) => OrderedSet::LinkPartnerIdle,
        (j, c) => {
            // "A received ordered set that consists of two
            //  code-groups, the first of which is /K28.5/ and the
            //  second of which is a data code-group other than
            //  /D21.5/ or /D2.2/ (or /D6.5/ or /D26.4/ to support
            //  EEE capability), is treated as an /I/ ordered set."
            //  [36.2.4.12]
            warn!("Unexpected codegroup after K28.5 at {j}: {:?}", c);
            OrderedSet::Idle
        }
    };
    Some(out)
}

enum Pcs {
    OrderedSet(OrderedSet),
    Data(u8),
}

impl std::fmt::Debug for Pcs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pcs::OrderedSet(o) => o.fmt(f),
            Pcs::Data(v) => write!(f, "{:02x}", v),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum OrderedSet {
    /// Configuration
    Configuration(Codegroup, Codegroup),
    /// Idle
    Idle,
    /// Carrier extend
    CarrierExtend,
    /// Start of packet
    StartOfPacket,
    /// End of packet
    EndOfPacket,
    /// Error propagation
    ErrorPropagation,
    /// Link partner idle 1
    LinkPartnerIdle,
}

impl std::fmt::Display for OrderedSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Configuration(a, b) => write!(f, "/C/{}/{}/", a, b),
            Self::Idle => write!(f, "/I/"),
            Self::CarrierExtend => write!(f, "/R/"),
            Self::StartOfPacket => write!(f, "/S/"),
            Self::EndOfPacket => write!(f, "/T/"),
            Self::ErrorPropagation => write!(f, "/V/"),
            Self::LinkPartnerIdle => write!(f, "/LI/"),
        }
    }
}
