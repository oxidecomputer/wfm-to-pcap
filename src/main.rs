use anyhow::Result;
use env_logger::Env;
use log::info;
use nom::{
    bytes::complete::{tag, take},
    multi::count,
    number::complete::{
        le_f32, le_f64, le_i16, le_i32, le_u16, le_u32, le_u64, le_u8,
    },
};

#[allow(dead_code)]
#[derive(Debug)]
struct ExplicitDimension {
    dim_scale: f64,
    dim_offset: f64,
    dim_size: u32,
    units: String,
    dim_resolution: f64,
    dim_ref_point: f64,
    dim_fmt: u32,
    dim_storage: u32,
    user_view: UserView,
}

#[allow(dead_code)]
#[derive(Debug)]
struct UserView {
    user_scale: f64,
    user_units: String,
    user_offset: f64,
    point_density: f64,
    href: f64,
    trig_delay: f64,
}

fn parse_user_view(
    data_in: &[u8],
) -> nom::IResult<&[u8], UserView, nom::error::Error<&[u8]>> {
    let data = data_in;
    let (data, user_scale) = le_f64(data)?;
    let (data, user_units) = take(20usize)(data)?;
    let (data, user_offset) = le_f64(data)?;
    let (data, point_density) = le_f64(data)?;
    let (data, href) = le_f64(data)?;
    let (data, trig_delay) = le_f64(data)?;
    Ok((
        data,
        UserView {
            user_scale,
            user_units: std::str::from_utf8(user_units).unwrap().to_owned(),
            user_offset,
            point_density,
            href,
            trig_delay,
        },
    ))
}

fn parse_explicit_dimension(
    data_in: &[u8],
) -> nom::IResult<&[u8], ExplicitDimension, nom::error::Error<&[u8]>> {
    let data = data_in;
    let (data, dim_scale) = le_f64(data)?;
    let (data, dim_offset) = le_f64(data)?;
    let (data, dim_size) = le_u32(data)?;
    let (data, units) = take(20usize)(data)?;
    let (data, _dim_extent_min) = le_f64(data)?;
    let (data, _dim_extent_max) = le_f64(data)?;
    let (data, dim_resolution) = le_f64(data)?;
    let (data, dim_ref_point) = le_f64(data)?;
    let (data, dim_fmt) = le_u32(data)?;
    let (data, dim_storage) = le_u32(data)?;
    let (data, _n_value) = le_u32(data)?;
    let (data, _over_range) = le_u32(data)?;
    let (data, _under_range) = le_u32(data)?;
    let (data, _high_range) = le_u32(data)?;
    let (data, _low_range) = le_u32(data)?;
    let (data, user_view) = parse_user_view(data)?;
    Ok((
        data,
        ExplicitDimension {
            dim_fmt,
            dim_resolution,
            dim_ref_point,
            dim_storage,
            dim_offset,
            dim_size,
            dim_scale,
            user_view,
            units: std::str::from_utf8(units).unwrap().to_owned(),
        },
    ))
}

#[allow(dead_code)]
#[derive(Debug)]
struct ImplicitDimension {
    dim_scale: f64,
    dim_offset: f64,
    dim_size: u32,
    units: String,
    dim_extent_min: f64,
    dim_extent_max: f64,
    dim_resolution: f64,
    dim_ref_pt: f64,
    dim_spacing: u32,
    user_view: UserView,
}

fn parse_implicit_dimension(
    data_in: &[u8],
) -> nom::IResult<&[u8], ImplicitDimension, nom::error::Error<&[u8]>> {
    let data = data_in;
    let (data, dim_scale) = le_f64(data)?;
    let (data, dim_offset) = le_f64(data)?;
    let (data, dim_size) = le_u32(data)?;
    let (data, units) = take(20usize)(data)?;
    let (data, dim_extent_min) = le_f64(data)?;
    let (data, dim_extent_max) = le_f64(data)?;
    let (data, dim_resolution) = le_f64(data)?;
    let (data, dim_ref_pt) = le_f64(data)?;
    let (data, dim_spacing) = le_u32(data)?;

    let (data, user_view) = parse_user_view(data)?;
    Ok((
        data,
        ImplicitDimension {
            dim_scale,
            dim_offset,
            dim_size,
            units: std::str::from_utf8(units).unwrap().to_owned(),
            dim_extent_min,
            dim_extent_max,
            dim_resolution,
            dim_ref_pt,
            dim_spacing,
            user_view,
        },
    ))
}

#[allow(dead_code)]
#[derive(Debug)]
struct TimeBase {
    real_point_spacing: u32,
    sweep: u32,
    base_type: u32,
}

fn parse_time_base(
    data: &[u8],
) -> nom::IResult<&[u8], TimeBase, nom::error::Error<&[u8]>> {
    let (data, real_point_spacing) = le_u32(data)?;
    let (data, sweep) = le_u32(data)?;
    let (data, base_type) = le_u32(data)?;

    Ok((
        data,
        TimeBase {
            real_point_spacing,
            sweep,
            base_type,
        },
    ))
}

fn parse_wfm_update(
    data: &[u8],
) -> nom::IResult<&[u8], (), nom::error::Error<&[u8]>> {
    let (data, real_pt_offset) = le_u32(data)?;
    let (data, tt_offset) = le_f64(data)?;
    let (data, frac_sec) = le_f64(data)?;
    let (data, gmt_sec) = le_i32(data)?;
    Ok((data, ()))
}

#[allow(dead_code)]
#[derive(Debug)]
struct WfmCurveInfo {
    precharge_start_offset: u32,
    data_start_offset: u32,
    postcharge_start_offset: u32,
    postcharge_stop_offset: u32,
}

fn parse_wfm_curve_info(
    data: &[u8],
) -> nom::IResult<&[u8], WfmCurveInfo, nom::error::Error<&[u8]>> {
    let (data, _state_flags) = le_u32(data)?;
    let (data, _checksum_type) = le_u32(data)?;
    let (data, _checksum) = le_u16(data)?;
    let (data, precharge_start_offset) = le_u32(data)?;
    let (data, data_start_offset) = le_u32(data)?;
    let (data, postcharge_start_offset) = le_u32(data)?;
    let (data, postcharge_stop_offset) = le_u32(data)?;
    let (data, _end_of_curve_buffer_offset) = le_u32(data)?;
    Ok((
        data,
        WfmCurveInfo {
            precharge_start_offset,
            data_start_offset,
            postcharge_start_offset,
            postcharge_stop_offset,
        },
    ))
}

#[allow(dead_code)]
#[derive(Debug)]
struct WaveformStaticFileInfo {
    byte_order: u16,
    version: String,
    ndigits: u8,
    bytes_to_end: u32,
    bytes_per_pt: u8,
    byte_offset: u32,
    hzoom_scale: u32,
    hzoom_pos: f32,
    vzoom_scale: f64,
    vzoom_pos: f32,
    label: String,
    num_fast_frames_minus_1: u32,
    waveform_header_size: u16,
}

fn parse_static_file_info(
    data: &[u8],
) -> nom::IResult<&[u8], WaveformStaticFileInfo, nom::error::Error<&[u8]>> {
    let (data, byte_order) = le_u16(data)?;
    let (data, version) = tag(":WFM#003")(data)?;
    let (data, ndigits) = le_u8(data)?;
    let (data, bytes_to_end) = le_u32(data)?;
    let (data, bytes_per_pt) = le_u8(data)?;
    let (data, byte_offset) = le_u32(data)?;
    let (data, hzoom_scale) = le_u32(data)?;
    let (data, hzoom_pos) = le_f32(data)?;
    let (data, vzoom_scale) = le_f64(data)?;
    let (data, vzoom_pos) = le_f32(data)?;
    let (data, label) = take(32usize)(data)?;
    let (data, num_fast_frames_minus_1) = le_u32(data)?;
    let (data, waveform_header_size) = le_u16(data)?;

    Ok((
        data,
        WaveformStaticFileInfo {
            byte_order,
            version: std::str::from_utf8(version).unwrap().to_owned(),
            ndigits,
            bytes_to_end,
            bytes_per_pt,
            byte_offset,
            hzoom_scale,
            hzoom_pos,
            vzoom_scale,
            vzoom_pos,
            label: std::str::from_utf8(label).unwrap().to_owned(),
            num_fast_frames_minus_1,
            waveform_header_size,
        },
    ))
}

#[allow(dead_code)]
#[derive(Debug)]
struct WaveformHeader {
    set_type: u32,
    waveform_count: u32,
    acquisition_counter: u64,
    wfm_update_spec_cnt: u32,
    implicit_dim_ref_cnt: u32,
    explicit_dim_ref_cnt: u32,
    data_type: u32,
    curve_ref_count: u32,
    summary_frame: u16,
    pix_map_display_format: u32,
}

fn parse_waveform_header(
    data: &[u8],
) -> nom::IResult<&[u8], WaveformHeader, nom::error::Error<&[u8]>> {
    let (data, set_type) = le_u32(data)?;
    assert_eq!(set_type, 0, "FastFrame is not supported");
    let (data, waveform_count) = le_u32(data)?;
    let (data, acquisition_counter) = le_u64(data)?;

    let (data, _transaction_counter) = le_u64(data)?;
    let (data, _slot_id) = le_u32(data)?;
    let (data, _is_static_flag) = le_u32(data)?;

    let (data, wfm_update_spec_cnt) = le_u32(data)?;
    let (data, implicit_dim_ref_cnt) = le_u32(data)?;
    let (data, explicit_dim_ref_cnt) = le_u32(data)?;

    let (data, data_type) = le_u32(data)?;
    assert_eq!(data_type, 2, "expected WFMDATA_VECTOR (2)",);

    let (data, _gen_purpose_counter) = le_u64(data)?;
    let (data, _acc_wfm_counter) = le_u32(data)?;
    let (data, _tgt_acc_counter) = le_u32(data)?;

    let (data, curve_ref_count) = le_u32(data)?;
    assert_eq!(curve_ref_count, 1);

    let (data, _num_requested_fast_frames) = le_u32(data)?;
    let (data, _num_acquired_fast_frames) = le_u32(data)?;

    let (data, summary_frame) = le_u16(data)?;
    assert_eq!(summary_frame, 0, "expected SUMMARY_FRAME_OFF (0)",);

    let (data, pix_map_display_format) = le_u32(data)?;
    let (data, _pixmap_value) = le_u64(data)?;
    Ok((
        data,
        WaveformHeader {
            set_type,
            waveform_count,
            acquisition_counter,
            wfm_update_spec_cnt,
            implicit_dim_ref_cnt,
            explicit_dim_ref_cnt,
            data_type,
            curve_ref_count,
            summary_frame,
            pix_map_display_format,
        },
    ))
}

/// Raw waveform capture
struct Trace {
    /// Raw samples
    pts: Vec<i16>,

    /// Sample point spacing, in seconds
    scale: f64,
}

/// Parses a `.wfm` file
///
/// See [this specification](https://download.tek.com/manual/Waveform-File-Format-Manual-077022011.pdf)
/// for details on the fileformat.
fn parse_wfm(
    data_in: &[u8],
) -> nom::IResult<&[u8], Trace, nom::error::Error<&[u8]>> {
    let data = data_in;
    let (data, _header) = parse_static_file_info(data)?;
    let (data, _header) = parse_waveform_header(data)?;

    // The first explicit dimension is voltage, probably
    let (data, edim1) = parse_explicit_dimension(data)?;
    assert_eq!(edim1.dim_fmt, 0, "expected EXPLICIT_INT16 (0)");
    assert_eq!(edim1.dim_storage, 0, "expected EXPLICIT_SAMPLE (0)");

    // The second explicit dimension is unused
    let (data, _edim2) = parse_explicit_dimension(data)?;

    // The first implicit dimension is time
    let (data, idim1) = parse_implicit_dimension(data)?;

    // The second implicit dimension unused
    let (data, _idim2) = parse_implicit_dimension(data)?;

    // Lots of stuff that we don't care about
    let (data, _tb1) = parse_time_base(data)?;
    let (data, _tb2) = parse_time_base(data)?;
    let (data, _wfm_update) = parse_wfm_update(data)?;

    // The actual curve info!
    let (data, wfm_curve) = parse_wfm_curve_info(data)?;

    // Time to actually read the waveform
    let (data, _skip) = take(wfm_curve.data_start_offset)(data)?;
    let curve_size_bytes =
        wfm_curve.postcharge_start_offset - wfm_curve.data_start_offset;
    let num_pts = (curve_size_bytes as usize) / (std::mem::size_of::<i16>());

    // Pull raw bytes out of the array
    let (data, pts) = count(le_i16, num_pts)(data)?;

    Ok((
        data,
        Trace {
            scale: idim1.dim_scale,
            pts,
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::Builder::from_env(Env::default().default_filter_or("info"))
        .init();

    let mut args = std::env::args();
    args.next().unwrap();
    let filename = args.next().expect("Missing filename");
    info!("Opening {}", filename);

    let data = std::fs::read(filename)?;

    let (_, t) = parse_wfm(data.as_slice()).unwrap();

    const QSGMII_CLOCK_RATE: f64 = 5e9;
    const SECS_PER_CLOCK: f64 = 1.0 / QSGMII_CLOCK_RATE;
    let samples_per_clock = (SECS_PER_CLOCK / t.scale).round() as usize;

    let mut crossings = vec![];
    for (i, (a, b)) in t.pts.iter().zip(&t.pts[1..]).enumerate() {
        if (*a < 0) != (*b < 0) {
            crossings.push(i);
        }
    }
    info!("Found {} crossings", crossings.len());
    let comma_length = samples_per_clock * 4 + samples_per_clock / 2;
    let mut commas = vec![];
    for (a, b) in crossings.iter().zip(&crossings[1..]) {
        if (b - a) >= comma_length {
            commas.push(*a);
        }
    }
    info!("Found {} commas", commas.len());

    // the comma is part of 001111_10xx or 110000_01xx, so we back up two clocks
    // to read the full symbol.
    let mut iter = commas.iter().peekable();
    let mut i = 0;
    let mut value = 0;
    let mut cgs = vec![];
    while let Some(comma) = iter.next() {
        let mut comma: usize = *comma;
        comma -= samples_per_clock / 2 + samples_per_clock;

        // Re-sync to the next comma at two clock periods before the next comma
        // crossing, to avoid clock drift.
        let next = iter.peek().cloned().map(|c| c - 2 * samples_per_clock);
        while comma < t.pts.len()
            && next.map(|next| comma < next).unwrap_or(true)
        {
            value <<= 1;
            if t.pts[comma] > 0 {
                value |= 0;
            } else {
                value |= 1;
            }
            i += 1;
            comma += samples_per_clock;
            if i == 6 {
                print!("_");
            } else if i == 10 {
                let cg = Codegroup::try_from(value).unwrap();
                println!(" => {:?}", cg);
                cgs.push(cg);
                if !matches!(
                    cg,
                    Codegroup::D16_2 | Codegroup::K28_1 | Codegroup::K28_5
                ) {
                    println!("    HI");
                }
                i = 0;
                value = 0;
            }
        }
    }

    Ok(())
}

#[derive(Copy, Clone, Debug)]
enum Codegroup {
    D0_0,
    D1_0,
    D2_0,
    D3_0,
    D4_0,
    D5_0,
    D6_0,
    D7_0,
    D8_0,
    D9_0,
    D10_0,
    D11_0,
    D12_0,
    D13_0,
    D14_0,
    D15_0,
    D16_0,
    D17_0,
    D18_0,
    D19_0,
    D20_0,
    D21_0,
    D22_0,
    D23_0,
    D24_0,
    D25_0,
    D26_0,
    D27_0,
    D28_0,
    D29_0,
    D30_0,
    D31_0,
    D0_1,
    D1_1,
    D2_1,
    D3_1,
    D4_1,
    D5_1,
    D6_1,
    D7_1,
    D8_1,
    D9_1,
    D10_1,
    D11_1,
    D12_1,
    D13_1,
    D14_1,
    D15_1,
    D16_1,
    D17_1,
    D18_1,
    D19_1,
    D20_1,
    D21_1,
    D22_1,
    D23_1,
    D24_1,
    D25_1,
    D26_1,
    D27_1,
    D28_1,
    D29_1,
    D30_1,
    D31_1,
    D0_2,
    D1_2,
    D2_2,
    D3_2,
    D4_2,
    D5_2,
    D6_2,
    D7_2,
    D8_2,
    D9_2,
    D10_2,
    D11_2,
    D12_2,
    D13_2,
    D14_2,
    D15_2,
    D16_2,
    D17_2,
    D18_2,
    D19_2,
    D20_2,
    D21_2,
    D22_2,
    D23_2,
    D24_2,
    D25_2,
    D26_2,
    D27_2,
    D28_2,
    D29_2,
    D30_2,
    D31_2,
    D0_3,
    D1_3,
    D2_3,
    D3_3,
    D4_3,
    D5_3,
    D6_3,
    D7_3,
    D8_3,
    D9_3,
    D10_3,
    D11_3,
    D12_3,
    D13_3,
    D14_3,
    D15_3,
    D16_3,
    D17_3,
    D18_3,
    D19_3,
    D20_3,
    D21_3,
    D22_3,
    D23_3,
    D24_3,
    D25_3,
    D26_3,
    D27_3,
    D28_3,
    D29_3,
    D30_3,
    D31_3,
    D0_4,
    D1_4,
    D2_4,
    D3_4,
    D4_4,
    D5_4,
    D6_4,
    D7_4,
    D8_4,
    D9_4,
    D10_4,
    D11_4,
    D12_4,
    D13_4,
    D14_4,
    D15_4,
    D16_4,
    D17_4,
    D18_4,
    D19_4,
    D20_4,
    D21_4,
    D22_4,
    D23_4,
    D24_4,
    D25_4,
    D26_4,
    D27_4,
    D28_4,
    D29_4,
    D30_4,
    D31_4,
    D0_5,
    D1_5,
    D2_5,
    D3_5,
    D4_5,
    D5_5,
    D6_5,
    D7_5,
    D8_5,
    D9_5,
    D10_5,
    D11_5,
    D12_5,
    D13_5,
    D14_5,
    D15_5,
    D16_5,
    D17_5,
    D18_5,
    D19_5,
    D20_5,
    D21_5,
    D22_5,
    D23_5,
    D24_5,
    D25_5,
    D26_5,
    D27_5,
    D28_5,
    D29_5,
    D30_5,
    D31_5,
    D0_6,
    D1_6,
    D2_6,
    D3_6,
    D4_6,
    D5_6,
    D6_6,
    D7_6,
    D8_6,
    D9_6,
    D10_6,
    D11_6,
    D12_6,
    D13_6,
    D14_6,
    D15_6,
    D16_6,
    D17_6,
    D18_6,
    D19_6,
    D20_6,
    D21_6,
    D22_6,
    D23_6,
    D24_6,
    D25_6,
    D26_6,
    D27_6,
    D28_6,
    D29_6,
    D30_6,
    D31_6,
    D0_7,
    D1_7,
    D2_7,
    D3_7,
    D4_7,
    D5_7,
    D6_7,
    D7_7,
    D8_7,
    D9_7,
    D10_7,
    D11_7,
    D12_7,
    D13_7,
    D14_7,
    D15_7,
    D16_7,
    D17_7,
    D18_7,
    D19_7,
    D20_7,
    D21_7,
    D22_7,
    D23_7,
    D24_7,
    D25_7,
    D26_7,
    D27_7,
    D28_7,
    D29_7,
    D30_7,
    D31_7,
    K28_0,
    K28_1,
    K28_2,
    K28_3,
    K28_4,
    K28_5,
    K28_6,
    K28_7,
    K23_7,
    K27_7,
    K29_7,
    K30_7,
}

impl From<Codegroup> for u8 {
    fn from(c: Codegroup) -> Self {
        use Codegroup::*;
        match c {
            D0_0 => 0x00,
            D1_0 => 0x01,
            D2_0 => 0x02,
            D3_0 => 0x03,
            D4_0 => 0x04,
            D5_0 => 0x05,
            D6_0 => 0x06,
            D7_0 => 0x07,
            D8_0 => 0x08,
            D9_0 => 0x09,
            D10_0 => 0x0A,
            D11_0 => 0x0B,
            D12_0 => 0x0C,
            D13_0 => 0x0D,
            D14_0 => 0x0E,
            D15_0 => 0x0F,
            D16_0 => 0x10,
            D17_0 => 0x11,
            D18_0 => 0x12,
            D19_0 => 0x13,
            D20_0 => 0x14,
            D21_0 => 0x15,
            D22_0 => 0x16,
            D23_0 => 0x17,
            D24_0 => 0x18,
            D25_0 => 0x19,
            D26_0 => 0x1A,
            D27_0 => 0x1B,
            D28_0 => 0x1C,
            D29_0 => 0x1D,
            D30_0 => 0x1E,
            D31_0 => 0x1F,
            D0_1 => 0x20,
            D1_1 => 0x21,
            D2_1 => 0x22,
            D3_1 => 0x23,
            D4_1 => 0x24,
            D5_1 => 0x25,
            D6_1 => 0x26,
            D7_1 => 0x27,
            D8_1 => 0x28,
            D9_1 => 0x29,
            D10_1 => 0x2A,
            D11_1 => 0x2B,
            D12_1 => 0x2C,
            D13_1 => 0x2D,
            D14_1 => 0x2E,
            D15_1 => 0x2F,
            D16_1 => 0x30,
            D17_1 => 0x31,
            D18_1 => 0x32,
            D19_1 => 0x33,
            D20_1 => 0x34,
            D21_1 => 0x35,
            D22_1 => 0x36,
            D23_1 => 0x37,
            D24_1 => 0x38,
            D25_1 => 0x39,
            D26_1 => 0x3A,
            D27_1 => 0x3B,
            D28_1 => 0x3C,
            D29_1 => 0x3D,
            D30_1 => 0x3E,
            D31_1 => 0x3F,
            D0_2 => 0x40,
            D1_2 => 0x41,
            D2_2 => 0x42,
            D3_2 => 0x43,
            D4_2 => 0x44,
            D5_2 => 0x45,
            D6_2 => 0x46,
            D7_2 => 0x47,
            D8_2 => 0x48,
            D9_2 => 0x49,
            D10_2 => 0x4A,
            D11_2 => 0x4B,
            D12_2 => 0x4C,
            D13_2 => 0x4D,
            D14_2 => 0x4E,
            D15_2 => 0x4F,
            D16_2 => 0x50,
            D17_2 => 0x51,
            D18_2 => 0x52,
            D19_2 => 0x53,
            D20_2 => 0x54,
            D21_2 => 0x55,
            D22_2 => 0x56,
            D23_2 => 0x57,
            D24_2 => 0x58,
            D25_2 => 0x59,
            D26_2 => 0x5A,
            D27_2 => 0x5B,
            D28_2 => 0x5C,
            D29_2 => 0x5D,
            D30_2 => 0x5E,
            D31_2 => 0x5F,
            D0_3 => 0x60,
            D1_3 => 0x61,
            D2_3 => 0x62,
            D3_3 => 0x63,
            D4_3 => 0x64,
            D5_3 => 0x65,
            D6_3 => 0x66,
            D7_3 => 0x67,
            D8_3 => 0x68,
            D9_3 => 0x69,
            D10_3 => 0x6A,
            D11_3 => 0x6B,
            D12_3 => 0x6C,
            D13_3 => 0x6D,
            D14_3 => 0x6E,
            D15_3 => 0x6F,
            D16_3 => 0x70,
            D17_3 => 0x71,
            D18_3 => 0x72,
            D19_3 => 0x73,
            D20_3 => 0x74,
            D21_3 => 0x75,
            D22_3 => 0x76,
            D23_3 => 0x77,
            D24_3 => 0x78,
            D25_3 => 0x79,
            D26_3 => 0x7A,
            D27_3 => 0x7B,
            D28_3 => 0x7C,
            D29_3 => 0x7D,
            D30_3 => 0x7E,
            D31_3 => 0x7F,
            D0_4 => 0x80,
            D1_4 => 0x81,
            D2_4 => 0x82,
            D3_4 => 0x83,
            D4_4 => 0x84,
            D5_4 => 0x85,
            D6_4 => 0x86,
            D7_4 => 0x87,
            D8_4 => 0x88,
            D9_4 => 0x89,
            D10_4 => 0x8A,
            D11_4 => 0x8B,
            D12_4 => 0x8C,
            D13_4 => 0x8D,
            D14_4 => 0x8E,
            D15_4 => 0x8F,
            D16_4 => 0x90,
            D17_4 => 0x91,
            D18_4 => 0x92,
            D19_4 => 0x93,
            D20_4 => 0x94,
            D21_4 => 0x95,
            D22_4 => 0x96,
            D23_4 => 0x97,
            D24_4 => 0x98,
            D25_4 => 0x99,
            D26_4 => 0x9A,
            D27_4 => 0x9B,
            D28_4 => 0x9C,
            D29_4 => 0x9D,
            D30_4 => 0x9E,
            D31_4 => 0x9F,
            D0_5 => 0xA0,
            D1_5 => 0xA1,
            D2_5 => 0xA2,
            D3_5 => 0xA3,
            D4_5 => 0xA4,
            D5_5 => 0xA5,
            D6_5 => 0xA6,
            D7_5 => 0xA7,
            D8_5 => 0xA8,
            D9_5 => 0xA9,
            D10_5 => 0xAA,
            D11_5 => 0xAB,
            D12_5 => 0xAC,
            D13_5 => 0xAD,
            D14_5 => 0xAE,
            D15_5 => 0xAF,
            D16_5 => 0xB0,
            D17_5 => 0xB1,
            D18_5 => 0xB2,
            D19_5 => 0xB3,
            D20_5 => 0xB4,
            D21_5 => 0xB5,
            D22_5 => 0xB6,
            D23_5 => 0xB7,
            D24_5 => 0xB8,
            D25_5 => 0xB9,
            D26_5 => 0xBA,
            D27_5 => 0xBB,
            D28_5 => 0xBC,
            D29_5 => 0xBD,
            D30_5 => 0xBE,
            D31_5 => 0xBF,
            D0_6 => 0xC0,
            D1_6 => 0xC1,
            D2_6 => 0xC2,
            D3_6 => 0xC3,
            D4_6 => 0xC4,
            D5_6 => 0xC5,
            D6_6 => 0xC6,
            D7_6 => 0xC7,
            D8_6 => 0xC8,
            D9_6 => 0xC9,
            D10_6 => 0xCA,
            D11_6 => 0xCB,
            D12_6 => 0xCC,
            D13_6 => 0xCD,
            D14_6 => 0xCE,
            D15_6 => 0xCF,
            D16_6 => 0xD0,
            D17_6 => 0xD1,
            D18_6 => 0xD2,
            D19_6 => 0xD3,
            D20_6 => 0xD4,
            D21_6 => 0xD5,
            D22_6 => 0xD6,
            D23_6 => 0xD7,
            D24_6 => 0xD8,
            D25_6 => 0xD9,
            D26_6 => 0xDA,
            D27_6 => 0xDB,
            D28_6 => 0xDC,
            D29_6 => 0xDD,
            D30_6 => 0xDE,
            D31_6 => 0xDF,
            D0_7 => 0xE0,
            D1_7 => 0xE1,
            D2_7 => 0xE2,
            D3_7 => 0xE3,
            D4_7 => 0xE4,
            D5_7 => 0xE5,
            D6_7 => 0xE6,
            D7_7 => 0xE7,
            D8_7 => 0xE8,
            D9_7 => 0xE9,
            D10_7 => 0xEA,
            D11_7 => 0xEB,
            D12_7 => 0xEC,
            D13_7 => 0xED,
            D14_7 => 0xEE,
            D15_7 => 0xEF,
            D16_7 => 0xF0,
            D17_7 => 0xF1,
            D18_7 => 0xF2,
            D19_7 => 0xF3,
            D20_7 => 0xF4,
            D21_7 => 0xF5,
            D22_7 => 0xF6,
            D23_7 => 0xF7,
            D24_7 => 0xF8,
            D25_7 => 0xF9,
            D26_7 => 0xFA,
            D27_7 => 0xFB,
            D28_7 => 0xFC,
            D29_7 => 0xFD,
            D30_7 => 0xFE,
            D31_7 => 0xFF,
            K28_0 => 0x1C,
            K28_1 => 0x3C,
            K28_2 => 0x5C,
            K28_3 => 0x7C,
            K28_4 => 0x9C,
            K28_5 => 0xBC,
            K28_6 => 0xDC,
            K28_7 => 0xFC,
            K23_7 => 0xF7,
            K27_7 => 0xFB,
            K29_7 => 0xFD,
            K30_7 => 0xFE,
        }
    }
}
impl TryFrom<u16> for Codegroup {
    type Error = ();
    fn try_from(value: u16) -> Result<Self, Self::Error> {
        use Codegroup::*;
        let out = match value {
            0b100111_0100 | 0b011000_1011 => D0_0,
            0b011101_0100 | 0b100010_1011 => D1_0,
            0b101101_0100 | 0b010010_1011 => D2_0,
            0b110001_1011 | 0b110001_0100 => D3_0,
            0b110101_0100 | 0b001010_1011 => D4_0,
            0b101001_1011 | 0b101001_0100 => D5_0,
            0b011001_1011 | 0b011001_0100 => D6_0,
            0b111000_1011 | 0b000111_0100 => D7_0,
            0b111001_0100 | 0b000110_1011 => D8_0,
            0b100101_1011 | 0b100101_0100 => D9_0,
            0b010101_1011 | 0b010101_0100 => D10_0,
            0b110100_1011 | 0b110100_0100 => D11_0,
            0b001101_1011 | 0b001101_0100 => D12_0,
            0b101100_1011 | 0b101100_0100 => D13_0,
            0b011100_1011 | 0b011100_0100 => D14_0,
            0b010111_0100 | 0b101000_1011 => D15_0,
            0b011011_0100 | 0b100100_1011 => D16_0,
            0b100011_1011 | 0b100011_0100 => D17_0,
            0b010011_1011 | 0b010011_0100 => D18_0,
            0b110010_1011 | 0b110010_0100 => D19_0,
            0b001011_1011 | 0b001011_0100 => D20_0,
            0b101010_1011 | 0b101010_0100 => D21_0,
            0b011010_1011 | 0b011010_0100 => D22_0,
            0b111010_0100 | 0b000101_1011 => D23_0,
            0b110011_0100 | 0b001100_1011 => D24_0,
            0b100110_1011 | 0b100110_0100 => D25_0,
            0b010110_1011 | 0b010110_0100 => D26_0,
            0b110110_0100 | 0b001001_1011 => D27_0,
            0b001110_1011 | 0b001110_0100 => D28_0,
            0b101110_0100 | 0b010001_1011 => D29_0,
            0b011110_0100 | 0b100001_1011 => D30_0,
            0b101011_0100 | 0b010100_1011 => D31_0,
            0b100111_1001 | 0b011000_1001 => D0_1,
            0b011101_1001 | 0b100010_1001 => D1_1,
            0b101101_1001 | 0b010010_1001 => D2_1,
            0b110001_1001 => D3_1,
            0b110101_1001 | 0b001010_1001 => D4_1,
            0b101001_1001 => D5_1,
            0b011001_1001 => D6_1,
            0b111000_1001 | 0b000111_1001 => D7_1,
            0b111001_1001 | 0b000110_1001 => D8_1,
            0b100101_1001 => D9_1,
            0b010101_1001 => D10_1,
            0b110100_1001 => D11_1,
            0b001101_1001 => D12_1,
            0b101100_1001 => D13_1,
            0b011100_1001 => D14_1,
            0b010111_1001 | 0b101000_1001 => D15_1,
            0b011011_1001 | 0b100100_1001 => D16_1,
            0b100011_1001 => D17_1,
            0b010011_1001 => D18_1,
            0b110010_1001 => D19_1,
            0b001011_1001 => D20_1,
            0b101010_1001 => D21_1,
            0b011010_1001 => D22_1,
            0b111010_1001 | 0b000101_1001 => D23_1,
            0b110011_1001 | 0b001100_1001 => D24_1,
            0b100110_1001 => D25_1,
            0b010110_1001 => D26_1,
            0b110110_1001 | 0b001001_1001 => D27_1,
            0b001110_1001 => D28_1,
            0b101110_1001 | 0b010001_1001 => D29_1,
            0b011110_1001 | 0b100001_1001 => D30_1,
            0b101011_1001 | 0b010100_1001 => D31_1,
            0b100111_0101 | 0b011000_0101 => D0_2,
            0b011101_0101 | 0b100010_0101 => D1_2,
            0b101101_0101 | 0b010010_0101 => D2_2,
            0b110001_0101 => D3_2,
            0b110101_0101 | 0b001010_0101 => D4_2,
            0b101001_0101 => D5_2,
            0b011001_0101 => D6_2,
            0b111000_0101 | 0b000111_0101 => D7_2,
            0b111001_0101 | 0b000110_0101 => D8_2,
            0b100101_0101 => D9_2,
            0b010101_0101 => D10_2,
            0b110100_0101 => D11_2,
            0b001101_0101 => D12_2,
            0b101100_0101 => D13_2,
            0b011100_0101 => D14_2,
            0b010111_0101 | 0b101000_0101 => D15_2,
            0b011011_0101 | 0b100100_0101 => D16_2,
            0b100011_0101 => D17_2,
            0b010011_0101 => D18_2,
            0b110010_0101 => D19_2,
            0b001011_0101 => D20_2,
            0b101010_0101 => D21_2,
            0b011010_0101 => D22_2,
            0b111010_0101 | 0b000101_0101 => D23_2,
            0b110011_0101 | 0b001100_0101 => D24_2,
            0b100110_0101 => D25_2,
            0b010110_0101 => D26_2,
            0b110110_0101 | 0b001001_0101 => D27_2,
            0b001110_0101 => D28_2,
            0b101110_0101 | 0b010001_0101 => D29_2,
            0b011110_0101 | 0b100001_0101 => D30_2,
            0b101011_0101 | 0b010100_0101 => D31_2,
            0b100111_0011 | 0b011000_1100 => D0_3,
            0b011101_0011 | 0b100010_1100 => D1_3,
            0b101101_0011 | 0b010010_1100 => D2_3,
            0b110001_1100 | 0b110001_0011 => D3_3,
            0b110101_0011 | 0b001010_1100 => D4_3,
            0b101001_1100 | 0b101001_0011 => D5_3,
            0b011001_1100 | 0b011001_0011 => D6_3,
            0b111000_1100 | 0b000111_0011 => D7_3,
            0b111001_0011 | 0b000110_1100 => D8_3,
            0b100101_1100 | 0b100101_0011 => D9_3,
            0b010101_1100 | 0b010101_0011 => D10_3,
            0b110100_1100 | 0b110100_0011 => D11_3,
            0b001101_1100 | 0b001101_0011 => D12_3,
            0b101100_1100 | 0b101100_0011 => D13_3,
            0b011100_1100 | 0b011100_0011 => D14_3,
            0b010111_0011 | 0b101000_1100 => D15_3,
            0b011011_0011 | 0b100100_1100 => D16_3,
            0b100011_1100 | 0b100011_0011 => D17_3,
            0b010011_1100 | 0b010011_0011 => D18_3,
            0b110010_1100 | 0b110010_0011 => D19_3,
            0b001011_1100 | 0b001011_0011 => D20_3,
            0b101010_1100 | 0b101010_0011 => D21_3,
            0b011010_1100 | 0b011010_0011 => D22_3,
            0b111010_0011 | 0b000101_1100 => D23_3,
            0b110011_0011 | 0b001100_1100 => D24_3,
            0b100110_1100 | 0b100110_0011 => D25_3,
            0b010110_1100 | 0b010110_0011 => D26_3,
            0b110110_0011 | 0b001001_1100 => D27_3,
            0b001110_1100 | 0b001110_0011 => D28_3,
            0b101110_0011 | 0b010001_1100 => D29_3,
            0b011110_0011 | 0b100001_1100 => D30_3,
            0b101011_0011 | 0b010100_1100 => D31_3,
            0b100111_0010 | 0b011000_1101 => D0_4,
            0b011101_0010 | 0b100010_1101 => D1_4,
            0b101101_0010 | 0b010010_1101 => D2_4,
            0b110001_1101 | 0b110001_0010 => D3_4,
            0b110101_0010 | 0b001010_1101 => D4_4,
            0b101001_1101 | 0b101001_0010 => D5_4,
            0b011001_1101 | 0b011001_0010 => D6_4,
            0b111000_1101 | 0b000111_0010 => D7_4,
            0b111001_0010 | 0b000110_1101 => D8_4,
            0b100101_1101 | 0b100101_0010 => D9_4,
            0b010101_1101 | 0b010101_0010 => D10_4,
            0b110100_1101 | 0b110100_0010 => D11_4,
            0b001101_1101 | 0b001101_0010 => D12_4,
            0b101100_1101 | 0b101100_0010 => D13_4,
            0b011100_1101 | 0b011100_0010 => D14_4,
            0b010111_0010 | 0b101000_1101 => D15_4,
            0b011011_0010 | 0b100100_1101 => D16_4,
            0b100011_1101 | 0b100011_0010 => D17_4,
            0b010011_1101 | 0b010011_0010 => D18_4,
            0b110010_1101 | 0b110010_0010 => D19_4,
            0b001011_1101 | 0b001011_0010 => D20_4,
            0b101010_1101 | 0b101010_0010 => D21_4,
            0b011010_1101 | 0b011010_0010 => D22_4,
            0b111010_0010 | 0b000101_1101 => D23_4,
            0b110011_0010 | 0b001100_1101 => D24_4,
            0b100110_1101 | 0b100110_0010 => D25_4,
            0b010110_1101 | 0b010110_0010 => D26_4,
            0b110110_0010 | 0b001001_1101 => D27_4,
            0b001110_1101 | 0b001110_0010 => D28_4,
            0b101110_0010 | 0b010001_1101 => D29_4,
            0b011110_0010 | 0b100001_1101 => D30_4,
            0b101011_0010 | 0b010100_1101 => D31_4,
            0b100111_1010 | 0b011000_1010 => D0_5,
            0b011101_1010 | 0b100010_1010 => D1_5,
            0b101101_1010 | 0b010010_1010 => D2_5,
            0b110001_1010 => D3_5,
            0b110101_1010 | 0b001010_1010 => D4_5,
            0b101001_1010 => D5_5,
            0b011001_1010 => D6_5,
            0b111000_1010 | 0b000111_1010 => D7_5,
            0b111001_1010 | 0b000110_1010 => D8_5,
            0b100101_1010 => D9_5,
            0b010101_1010 => D10_5,
            0b110100_1010 => D11_5,
            0b001101_1010 => D12_5,
            0b101100_1010 => D13_5,
            0b011100_1010 => D14_5,
            0b010111_1010 | 0b101000_1010 => D15_5,
            0b011011_1010 | 0b100100_1010 => D16_5,
            0b100011_1010 => D17_5,
            0b010011_1010 => D18_5,
            0b110010_1010 => D19_5,
            0b001011_1010 => D20_5,
            0b101010_1010 => D21_5,
            0b011010_1010 => D22_5,
            0b111010_1010 | 0b000101_1010 => D23_5,
            0b110011_1010 | 0b001100_1010 => D24_5,
            0b100110_1010 => D25_5,
            0b010110_1010 => D26_5,
            0b110110_1010 | 0b001001_1010 => D27_5,
            0b001110_1010 => D28_5,
            0b101110_1010 | 0b010001_1010 => D29_5,
            0b011110_1010 | 0b100001_1010 => D30_5,
            0b101011_1010 | 0b010100_1010 => D31_5,
            0b100111_0110 | 0b011000_0110 => D0_6,
            0b011101_0110 | 0b100010_0110 => D1_6,
            0b101101_0110 | 0b010010_0110 => D2_6,
            0b110001_0110 => D3_6,
            0b110101_0110 | 0b001010_0110 => D4_6,
            0b101001_0110 => D5_6,
            0b011001_0110 => D6_6,
            0b111000_0110 | 0b000111_0110 => D7_6,
            0b111001_0110 | 0b000110_0110 => D8_6,
            0b100101_0110 => D9_6,
            0b010101_0110 => D10_6,
            0b110100_0110 => D11_6,
            0b001101_0110 => D12_6,
            0b101100_0110 => D13_6,
            0b011100_0110 => D14_6,
            0b010111_0110 | 0b101000_0110 => D15_6,
            0b011011_0110 | 0b100100_0110 => D16_6,
            0b100011_0110 => D17_6,
            0b010011_0110 => D18_6,
            0b110010_0110 => D19_6,
            0b001011_0110 => D20_6,
            0b101010_0110 => D21_6,
            0b011010_0110 => D22_6,
            0b111010_0110 | 0b000101_0110 => D23_6,
            0b110011_0110 | 0b001100_0110 => D24_6,
            0b100110_0110 => D25_6,
            0b010110_0110 => D26_6,
            0b110110_0110 | 0b001001_0110 => D27_6,
            0b001110_0110 => D28_6,
            0b101110_0110 | 0b010001_0110 => D29_6,
            0b011110_0110 | 0b100001_0110 => D30_6,
            0b101011_0110 | 0b010100_0110 => D31_6,
            0b100111_0001 | 0b011000_1110 => D0_7,
            0b011101_0001 | 0b100010_1110 => D1_7,
            0b101101_0001 | 0b010010_1110 => D2_7,
            0b110001_1110 | 0b110001_0001 => D3_7,
            0b110101_0001 | 0b001010_1110 => D4_7,
            0b101001_1110 | 0b101001_0001 => D5_7,
            0b011001_1110 | 0b011001_0001 => D6_7,
            0b111000_1110 | 0b000111_0001 => D7_7,
            0b111001_0001 | 0b000110_1110 => D8_7,
            0b100101_1110 | 0b100101_0001 => D9_7,
            0b010101_1110 | 0b010101_0001 => D10_7,
            0b110100_1110 | 0b110100_1000 => D11_7,
            0b001101_1110 | 0b001101_0001 => D12_7,
            0b101100_1110 | 0b101100_1000 => D13_7,
            0b011100_1110 | 0b011100_1000 => D14_7,
            0b010111_0001 | 0b101000_1110 => D15_7,
            0b011011_0001 | 0b100100_1110 => D16_7,
            0b100011_0111 | 0b100011_0001 => D17_7,
            0b010011_0111 | 0b010011_0001 => D18_7,
            0b110010_1110 | 0b110010_0001 => D19_7,
            0b001011_0111 | 0b001011_0001 => D20_7,
            0b101010_1110 | 0b101010_0001 => D21_7,
            0b011010_1110 | 0b011010_0001 => D22_7,
            0b111010_0001 | 0b000101_1110 => D23_7,
            0b110011_0001 | 0b001100_1110 => D24_7,
            0b100110_1110 | 0b100110_0001 => D25_7,
            0b010110_1110 | 0b010110_0001 => D26_7,
            0b110110_0001 | 0b001001_1110 => D27_7,
            0b001110_1110 | 0b001110_0001 => D28_7,
            0b101110_0001 | 0b010001_1110 => D29_7,
            0b011110_0001 | 0b100001_1110 => D30_7,
            0b101011_0001 | 0b010100_1110 => D31_7,
            0b001111_0100 | 0b110000_1011 => K28_0,
            0b001111_1001 | 0b110000_0110 => K28_1,
            0b001111_0101 | 0b110000_1010 => K28_2,
            0b001111_0011 | 0b110000_1100 => K28_3,
            0b001111_0010 | 0b110000_1101 => K28_4,
            0b001111_1010 | 0b110000_0101 => K28_5,
            0b001111_0110 | 0b110000_1001 => K28_6,
            0b001111_1000 | 0b110000_0111 => K28_7,
            0b111010_1000 | 0b000101_0111 => K23_7,
            0b110110_1000 | 0b001001_0111 => K27_7,
            0b101110_1000 | 0b010001_0111 => K29_7,
            0b011110_1000 | 0b100001_0111 => K30_7,
            _ => return Err(()),
        };
        Ok(out)
    }
}

/*
 * Table from 802.3-2015 Section 3
    D0.0 00 000 00000 100111 0100 011000 1011
    D1.0 01 000 00001 011101 0100 100010 1011
    D2.0 02 000 00010 101101 0100 010010 1011
    D3.0 03 000 00011 110001 1011 110001 0100
    D4.0 04 000 00100 110101 0100 001010 1011
    D5.0 05 000 00101 101001 1011 101001 0100
    D6.0 06 000 00110 011001 1011 011001 0100
    D7.0 07 000 00111 111000 1011 000111 0100
    D8.0 08 000 01000 111001 0100 000110 1011
    D9.0 09 000 01001 100101 1011 100101 0100
    D10.0 0A 000 01010 010101 1011 010101 0100
    D11.0 0B 000 01011 110100 1011 110100 0100
    D12.0 0C 000 01100 001101 1011 001101 0100
    D13.0 0D 000 01101 101100 1011 101100 0100
    D14.0 0E 000 01110 011100 1011 011100 0100
    D15.0 0F 000 01111 010111 0100 101000 1011
    D16.0 10 000 10000 011011 0100 100100 1011
    D17.0 11 000 10001 100011 1011 100011 0100
    D18.0 12 000 10010 010011 1011 010011 0100
    D19.0 13 000 10011 110010 1011 110010 0100
    D20.0 14 000 10100 001011 1011 001011 0100
    D21.0 15 000 10101 101010 1011 101010 0100
    D22.0 16 000 10110 011010 1011 011010 0100
    D23.0 17 000 10111 111010 0100 000101 1011
    D24.0 18 000 11000 110011 0100 001100 1011
    D25.0 19 000 11001 100110 1011 100110 0100
    D26.0 1A 000 11010 010110 1011 010110 0100
    D27.0 1B 000 11011 110110 0100 001001 1011
    D28.0 1C 000 11100 001110 1011 001110 0100
    D29.0 1D 000 11101 101110 0100 010001 1011
    D30.0 1E 000 11110 011110 0100 100001 1011
    D31.0 1F 000 11111 101011 0100 010100 1011
    D0.1 20 001 00000 100111 1001 011000 1001
    D1.1 21 001 00001 011101 1001 100010 1001
    D2.1 22 001 00010 101101 1001 010010 1001
    D3.1 23 001 00011 110001 1001 110001 1001
    D4.1 24 001 00100 110101 1001 001010 1001
    D5.1 25 001 00101 101001 1001 101001 1001
    D6.1 26 001 00110 011001 1001 011001 1001
    D7.1 27 001 00111 111000 1001 000111 1001
    D8.1 28 001 01000 111001 1001 000110 1001
    D9.1 29 001 01001 100101 1001 100101 1001
    D10.1 2A 001 01010 010101 1001 010101 1001
    D11.1 2B 001 01011 110100 1001 110100 1001
    D12.1 2C 001 01100 001101 1001 001101 1001
    D13.1 2D 001 01101 101100 1001 101100 1001
    D14.1 2E 001 01110 011100 1001 011100 1001
    D15.1 2F 001 01111 010111 1001 101000 1001
    D16.1 30 001 10000 011011 1001 100100 1001
    D17.1 31 001 10001 100011 1001 100011 1001
    D18.1 32 001 10010 010011 1001 010011 1001
    D19.1 33 001 10011 110010 1001 110010 1001
    D20.1 34 001 10100 001011 1001 001011 1001
    D21.1 35 001 10101 101010 1001 101010 1001
    D22.1 36 001 10110 011010 1001 011010 1001
    D23.1 37 001 10111 111010 1001 000101 1001
    D24.1 38 001 11000 110011 1001 001100 1001
    D25.1 39 001 11001 100110 1001 100110 1001
    D26.1 3A 001 11010 010110 1001 010110 1001
    D27.1 3B 001 11011 110110 1001 001001 1001
    D28.1 3C 001 11100 001110 1001 001110 1001
    D29.1 3D 001 11101 101110 1001 010001 1001
    D30.1 3E 001 11110 011110 1001 100001 1001
    D31.1 3F 001 11111 101011 1001 010100 1001
    D0.2 40 010 00000 100111 0101 011000 0101
    D1.2 41 010 00001 011101 0101 100010 0101
    D2.2 42 010 00010 101101 0101 010010 0101
    D3.2 43 010 00011 110001 0101 110001 0101
    D4.2 44 010 00100 110101 0101 001010 0101
    D5.2 45 010 00101 101001 0101 101001 0101
    D6.2 46 010 00110 011001 0101 011001 0101
    D7.2 47 010 00111 111000 0101 000111 0101
    D8.2 48 010 01000 111001 0101 000110 0101
    D9.2 49 010 01001 100101 0101 100101 0101
    D10.2 4A 010 01010 010101 0101 010101 0101
    D11.2 4B 010 01011 110100 0101 110100 0101
    D12.2 4C 010 01100 001101 0101 001101 0101
    D13.2 4D 010 01101 101100 0101 101100 0101
    D14.2 4E 010 01110 011100 0101 011100 0101
    D15.2 4F 010 01111 010111 0101 101000 0101
    D16.2 50 010 10000 011011 0101 100100 0101
    D17.2 51 010 10001 100011 0101 100011 0101
    D18.2 52 010 10010 010011 0101 010011 0101
    D19.2 53 010 10011 110010 0101 110010 0101
    D20.2 54 010 10100 001011 0101 001011 0101
    D21.2 55 010 10101 101010 0101 101010 0101
    D22.2 56 010 10110 011010 0101 011010 0101
    D23.2 57 010 10111 111010 0101 000101 0101
    D24.2 58 010 11000 110011 0101 001100 0101
    D25.2 59 010 11001 100110 0101 100110 0101
    D26.2 5A 010 11010 010110 0101 010110 0101
    D27.2 5B 010 11011 110110 0101 001001 0101
    D28.2 5C 010 11100 001110 0101 001110 0101
    D29.2 5D 010 11101 101110 0101 010001 0101
    D30.2 5E 010 11110 011110 0101 100001 0101
    D31.2 5F 010 11111 101011 0101 010100 0101
    D0.3 60 011 00000 100111 0011 011000 1100
    D1.3 61 011 00001 011101 0011 100010 1100
    D2.3 62 011 00010 101101 0011 010010 1100
    D3.3 63 011 00011 110001 1100 110001 0011
    D4.3 64 011 00100 110101 0011 001010 1100
    D5.3 65 011 00101 101001 1100 101001 0011
    D6.3 66 011 00110 011001 1100 011001 0011
    D7.3 67 011 00111 111000 1100 000111 0011
    D8.3 68 011 01000 111001 0011 000110 1100
    D9.3 69 011 01001 100101 1100 100101 0011
    D10.3 6A 011 01010 010101 1100 010101 0011
    D11.3 6B 011 01011 110100 1100 110100 0011
    D12.3 6C 011 01100 001101 1100 001101 0011
    D13.3 6D 011 01101 101100 1100 101100 0011
    D14.3 6E 011 01110 011100 1100 011100 0011
    D15.3 6F 011 01111 010111 0011 101000 1100
    D16.3 70 011 10000 011011 0011 100100 1100
    D17.3 71 011 10001 100011 1100 100011 0011
    D18.3 72 011 10010 010011 1100 010011 0011
    D19.3 73 011 10011 110010 1100 110010 0011
    D20.3 74 011 10100 001011 1100 001011 0011
    D21.3 75 011 10101 101010 1100 101010 0011
    D22.3 76 011 10110 011010 1100 011010 0011
    D23.3 77 011 10111 111010 0011 000101 1100
    D24.3 78 011 11000 110011 0011 001100 1100
    D25.3 79 011 11001 100110 1100 100110 0011
    D26.3 7A 011 11010 010110 1100 010110 0011
    D27.3 7B 011 11011 110110 0011 001001 1100
    D28.3 7C 011 11100 001110 1100 001110 0011
    D29.3 7D 011 11101 101110 0011 010001 1100
    D30.3 7E 011 11110 011110 0011 100001 1100
    D31.3 7F 011 11111 101011 0011 010100 1100
    D0.4 80 100 00000 100111 0010 011000 1101
    D1.4 81 100 00001 011101 0010 100010 1101
    D2.4 82 100 00010 101101 0010 010010 1101
    D3.4 83 100 00011 110001 1101 110001 0010
    D4.4 84 100 00100 110101 0010 001010 1101
    D5.4 85 100 00101 101001 1101 101001 0010
    D6.4 86 100 00110 011001 1101 011001 0010
    D7.4 87 100 00111 111000 1101 000111 0010
    D8.4 88 100 01000 111001 0010 000110 1101
    D9.4 89 100 01001 100101 1101 100101 0010
    D10.4 8A 100 01010 010101 1101 010101 0010
    D11.4 8B 100 01011 110100 1101 110100 0010
    D12.4 8C 100 01100 001101 1101 001101 0010
    D13.4 8D 100 01101 101100 1101 101100 0010
    D14.4 8E 100 01110 011100 1101 011100 0010
    D15.4 8F 100 01111 010111 0010 101000 1101
    D16.4 90 100 10000 011011 0010 100100 1101
    D17.4 91 100 10001 100011 1101 100011 0010
    D18.4 92 100 10010 010011 1101 010011 0010
    D19.4 93 100 10011 110010 1101 110010 0010
    D20.4 94 100 10100 001011 1101 001011 0010
    D21.4 95 100 10101 101010 1101 101010 0010
    D22.4 96 100 10110 011010 1101 011010 0010
    D23.4 97 100 10111 111010 0010 000101 1101
    D24.4 98 100 11000 110011 0010 001100 1101
    D25.4 99 100 11001 100110 1101 100110 0010
    D26.4 9A 100 11010 010110 1101 010110 0010
    D27.4 9B 100 11011 110110 0010 001001 1101
    D28.4 9C 100 11100 001110 1101 001110 0010
    D29.4 9D 100 11101 101110 0010 010001 1101
    D30.4 9E 100 11110 011110 0010 100001 1101
    D31.4 9F 100 11111 101011 0010 010100 1101
    D0.5 A0 101 00000 100111 1010 011000 1010
    D1.5 A1 101 00001 011101 1010 100010 1010
    D2.5 A2 101 00010 101101 1010 010010 1010
    D3.5 A3 101 00011 110001 1010 110001 1010
    D4.5 A4 101 00100 110101 1010 001010 1010
    D5.5 A5 101 00101 101001 1010 101001 1010
    D6.5 A6 101 00110 011001 1010 011001 1010
    D7.5 A7 101 00111 111000 1010 000111 1010
    D8.5 A8 101 01000 111001 1010 000110 1010
    D9.5 A9 101 01001 100101 1010 100101 1010
    D10.5 AA 101 01010 010101 1010 010101 1010
    D11.5 AB 101 01011 110100 1010 110100 1010
    D12.5 AC 101 01100 001101 1010 001101 1010
    D13.5 AD 101 01101 101100 1010 101100 1010
    D14.5 AE 101 01110 011100 1010 011100 1010
    D15.5 AF 101 01111 010111 1010 101000 1010
    D16.5 B0 101 10000 011011 1010 100100 1010
    D17.5 B1 101 10001 100011 1010 100011 1010
    D18.5 B2 101 10010 010011 1010 010011 1010
    D19.5 B3 101 10011 110010 1010 110010 1010
    D20.5 B4 101 10100 001011 1010 001011 1010
    D21.5 B5 101 10101 101010 1010 101010 1010
    D22.5 B6 101 10110 011010 1010 011010 1010
    D23.5 B7 101 10111 111010 1010 000101 1010
    D24.5 B8 101 11000 110011 1010 001100 1010
    D25.5 B9 101 11001 100110 1010 100110 1010
    D26.5 BA 101 11010 010110 1010 010110 1010
    D27.5 BB 101 11011 110110 1010 001001 1010
    D28.5 BC 101 11100 001110 1010 001110 1010
    D29.5 BD 101 11101 101110 1010 010001 1010
    D30.5 BE 101 11110 011110 1010 100001 1010
    D31.5 BF 101 11111 101011 1010 010100 1010
    D0.6 C0 110 00000 100111 0110 011000 0110
    D1.6 C1 110 00001 011101 0110 100010 0110
    D2.6 C2 110 00010 101101 0110 010010 0110
    D3.6 C3 110 00011 110001 0110 110001 0110
    D4.6 C4 110 00100 110101 0110 001010 0110
    D5.6 C5 110 00101 101001 0110 101001 0110
    D6.6 C6 110 00110 011001 0110 011001 0110
    D7.6 C7 110 00111 111000 0110 000111 0110
    D8.6 C8 110 01000 111001 0110 000110 0110
    D9.6 C9 110 01001 100101 0110 100101 0110
    D10.6 CA 110 01010 010101 0110 010101 0110
    D11.6 CB 110 01011 110100 0110 110100 0110
    D12.6 CC 110 01100 001101 0110 001101 0110
    D13.6 CD 110 01101 101100 0110 101100 0110
    D14.6 CE 110 01110 011100 0110 011100 0110
    D15.6 CF 110 01111 010111 0110 101000 0110
    D16.6 D0 110 10000 011011 0110 100100 0110
    D17.6 D1 110 10001 100011 0110 100011 0110
    D18.6 D2 110 10010 010011 0110 010011 0110
    D19.6 D3 110 10011 110010 0110 110010 0110
    D20.6 D4 110 10100 001011 0110 001011 0110
    D21.6 D5 110 10101 101010 0110 101010 0110
    D22.6 D6 110 10110 011010 0110 011010 0110
    D23.6 D7 110 10111 111010 0110 000101 0110
    D24.6 D8 110 11000 110011 0110 001100 0110
    D25.6 D9 110 11001 100110 0110 100110 0110
    D26.6 DA 110 11010 010110 0110 010110 0110
    D27.6 DB 110 11011 110110 0110 001001 0110
    D28.6 DC 110 11100 001110 0110 001110 0110
    D29.6 DD 110 11101 101110 0110 010001 0110
    D30.6 DE 110 11110 011110 0110 100001 0110
    D31.6 DF 110 11111 101011 0110 010100 0110
    D0.7 E0 111 00000 100111 0001 011000 1110
    D1.7 E1 111 00001 011101 0001 100010 1110
    D2.7 E2 111 00010 101101 0001 010010 1110
    D3.7 E3 111 00011 110001 1110 110001 0001
    D4.7 E4 111 00100 110101 0001 001010 1110
    D5.7 E5 111 00101 101001 1110 101001 0001
    D6.7 E6 111 00110 011001 1110 011001 0001
    D7.7 E7 111 00111 111000 1110 000111 0001
    D8.7 E8 111 01000 111001 0001 000110 1110
    D9.7 E9 111 01001 100101 1110 100101 0001
    D10.7 EA 111 01010 010101 1110 010101 0001
    D11.7 EB 111 01011 110100 1110 110100 1000
    D12.7 EC 111 01100 001101 1110 001101 0001
    D13.7 ED 111 01101 101100 1110 101100 1000
    D14.7 EE 111 01110 011100 1110 011100 1000
    D15.7 EF 111 01111 010111 0001 101000 1110
    D16.7 F0 111 10000 011011 0001 100100 1110
    D17.7 F1 111 10001 100011 0111 100011 0001
    D18.7 F2 111 10010 010011 0111 010011 0001
    D19.7 F3 111 10011 110010 1110 110010 0001
    D20.7 F4 111 10100 001011 0111 001011 0001
    D21.7 F5 111 10101 101010 1110 101010 0001
    D22.7 F6 111 10110 011010 1110 011010 0001
    D23.7 F7 111 10111 111010 0001 000101 1110
    D24.7 F8 111 11000 110011 0001 001100 1110
    D25.7 F9 111 11001 100110 1110 100110 0001
    D26.7 FA 111 11010 010110 1110 010110 0001
    D27.7 FB 111 11011 110110 0001 001001 1110
    D28.7 FC 111 11100 001110 1110 001110 0001
    D29.7 FD 111 11101 101110 0001 010001 1110
    D30.7 FE 111 11110 011110 0001 100001 1110
    D31.7 FF 111 11111 101011 0001 010100 1110
    K28.0 1C 000 11100 001111 0100 110000 1011
    K28.1 3C 001 11100 001111 1001 110000 0110
    K28.2 5C 010 11100 001111 0101 110000 1010
    K28.3 7C 011 11100 001111 0011 110000 1100
    K28.4 9C 100 11100 001111 0010 110000 1101
    K28.5 BC 101 11100 001111 1010 110000 0101
    K28.6 DC 110 11100 001111 0110 110000 1001
    K28.7 FC 111 11100 001111 1000 110000 0111
    K23.7 F7 111 10111 111010 1000 000101 0111
    K27.7 FB 111 11011 110110 1000 001001 0111
    K29.7 FD 111 11101 101110 1000 010001 0111
    K30.7 FE 111 11110 011110 1000 100001 0111
*/
