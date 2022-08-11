This repository demonstrates a minimal workflow to go from oscilloscope data to
decoded packets suitable for analysis with Wireshark.

The pipeline includes
- A minimal parser for Tektronics `.wfm` files
- QSGMII comma detection and code-group extraction
- Separating code-groups into packets based on special characters
- Writing a `.pcap` file out

For a detailed write-up, check out
[this blog post](https://www.mattkeeter.com/blog/2022-08-11-udp/).
