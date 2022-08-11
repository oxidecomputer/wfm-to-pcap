// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

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

#[allow(dead_code)]
#[derive(Debug)]
struct WfmUpdate {
    real_point_offset: u32,
    tt_offset: f64,
    frac_sec: f64,
    gmt_sec: i32,
}

fn parse_wfm_update(
    data: &[u8],
) -> nom::IResult<&[u8], WfmUpdate, nom::error::Error<&[u8]>> {
    let (data, real_point_offset) = le_u32(data)?;
    let (data, tt_offset) = le_f64(data)?;
    let (data, frac_sec) = le_f64(data)?;
    let (data, gmt_sec) = le_i32(data)?;
    Ok((
        data,
        WfmUpdate {
            real_point_offset,
            tt_offset,
            frac_sec,
            gmt_sec,
        },
    ))
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
pub struct WfmCurve {
    /// Raw samples
    pub pts: Vec<i16>,

    /// Sample point spacing, in seconds
    pub scale: f64,
}

/// Parses a `.wfm` file
///
/// See [this specification](https://download.tek.com/manual/Waveform-File-Format-Manual-077022011.pdf)
/// for details on the file format.
pub fn parse_wfm(
    data_in: &[u8],
) -> nom::IResult<&[u8], WfmCurve, nom::error::Error<&[u8]>> {
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
        WfmCurve {
            scale: idim1.dim_scale,
            pts,
        },
    ))
}
