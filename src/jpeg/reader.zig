const std = @import("std");

const zigimg = @import("zigimg");

const ImageConfig = struct {
    width: u32,
    height: u32,
    pixel_format: ?zigimg.PixelFormat,
};

const idct = @import("idct.zig");
const HuffTable = @import("HuffTable.zig");

const dbg = std.debug.print;

fn dbgln(comptime fmt: []const u8) void {
    std.debug.print("{s}\n", .{fmt});
}

const max_components = 4;
const max_tc = 1;
const max_th = 3;
const max_tq = 3;

pub const FormatError = error{
    /// SOI marker missing or incorrect
    InvalidSOIMarker,
    RepeatedComponentIdentifier,
    MissingSosMarker,
    ProgressiveACCoefficientsForMoreThanOneComponent,
    UnknownComponentSelector,
    UnexpectedEof,
    BadSuccessiveApproximation,
    BadTdValue,
    BadSpectralSelection,
    UnknownMarker,
    HuffTooLong,
    BadTaValue,
    DhtWrongLength,
    SamplingFactorsTooLarge,
    HuffZeroLength,
    ShortSegmentLength,
    SosWrongLength,
    BadTqValue,
    BadTcValue,
    BadThValue,
    BadPqValue,
    MultipleSofMarkers,
    DqtWrongLength,
    SofWrongLength,
};

pub const UnsupportedError = error{
    Marker,
    NumberComponents,
    LumaChromaSubSamplingRatio,
    Precision,
};

// [Table B.1] Marker code assignments
pub const Marker = enum(u16) {
    // Start Of Frame (Baseline Sequential).
    sof0 = 0xc0,
    // Start Of Frame (Extended Sequential).
    sof1 = 0xc1,
    // Start Of Frame (Progressive).
    sof2 = 0xc2,
    // Define Huffman Table.
    dht = 0xc4,
    // ReSTart (0).
    rst0 = 0xd0,
    // ReSTart (7).
    rst7 = 0xd7,
    // Start Of Image.
    soi = 0xd8,
    // End Of Image.
    eoi = 0xd9,
    // Start Of Scan.
    sos = 0xda,
    // Define Quantization Table.
    dqt = 0xdb,
    // Define Restart Interval.
    dri = 0xdd,
    // COMment
    com = 0xfe,
    // "APPlication specific" markers aren't part of the JPEG spec per se,
    // but in practice, their use is described at
    // https://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/JPEG.html
    app0 = 0xe0,
    app14 = 0xee,
    app15 = 0xef,
    _,
};

// See https://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/JPEG.html#Adobe
pub const AdobeTransform = enum {
    unknown,
    y_cb_cr,
    y_cb_cr_k,
};

const GrayImage = struct {
    img: *zigimg.Image,
    format: zigimg.PixelFormat = zigimg.PixelFormat.grayscale8,
};

// Component specification, specified in section B.2.2.
const Component = struct {
    // Horizontal sampling factor.
    h: i32 = 0,
    // Vertical sampling factor.
    v: i32 = 0,
    // Component identifier.
    c: u8 = 0,
    // Quantization table destination selector.
    tq: u8 = 0,
};

const Decoder = @This();

// Bits holds the unprocessed bits that have been taken from the byte-stream.
// The n least significant bits of a form the unread bits, to be read in MSB to
// LSB order.
const Bits = struct {
    // accumulator
    a: u32 = 0,
    // mask.  m==1<<(n-1) when n>0, with m==0 when n==0.
    m: u32 = 0,
    // the number of unread bits in a.
    n: i32 = 0,
};

al: std.mem.Allocator,
r: std.io.AnyReader,
bits: Bits,

// bytes is a byte buffer, similar to a bufio.Reader, except that it
// has to be able to unread more than 1 byte, due to byte stuffing.
// Byte stuffing is specified in section F.1.2.3.
bytes: struct {
    // buf[i:j] are the buffered bytes read from the underlying
    // io.Reader that haven't yet been passed further on.
    buffer: [4096]u8,
    i: usize = 0,
    j: usize = 0,
    // num_unreadable is the number of bytes to back up i after
    // overshooting. It can be 0, 1 or 2.
    num_unreadable: usize = 0,
},
width: u32 = 0,
height: u32 = 0,

img1: ?GrayImage = null,
img3: ?YCbCrImage = null,

restart_interval: u16 = 0,
num_components: u8 = 0,

// As per section 4.5, there are four modes of operation (selected by the
// SOF? markers): sequential DCT, progressive DCT, lossless and
// hierarchical, although this implementation does not support the latter
// two non-DCT modes. Sequential DCT is further split into baseline and
// extended, as per section 4.11.
baseline: bool = false,
progressive: bool = false,
jfif: bool = false,
adobe_transform_valid: bool = false,
adobe_transform: AdobeTransform = .unknown,
// End-of-Band run, specified in section G.1.2.2.
eob_run: u16 = 0,

comp: [max_components]Component,
huff: [max_tc + 1][max_th + 1]HuffTable,
// Quantization table, in zig-zag order.
quant: [max_tq + 1]idct.Block,
tmp: [2 * idct.block_size]u8 = [_]u8{0} ** (2 * idct.block_size),

// TODO decode reads a JPEG image from r and returns it as an
pub fn decode(al: std.mem.Allocator, r: std.io.AnyReader) !void {
    var d = Decoder{
        .al = al,
        .r = r,
        .bytes = .{ .buffer = [_]u8{0} ** 4096 },
        .comp = [_]Component{.{}} ** max_components,
        .huff = undefined,
        .quant = undefined,
        .bits = undefined,
    };
    try d.dec(false);
}

pub fn decodeConfig(r: std.io.AnyReader) !ImageConfig {
    var d = Decoder{
        .al = undefined,
        .r = r,
        .bytes = .{ .buffer = [_]u8{0} ** 4096 },
        .comp = [_]Component{.{}} ** max_components,
        .huff = undefined,
        .quant = undefined,
        .bits = undefined,
    };
    try d.dec(true);

    return switch (d.num_components) {
        1 => ImageConfig{
            .width = d.width,
            .height = d.height,
            .pixel_format = zigimg.PixelFormat.grayscale8,
        },
        3 => ImageConfig{
            .width = d.width,
            .height = d.height,
            .pixel_format = if (d.isRgb()) zigimg.PixelFormat.rgb24 else null,
        },

        4 => ImageConfig{
            .width = d.width,
            .height = d.height,
            .pixel_format = null,
        },
        else => FormatError.InvalidSOIMarker,
    };
}

// TODO dec reads a JPEG image from r and returns it as an ??
fn dec(self: *Decoder, config_only: bool) !void {
    try self.readFull(self.tmp[0..2]);
    // Check for the Start of Image marker.
    if (self.tmp[0] != 0xFF or self.tmp[1] != @intFromEnum(Marker.soi)) {
        std.debug.print("{x} {x}\n", .{ self.tmp[0], self.tmp[1] });
        return FormatError.InvalidSOIMarker;
    } else {
        dbgln("SOI successfully read");
    }

    while (true) {
        try self.readFull(self.tmp[0..2]);
        while (self.tmp[0] != 0xff) {
            // Strictly speaking, this is a format error. However, libjpeg is
            // liberal in what it accepts. As of version 9, next_marker in
            // jdmarker.c treats this as a warning (JWRN_EXTRANEOUS_DATA) and
            // continues to decode the stream. Even before next_marker sees
            // extraneous data, jpeg_fill_bit_buffer in jdhuff.c reads as many
            // bytes as it can, possibly past the end of a scan's data. It
            // effectively puts back any markers that it overscanned (e.g. an
            // "\xff\xd9" EOI marker), but it does not put back non-marker data,
            // and thus it can silently ignore a small number of extraneous
            // non-marker bytes before next_marker has a chance to see them (and
            // print a warning).
            //
            // We are therefore also liberal in what we accept. Extraneous data
            // is silently ignored.
            //
            // This is similar to, but not exactly the same as, the restart
            // mechanism within a scan (the RST[0-7] markers).
            //
            // Note that extraneous 0xff bytes in e.g. SOS data are escaped as
            // "\xff\x00", and so are detected a little further down below.
            self.tmp[0] = self.tmp[1];
            self.tmp[1] = try self.readByte();
        }

        var marker = self.tmp[1];

        if (marker == 0) {
            // Treat "\xff\x00" as extraneous data.
            continue;
        }
        while (marker == 0xff) {
            // Section B.1.1.2 says, "Any marker may optionally be preceded by any
            // number of fill bytes, which are bytes assigned code X'FF'".
            marker = try self.readByte();
        }
        if (marker == @intFromEnum(Marker.eoi)) {
            // Done!
            break;
        }
        if (@intFromEnum(Marker.rst0) <= marker and marker <= @intFromEnum(Marker.rst7)) {
            // Figures B.2 and B.16 of the specification suggest that restart markers should
            // only occur between Entropy Coded Segments and not after the final ECS.
            // However, some encoders may generate incorrect JPEGs with a final restart
            // marker. That restart marker will be seen here instead of inside the processSOS
            // method, and is ignored as a harmless error. Restart markers have no extra data,
            // so we check for this before we read the 16-bit length of the segment.
            continue;
        }

        // Read the 16-bit length of the segment. The value includes the 2 bytes for the
        // length itself, so we subtract 2 to get the number of remaining bytes.
        try self.readFull(self.tmp[0..2]);
        var n = @as(i32, self.tmp[0]) << 8;
        n = n + @as(i32, self.tmp[1]) - 2;
        if (n < 0) {
            return FormatError.ShortSegmentLength;
        }

        const marker_enum: Marker = @enumFromInt(marker);
        switch (marker_enum) {
            .sof0, .sof1, .sof2 => {
                self.baseline = marker_enum == Marker.sof0;
                self.progressive = marker_enum == Marker.sof1;
                try self.processSof(n);
                dbgln("SOF successfully read");
                if (config_only and self.jfif) {
                    return;
                }
            },
            .dqt => {
                if (config_only) {
                    try self.ignore(n);
                } else {
                    try self.processDqt(n);
                    dbgln("DQT successfully read");
                }
            },
            .dht => {
                if (config_only) {
                    try self.ignore(n);
                } else {
                    try self.processDht(n);
                    dbgln("DHT successfully read");
                }
            },
            .sos => {
                if (config_only) {
                    return;
                }
                try self.processSos(n);
            },
            else => {
                if (@intFromEnum(Marker.app0) <= marker and marker <= @intFromEnum(Marker.app15) or marker == @intFromEnum(Marker.com)) {
                    try self.ignore(n);
                } else if (marker < 0xc0) {
                    // See Table B.1 "Marker code assignments".
                    return FormatError.UnknownMarker;
                } else {
                    dbg("unsupported marker: {x}\n", .{marker});
                    return UnsupportedError.Marker;
                }
            },
        }
    }
}

// ignore ignores the next n bytes.
fn ignore(self: *Decoder, n: i32) !void {
    var local_n = n;
    // Unread the overshot bytes, if any.
    if (self.bytes.num_unreadable > 0) {
        if (self.bits.n >= 8) {
            self.unreadByteStuffedByte();
        }
        self.bytes.num_unreadable = 0;
    }

    while (true) {
        var m = self.bytes.j - self.bytes.i;
        if (m > local_n) {
            m = @intCast(local_n);
        }
        self.bytes.i += m;
        local_n -= @intCast(m);
        if (local_n == 0) {
            break;
        }
        try self.fill();
    }
}

// readByte returns the next byte, whether buffered or not buffered. It does
// not care about byte stuffing.
fn readByte(self: *Decoder) !u8 {
    while (self.bytes.i == self.bytes.j) {
        try self.fill();
    }
    const x = self.bytes.buffer[self.bytes.i];
    self.bytes.i += 1;
    self.bytes.num_unreadable = 0;
    return x;
}

// readFull reads exactly len(p) bytes into p. It does not care about byte
// stuffing.
fn readFull(self: *Decoder, p: []u8) !void {
    var offset: usize = 0;

    // Unread the overshot bytes, if any.
    if (self.bytes.num_unreadable > 0) {
        if (self.bits.n >= 8) {
            self.unreadByteStuffedByte();
        }
        self.bytes.num_unreadable = 0;
    }

    while (offset < p.len) {
        // Calculate how much can be copied from the internal buffer.
        const available = self.bytes.j - self.bytes.i;
        const to_copy = @min(available, p.len - offset);

        // Copy data from the internal buffer to the output buffer.
        @memcpy(p[offset .. offset + to_copy], self.bytes.buffer[self.bytes.i .. self.bytes.i + to_copy]);
        self.bytes.i += to_copy;
        offset += to_copy;

        // If the output buffer is fully written, we're done.
        if (offset == p.len) {
            break;
        }

        // Refill the internal buffer if it's exhausted.
        try self.fill();
    }
}

// fill fills up the d.bytes.buf buffer from the underlying io.Reader. It
// should only be called when there are no unread bytes in d.bytes.
fn fill(self: *Decoder) !void {
    // Ensure all bytes have been read before refilling.
    if (self.bytes.i != self.bytes.j) {
        @panic("Decoder.fill called when unread bytes exist");
    }

    // Preserve the last two bytes for potential unread operations.
    if (self.bytes.j > 2) {
        self.bytes.buffer[0] = self.bytes.buffer[self.bytes.j - 2];
        self.bytes.buffer[1] = self.bytes.buffer[self.bytes.j - 1];
        self.bytes.i = 2;
        self.bytes.j = 2;
    } else {
        self.bytes.i = 0;
        self.bytes.j = 0;
    }

    // Fill the rest of the buffer.
    const n = try self.r.read(self.bytes.buffer[self.bytes.j..]);
    self.bytes.j += n;

    if (n == 0) {
        return error.UnexpectedEOF;
    }
}

// unreadByteStuffedByte undoes the most recent readByteStuffedByte call,
// giving a byte of data back from d.bits to d.bytes. The Huffman look-up table
// requires at least 8 bits for look-up, which means that Huffman decoding can
// sometimes overshoot and read one or two too many bytes. Two-byte overshoot
// can happen when expecting to read a 0xff 0x00 byte-stuffed byte.
fn unreadByteStuffedByte(self: *Decoder) void {
    self.bytes.i -= self.bytes.num_unreadable;
    self.bytes.num_unreadable = 0;
    if (self.bits.n >= 8) {
        self.bits.a >>= 8;
        self.bits.n -= 8;
        self.bits.m >>= 8;
    }
}

// covered in section B.2.2
fn processSof(self: *Decoder, n: i32) !void {
    if (self.num_components != 0) {
        return FormatError.MultipleSofMarkers;
    }

    switch (n) {
        // grayscale image
        6 + 3 * 1 => self.num_components = 1,
        // YCbCr or RGB image
        6 + 3 * 3 => self.num_components = 3,
        // CMYK image
        6 + 3 * 4 => self.num_components = 4,
        else => return UnsupportedError.NumberComponents,
    }

    try self.readFull(self.tmp[0..@intCast(n)]);

    // only support 8-bit precision
    if (self.tmp[0] != 8) {
        return UnsupportedError.Precision;
    }

    self.height = @as(u32, self.tmp[1]) << 8;
    self.height += @as(u32, self.tmp[2]);
    self.width = @as(u32, self.tmp[3]) << 8;
    self.width += @as(u32, self.tmp[4]);

    if (self.tmp[5] != self.num_components) {
        return FormatError.SofWrongLength;
    }

    for (0..self.num_components) |i| {
        self.comp[i].c = self.tmp[6 + 3 * i];

        // Section B.2.2 states that "the value of C_i shall be different from
        // the values of C_1 through C_(i-1)".
        for (0..i) |j| {
            if (self.comp[i].c == self.comp[j].c) {
                return FormatError.RepeatedComponentIdentifier;
            }
        }

        self.comp[i].tq = self.tmp[8 + 3 * i];
        if (self.comp[i].tq > max_tq) {
            return FormatError.BadTqValue;
        }

        const hv = self.tmp[7 + 3 * i];
        var h = hv >> 4;
        var v = hv & 0x0f;
        if (h < 1 or 4 < h or v < 1 or 4 < v) {
            return UnsupportedError.LumaChromaSubSamplingRatio;
        }
        if (h == 3 or v == 3) {
            return UnsupportedError.LumaChromaSubSamplingRatio;
        }
        switch (self.num_components) {
            1 => {
                // If a JPEG image has only one component, section A.2 says "this data
                // is non-interleaved by definition" and section A.2.2 says "[in this
                // case...] the order of data units within a scan shall be left-to-right
                // and top-to-bottom... regardless of the values of H_1 and V_1". Section
                // 4.8.2 also says "[for non-interleaved data], the MCU is defined to be
                // one data unit". Similarly, section A.1.1 explains that it is the ratio
                // of H_i to max_j(H_j) that matters, and similarly for V. For grayscale
                // images, H_1 is the maximum H_j for all components j, so that ratio is
                // always 1. The component's (h, v) is effectively always (1, 1): even if
                // the nominal (h, v) is (2, 1), a 20x5 image is encoded in three 8x8
                // MCUs, not two 16x8 MCUs.
                h = 1;
                v = 1;
            },

            3 => {

                // For YCbCr images, we only support 4:4:4, 4:4:0, 4:2:2, 4:2:0,
                // 4:1:1 or 4:1:0 chroma subsampling ratios. This implies that the
                // (h, v) values for the Y component are either (1, 1), (1, 2),
                // (2, 1), (2, 2), (4, 1) or (4, 2), and the Y component's values
                // must be a multiple of the Cb and Cr component's values. We also
                // assume that the two chroma components have the same subsampling
                // ratio.
                switch (i) {
                    // Y
                    0 => {
                        // We have already verified, above, that h and v are both
                        // either 1, 2 or 4, so invalid (h, v) combinations are those
                        // with v == 4.
                        if (v == 4) return UnsupportedError.LumaChromaSubSamplingRatio;
                    },
                    // Cb
                    1 => {
                        if (@mod(self.comp[0].h, h) != 0 or @mod(self.comp[0].v, v) != 0) {
                            return UnsupportedError.LumaChromaSubSamplingRatio;
                        }
                    },
                    // Cr
                    2 => {
                        if (self.comp[1].h != h or self.comp[1].v != v) {
                            return UnsupportedError.LumaChromaSubSamplingRatio;
                        }
                    },
                    else => return UnsupportedError.NumberComponents,
                }
            },
            4 => {
                // For 4-component images (either CMYK or YCbCrK), we only support two
                // hv vectors: [0x11 0x11 0x11 0x11] and [0x22 0x11 0x11 0x22].
                // Theoretically, 4-component JPEG images could mix and match hv values
                // but in practice, those two combinations are the only ones in use,
                // and it simplifies the applyBlack code below if we can assume that:
                //	- for CMYK, the C and K channels have full samples, and if the M
                //	  and Y channels subsample, they subsample both horizontally and
                //	  vertically.
                //	- for YCbCrK, the Y and K channels have full samples.
                switch (i) {
                    0 => if (hv != 0x11 and hv != 0x22) return UnsupportedError.LumaChromaSubSamplingRatio,
                    1, 2 => if (hv != 0x11) return UnsupportedError.LumaChromaSubSamplingRatio,
                    3 => if (self.comp[0].h != h or self.comp[0].v != v) return UnsupportedError.LumaChromaSubSamplingRatio,
                    else => return UnsupportedError.NumberComponents,
                }
            },
            else => return UnsupportedError.NumberComponents,
        }

        self.comp[i].h = h;
        self.comp[i].v = v;
    }
}

// covered in section B.2.4.1
fn processDqt(self: *Decoder, n: i32) !void {
    var local_n = n;
    loop: while (local_n > 0) {
        local_n -= 1;
        const x = try self.readByte();
        const tq = x & 0x0f;
        if (tq > max_tq) {
            return FormatError.BadTqValue;
        }
        switch (x >> 4) {
            0 => {
                if (local_n < idct.block_size) {
                    return FormatError.BadPqValue;
                }
                local_n -= idct.block_size;
                try self.readFull(self.tmp[0..idct.block_size]);
                for (self.quant[tq], 0..) |_, i| {
                    self.quant[tq][i] = @intCast(self.tmp[i]);
                }
            },
            1 => {
                if (local_n < 2 * idct.block_size) {
                    break :loop;
                }
                local_n -= 2 * idct.block_size;
                try self.readFull(self.tmp[0 .. 2 * idct.block_size]);
                for (self.quant[tq], 0..) |_, i| {
                    const j = @as(i32, self.tmp[2 * i]) << 8;
                    self.quant[tq][i] = j | self.tmp[2 * i + 1];
                }
            },
            else => return FormatError.BadPqValue,
        }
    }
    if (local_n != 0) {
        return FormatError.DqtWrongLength;
    }
}

// processDHT processes a Define Huffman Table marker, and initializes a huffman
// struct from its contents. Specified in section B.2.4.2.
fn processDht(self: *Decoder, n: i32) !void {
    var local_n = n;
    while (local_n > 0) {
        if (local_n < 17) {
            return FormatError.DhtWrongLength;
        }
        try self.readFull(self.tmp[0..17]);
        const tc = self.tmp[0] >> 4;
        if (tc > max_tc) {
            return FormatError.BadTcValue;
        }
        const th = self.tmp[0] & 0x0f;
        // The baseline th <= 1 restriction is specified in table B.5.
        if (th > max_th or (self.baseline and th > 1)) {
            return FormatError.BadThValue;
        }
        const h = &self.huff[tc][th];

        // Read nCodes and h.vals (and derive h.nCodes).
        // nCodes[i] is the number of codes with code length i.
        // h.nCodes is the total number of codes.
        h.num_codes = 0;
        var num_codes = try self.al.alloc(i32, HuffTable.max_code_length);
        defer self.al.free(num_codes);

        for (0..num_codes.len) |i| {
            num_codes[i] = self.tmp[i + 1];
            h.num_codes += num_codes[i];
        }
        if (h.num_codes == 0) {
            return FormatError.HuffZeroLength;
        }
        if (h.num_codes > HuffTable.max_num_codes) {
            return FormatError.HuffTooLong;
        }
        local_n -= h.num_codes + 17;
        if (local_n < 0) {
            return FormatError.DhtWrongLength;
        }
        const huff_codes_len: usize = @intCast(h.num_codes);
        try self.readFull(h.vals[0..huff_codes_len]);

        var code: u32 = 0;
        var x: usize = 0;
        for (0..HuffTable.lut_size) |i| {
            code <<= 1;
            const current_code: usize = @intCast(num_codes[i]);
            for (0..current_code) |_| {
                // The codeLength is 1+i, so shift code by 8-(1+i) to
                // calculate the high bits for every 8-bit sequence
                // whose codeLength's high bits matches code.
                // The high 8 bits of lutValue are the encoded value.
                // The low 8 bits are 1 plus the codeLength.
                const base: u32 = code << @intCast(7 - i);
                const m: u16 = @intCast(2 * i);
                const lut_value: u16 = @as(u16, h.vals[x]) << @intCast(8) | m;
                for (0..@as(u16, 1) << @intCast(7 - i)) |k| {
                    h.lut[base | k] = lut_value;
                }
                code += 1;
                x += 1;
            }
        }

        // Derive minCodes, maxCodes, and valsIndices.
        var c: i32 = 0;
        var index: i32 = 0;
        for (num_codes, 0..) |nc, i| {
            if (nc == 0) {
                h.min_codes[i] = -1;
                h.max_codes[i] = -1;
                h.vals_indices[i] = -1;
            } else {
                h.min_codes[i] = c;
                h.max_codes[i] = c + nc - 1;
                h.vals_indices[i] = index;
                c += nc;
                index += nc;
            }
            c <<= 1;
        }
    }
}

// covered in section B.2.3.
fn processSos(self: *Decoder, n: i32) !void {
    if (self.num_components == 0) {
        return FormatError.MissingSosMarker;
    }

    if (n != 6 or 4 + 2 * self.num_components < n or @mod(n, 2) != 0) {
        return FormatError.SosWrongLength;
    }

    const n_index: usize = @intCast(n);
    try self.readFull(self.tmp[0..n_index]);

    const n_comp = self.tmp[0];

    if (n != 4 + 2 * n_comp) {
        return FormatError.SosWrongLength;
    }

    const ScanComponent = struct {
        id: u8,
        td: u8,
        ta: u8,
    };
    var scan = try self.al.alloc(ScanComponent, max_components);
    var total_hv: i32 = 0;
    for (0..n_comp) |i| {
        const comp_selector = self.tmp[1 + 2 * i];
        var comp_index: usize = 0;
        for (self.comp[0..self.num_components], 0..) |comp, j| {
            if (comp_selector == comp.c) {
                comp_index = j;
                break;
            }
        } else {
            return FormatError.UnknownComponentSelector;
        }
        scan[i].id = @intCast(comp_index);
        // Section B.2.3 states that "the value of Cs_j shall be different from
        // the values of Cs_1 through Cs_(j-1)". Since we have previously
        // verified that a frame's component identifiers (C_i values in section
        // B.2.2) are unique, it suffices to check that the implicit indexes
        // into d.comp are unique.
        for (0..i) |j| {
            if (scan[i].id == scan[j].id) {
                return FormatError.RepeatedComponentIdentifier;
            }
        }
        total_hv += self.comp[comp_index].h * self.comp[comp_index].v;
        // The baseline t <= 1 restriction is specified in table B.3.
        scan[i].td = self.tmp[2 + 2 * i] >> 4;
        var t = scan[i].td;
        if (t > max_th or (self.baseline and t > 1)) {
            return FormatError.BadTdValue;
        }
        scan[i].ta = self.tmp[2 + 2 * i] & 0x0f;
        t = scan[i].ta;
        if (t > max_th or (self.baseline and t > 1)) {
            return FormatError.BadTaValue;
        }

        // Section B.2.3 states that if there is more than one component then the
        // total H*V values in a scan must be <= 10.
        if (self.num_components > 1 and total_hv > 10) {
            return FormatError.SamplingFactorsTooLarge;
        }

        // zigStart and zigEnd are the spectral selection bounds.
        // ah and al are the successive approximation high and low values.
        // The spec calls these values Ss, Se, Ah and Al.
        //
        // For progressive JPEGs, these are the two more-or-less independent
        // aspects of progression. Spectral selection progression is when not
        // all of a block's 64 DCT coefficients are transmitted in one pass.
        // For example, three passes could transmit coefficient 0 (the DC
        // component), coefficients 1-5, and coefficients 6-63, in zig-zag
        // order. Successive approximation is when not all of the bits of a
        // band of coefficients are transmitted in one pass. For example,
        // three passes could transmit the 6 most significant bits, followed
        // by the second-least significant bit, followed by the least
        // significant bit.
        //
        // For sequential JPEGs, these parameters are hard-coded to 0/63/0/0, as
        // per table B.3.
        var zig_start: i32 = 0;
        var zig_end: i32 = idct.block_size - 1;
        var ah: u32 = 0;
        var al: u32 = 0;
        if (self.progressive) {
            zig_start = self.tmp[1 + 2 * n_comp];
            zig_end = self.tmp[2 + 2 * n_comp];
            ah = self.tmp[3 + 2 * n_comp] >> 4;
            al = self.tmp[3 + 2 * n_comp] & 0x0f;
            if ((zig_start == 0 and zig_end != 0) or zig_start > zig_end or idct.block_size <= zig_end) {
                return FormatError.BadSpectralSelection;
            }
            if (zig_start != 0 and n_comp != 1) {
                return FormatError.ProgressiveACCoefficientsForMoreThanOneComponent;
            }
            if (ah != 0 and ah != al + 1) {
                return FormatError.BadSuccessiveApproximation;
            }
        }

        // mxx and myy are the number of MCUs (Minimum Coded Units) in the image.
        // const h0 = self.comp[0].h;
        // const v0 = self.comp[0].v;
        // // const mxx = (self.width + 8 * h0 - 1) / (8 * h0);
        // // const myy = (self.height + 8 * v0 - 1) / (8 * v0);
        // if (self.img1) {}
    }
}

fn isRgb(self: *Decoder) bool {
    if (self.jfif) return false;

    if (self.adobe_transform_valid and self.adobe_transform == .unknown) {
        // https://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/JPEG.html#Adobe
        // says that 0 means Unknown (and in practice RGB) and 1 means YCbCr.
        return true;
    }

    return self.comp[0].c == 'R' and self.comp[1].c == 'G' and self.comp[2].c == 'B';
}
