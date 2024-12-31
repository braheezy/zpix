//! JPEG Decoder module.
//! JPEG is defined in ITU-T T.81: https://www.w3.org/Graphics/JPEG/itu-t81.pdf.
const std = @import("std");

pub const image = @import("../image.zig");
pub const imageutil = @import("../imageutil.zig");
const idct = @import("idct.zig");
const HuffTable = @import("HuffTable.zig");

/// JPEG-defined constants.
const max_components = 4;
const max_tc = 1;
const max_th = 3;
const max_tq = 3;
const ac_table = 1;
const dc_table = 0;

/// [Table B.1] Marker code assignments
pub const Marker = enum(u8) {
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

/// See https://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/JPEG.html#Adobe
pub const AdobeTransform = enum {
    unknown,
    y_cb_cr,
    y_cb_cr_k,
};

/// Component specification, specified in section B.2.2.
const Component = struct {
    // Horizontal sampling factor.
    h: i32 = 0,
    // Vertical sampling factor.
    v: i32 = 0,
    // Component identifier.
    id: u8 = 0,
    // Quantization table destination selector.
    tq: u8 = 0,
};

/// unzig maps from the zig-zag ordering to the natural ordering. For example,
/// unzig[3] is the column and row of the fourth element in zig-zag order. The
/// value is 16, which means first column (16%8 == 0) and third row (16/8 == 2).
const unzig: [idct.block_size]u8 = [_]u8{
    0,  1,  8,  16, 9,  2,  3,  10,
    17, 24, 32, 25, 18, 11, 4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13, 6,  7,  14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63,
};

/// Main decoder struct.
const Decoder = @This();

/// Bits holds the unprocessed bits that have been taken from the byte-stream.
/// The n least significant bits of a form the unread bits, to be read in MSB to
/// LSB order.
const Bits = struct {
    // accumulator
    a: u32 = 0,
    // mask.  m==1<<(n-1) when n>0, with m==0 when n==0.
    m: u32 = 0,
    // the number of unread bits in a.
    n: i32 = 0,
};

// memory allocator
al: std.mem.Allocator,
// reader into provided JPEG data
r: std.io.AnyReader,
bits: Bits,

// bytes is a byte buffer that it is able to unread more than 1 byte,
// due to byte stuffing, as specified in section F.1.2.3.
bytes: struct {
    // buffer[i:j] are the buffered bytes read from the underlying
    // `r` reader that haven't yet been passed further on.
    buffer: [4096]u8,
    i: usize = 0,
    j: usize = 0,
    // num_unreadable is the number of bytes to back up i after
    // overshooting. It can be 0, 1 or 2.
    num_unreadable: usize = 0,
},
// image dimensions
width: u32 = 0,
height: u32 = 0,

// destination image data
// img1: ?*image.GrayImage = null,
img: ?image.Image = null,
black_pixels: ?[]u8 = null,
black_stride: usize = 0,

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

component: [max_components]Component,
// Saved state between progressive-mode scans.
progressive_coefficients: [max_components]?[]idct.Block,
huff: [max_tc + 1][max_th + 1]HuffTable,
// Quantization table, in zig-zag order.
quant: [max_tq + 1]idct.Block,
// preallocated temp buffer to reuse while reading
tmp: [2 * idct.block_size]u8 = [_]u8{0} ** (2 * idct.block_size),

pub fn decode(al: std.mem.Allocator, r: std.io.AnyReader) !image.Image {
    var d = Decoder{
        .al = al,
        .r = r,
        .bytes = .{ .buffer = [_]u8{0} ** 4096 },
        .component = [_]Component{.{}} ** max_components,
        .huff = undefined,
        .quant = undefined,
        .bits = undefined,
        .progressive_coefficients = [_]?[]idct.Block{ null, null, null, null },
    };

    defer {
        for (d.progressive_coefficients) |slice_opt| {
            if (slice_opt) |slice| {
                d.al.free(slice);
            }
        }
    }

    return try d.decodeInner(false);
}

pub fn decodeConfig(r: std.io.AnyReader) !image.Config {
    var d = Decoder{
        .al = undefined,
        .r = r,
        .bytes = .{ .buffer = [_]u8{0} ** 4096 },
        .component = [_]Component{.{}} ** max_components,
        .huff = undefined,
        .quant = undefined,
        .bits = undefined,
    };
    _ = d.decodeInner(true) catch |err| {
        if (err != error.ConfigOnly) {
            return err;
        }
    };

    return switch (d.num_components) {
        1 => {
            return image.Config{
                .width = d.width,
                .height = d.height,
                .color_model = .Gray,
            };
        },
        3 => {
            return image.Config{
                .width = d.width,
                .height = d.height,
                .color_model = .YCbCr,
            };
        },
        4 => image.Config{
            .width = d.width,
            .height = d.height,
            // TODO: Support CMYK
            .color_model = .CMYK,
        },
        else => error.InvalidSOIMarker,
    };
}

fn decodeInner(self: *Decoder, config_only: bool) !image.Image {
    try self.readFull(self.tmp[0..2]);

    // Check for the Start of Image marker.
    if (self.tmp[0] != 0xFF or self.tmp[1] != @intFromEnum(Marker.soi)) {
        return error.InvalidSOIMarker;
    }

    // Process the remaining segments until the End Of Image marker.
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
            return error.ShortSegmentLength;
        }

        const marker_enum: Marker = @enumFromInt(marker);
        switch (marker_enum) {
            .sof0, .sof1, .sof2 => {
                self.baseline = marker_enum == Marker.sof0;
                self.progressive = marker_enum == Marker.sof2;
                try self.processSof(n);
                if (config_only and self.jfif) {
                    return error.ConfigOnly;
                }
            },
            .dqt => {
                if (config_only) {
                    try self.ignore(n);
                } else {
                    try self.processDqt(n);
                }
            },
            .dht => {
                if (config_only) {
                    try self.ignore(n);
                } else {
                    try self.processDht(n);
                }
            },
            .sos => {
                if (config_only) {
                    return error.ConfigOnly;
                }
                try self.processSos(n);
            },
            .app0 => try self.processApp0Marker(n),
            .app14 => try self.processApp14Marker(n),
            else => {
                if (@intFromEnum(Marker.app0) <= marker and marker <= @intFromEnum(Marker.app15) or marker == @intFromEnum(Marker.com)) {
                    try self.ignore(n);
                } else if (marker < 0xc0) {
                    // See Table B.1 "Marker code assignments".
                    return error.UnknownMarker;
                } else {
                    return error.UnsupportedMarker;
                }
            },
        }
    }

    if (self.progressive) {
        try self.reconstructProgressiveImage();
    }

    switch (self.img.?) {
        .Gray => |*gray_img| {
            return image.Image{ .Gray = gray_img.* };
        },
        .YCbCr => |*ycbcr_img| {
            if (self.black_pixels) |_| {
                // Black channel handling is not supported yet.
                return self.applyBlack();
            } else if (self.isRgb()) {
                return try self.convertToRGB();
            }
            return image.Image{ .YCbCr = ycbcr_img.* };
        },
        else => {
            return error.UnsupportedImageType;
        },
    }
    return error.MissingSosMarker;
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
        var remaining_bytes = self.bytes.j - self.bytes.i;
        if (remaining_bytes > local_n) {
            remaining_bytes = @intCast(local_n);
        }
        self.bytes.i += remaining_bytes;
        local_n -= @intCast(remaining_bytes);
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

// fill fills up the self.bytes.buffer from the underlying reader. It
// should only be called when there are no unread bytes in self.bytes.
fn fill(self: *Decoder) !void {
    // Ensure all bytes have been read before refilling.
    if (self.bytes.i != self.bytes.j) {
        @panic("Decoder.fill() called when unread bytes exist");
    }

    // Move the last 2 bytes to the start of the buffer, in case we need
    // to call unreadByteStuffedByte.
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
        return error.UnexpectedEof;
    }
}

// unreadByteStuffedByte undoes the most recent readByteStuffedByte call,
// giving a byte of data back from self.bits to self.bytes. The Huffman look-up table
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
        return error.MultipleSofMarkers;
    }

    self.num_components = switch (n) {
        // grayscale image
        6 + 3 * 1 => 1,
        // YCbCr or RGB image
        6 + 3 * 3 => 3,
        // CMYK image
        6 + 3 * 4 => 4,
        else => return error.NumberComponents,
    };

    try self.readFull(self.tmp[0..@intCast(n)]);

    // only support 8-bit precision
    if (self.tmp[0] != 8) {
        return error.Precision;
    }

    self.height = @as(u32, self.tmp[1]) << 8;
    self.height += @as(u32, self.tmp[2]);
    self.width = @as(u32, self.tmp[3]) << 8;
    self.width += @as(u32, self.tmp[4]);

    if (self.tmp[5] != self.num_components) {
        return error.SofWrongLength;
    }

    for (0..self.num_components) |i| {
        self.component[i].id = self.tmp[6 + 3 * i];

        // Section B.2.2 states that "the value of C_i shall be different from
        // the values of C_1 through C_(i-1)".
        for (0..i) |j| {
            if (self.component[i].id == self.component[j].id) {
                return error.RepeatedComponentIdentifier;
            }
        }

        self.component[i].tq = self.tmp[8 + 3 * i];
        if (self.component[i].tq > max_tq) {
            return error.BadTqValue;
        }

        const hv_sampling = self.tmp[7 + 3 * i];
        var h = hv_sampling >> 4;
        var v = hv_sampling & 0x0f;
        if (h < 1 or 4 < h or v < 1 or 4 < v) {
            return error.LumaChromaSubSamplingRatio;
        }
        if (h == 3 or v == 3) {
            return error.LumaChromaSubSamplingRatio;
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
                        if (v == 4) return error.LumaChromaSubSamplingRatio;
                    },
                    // Cb
                    1 => {
                        if (@mod(self.component[0].h, h) != 0 or @mod(self.component[0].v, v) != 0) {
                            return error.LumaChromaSubSamplingRatio;
                        }
                    },
                    // Cr
                    2 => {
                        if (self.component[1].h != h or self.component[1].v != v) {
                            return error.LumaChromaSubSamplingRatio;
                        }
                    },
                    else => return error.NumberComponents,
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
                    0 => if (hv_sampling != 0x11 and hv_sampling != 0x22) return error.LumaChromaSubSamplingRatio,
                    1, 2 => if (hv_sampling != 0x11) return error.LumaChromaSubSamplingRatio,
                    3 => if (self.component[0].h != h or self.component[0].v != v) return error.LumaChromaSubSamplingRatio,
                    else => return error.NumberComponents,
                }
            },
            else => return error.NumberComponents,
        }

        self.component[i].h = h;
        self.component[i].v = v;
    }
}

// covered in section B.2.4.1
fn processDqt(self: *Decoder, n: i32) !void {
    var local_n = n;
    loop: while (local_n > 0) {
        local_n -= 1;
        const quant_info = try self.readByte();
        const tq = quant_info & 0x0f;
        if (tq > max_tq) {
            return error.BadTqValue;
        }
        switch (quant_info >> 4) {
            0 => {
                if (local_n < idct.block_size) {
                    return error.BadPqValue;
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
            else => return error.BadPqValue,
        }
    }
    if (local_n != 0) {
        return error.DqtWrongLength;
    }
}

fn processApp0Marker(self: *Decoder, n: i32) !void {
    if (n < 5) {
        return self.ignore(n);
    }
    try self.readFull(self.tmp[0..5]);

    var local_n = n;
    local_n -= 5;

    self.jfif = self.tmp[0] == 'J' and self.tmp[1] == 'F' and self.tmp[2] == 'I' and self.tmp[3] == 'F' and self.tmp[4] == '\x00';

    if (n > 0) return self.ignore(local_n);
}

fn processApp14Marker(self: *Decoder, n: i32) !void {
    if (n < 12) {
        return self.ignore(n);
    }
    try self.readFull(self.tmp[0..12]);

    var local_n = n;
    local_n -= 12;

    if (self.tmp[0] == 'A' and self.tmp[1] == 'd' and self.tmp[2] == 'o' and self.tmp[3] == 'b' and self.tmp[4] == 'e') {
        self.adobe_transform_valid = true;
        self.adobe_transform = @enumFromInt(self.tmp[11]);
    }

    if (n > 0) return self.ignore(local_n);
}

fn isRgb(self: *Decoder) bool {
    if (self.jfif) return false;

    if (self.adobe_transform_valid and self.adobe_transform == .unknown) {
        // https://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/JPEG.html#Adobe
        // says that 0 means Unknown (and in practice RGB) and 1 means YCbCr.
        return true;
    }

    return self.component[0].id == 'R' and self.component[1].id == 'G' and self.component[2].id == 'B';
}

// readByteStuffedByte is like readByte but is for byte-stuffed Huffman data.
fn readByteStuffedByte(self: *Decoder) !u8 {
    // Take the fast path if there are at least two bytes in the buffer.
    if (self.bytes.i + 2 <= self.bytes.j) {
        const x = self.bytes.buffer[self.bytes.i];
        self.bytes.i += 1;
        self.bytes.num_unreadable = 1;

        if (x != 0xff) {
            return x;
        }

        if (self.bytes.buffer[self.bytes.i] != 0x00) {
            return error.MissingFF00;
        }

        self.bytes.i += 1;
        self.bytes.num_unreadable = 2;
        return 0xff;
    }

    self.bytes.num_unreadable = 0;

    var x = try self.readByte();
    self.bytes.num_unreadable = 1;

    if (x != 0xff) {
        return x;
    }

    x = try self.readByte();
    self.bytes.num_unreadable = 2;

    if (x != 0x00) {
        return error.MissingFF00;
    }

    return 0xff;
}

pub fn convertToRGB(self: *Decoder) !image.Image {
    // Ensure self.img is a YCbCr image before proceeding
    const ycbcr_img: image.YCbCrImage = switch (self.img.?) {
        .YCbCr => |img| img,
        else => return error.InvalidImageType,
    };

    const c_scale: usize = @intCast(@divTrunc(self.component[0].h, self.component[1].h));
    const bounds = ycbcr_img.bounds();
    var img = try image.RGBAImage.init(self.al, bounds);

    var y = bounds.min.y;
    while (y < bounds.max.y) : (y += 1) {
        const po: usize = @intCast(img.pixOffset(bounds.min.x, y));
        const yo: usize = @intCast(ycbcr_img.yOffset(bounds.min.x, y));
        const co: usize = @intCast(ycbcr_img.cOffset(bounds.min.x, y));

        var i: usize = 0;
        const i_max = bounds.max.x - bounds.min.x;
        while (i < i_max) : (i += 1) {
            img.pixels[po + 4 * i + 0] = ycbcr_img.y[yo + i];
            img.pixels[po + 4 * i + 1] = ycbcr_img.cb[co + i / c_scale];
            img.pixels[po + 4 * i + 2] = ycbcr_img.cr[co + i / c_scale];
            img.pixels[po + 4 * i + 3] = 255;
        }
    }
    return .{ .RGBA = img };
}

/// applyBlack combines self.img and self.black_pixels into a CMYK image.
/// The formula used depends on whether the JPEG image is stored as CMYK or YCbCrK,
/// indicated by the APP14 (Adobe) metadata.
///
/// Adobe CMYK JPEG images are inverted, where 255 means no ink instead of full ink,
/// so we apply "v = 255 - v" at various points. Note that a double inversion is
/// a no-op, so inversions might be implicit in the code below.
pub fn applyBlack(self: *Decoder) !image.Image {
    if (!self.adobe_transform_valid) {
        return error.UnsupportedColorModel;
    }

    // If the 4-component JPEG image isn't explicitly marked as "Unknown (RGB or CMYK)"
    // we assume that it is YCbCrK. This matches libjpeg's behavior.
    if (self.adobe_transform != .unknown) {
        // Convert the YCbCr part of the YCbCrK to RGB, invert the RGB to get CMY,
        // and patch in the original K. The RGB to CMY inversion cancels out the
        // 'Adobe inversion' described above, so in practice, only the fourth channel (black) is inverted.
        const bounds = self.img.?.bounds();
        var img = try image.RGBAImage.init(self.al, bounds);

        _ = switch (self.img.?) {
            .YCbCr => |*i| try imageutil.drawYCbCr(img, bounds, i, bounds.min),
            else => unreachable,
        };

        var i_base: usize = 0;
        var y: i32 = bounds.min.y;
        while (y < bounds.max.y) {
            var i: usize = i_base + 3;
            var x: i32 = bounds.min.x;
            while (x < bounds.max.x) {
                const y_delta: usize = @intCast(y - bounds.min.y);
                const x_delta: usize = @intCast(x - bounds.min.x);

                img.pixels[i] = 255 - self.black_pixels.?[y_delta * self.black_stride + x_delta];

                x += 1;
                i += 4;
            }

            y += 1;
            i_base += img.stride;
        }

        var cmyk_img = image.CMYKImage{};
        cmyk_img.pixels = img.pixels;
        cmyk_img.stride = img.stride;
        cmyk_img.rect = img.rect;
        return .{ .CMYK = &cmyk_img };
    }

    // return error.CMYKImageNotSupported;
    // The first three channels (cyan, magenta, yellow) of the CMYK were decoded into self.img3,
    // but each channel was decoded into a separate slice, and some channels may be subsampled.
    // We interleave the separate channels into an image.CMYK's single []u8 slice containing
    // 4 contiguous bytes per pixel.
    const bounds = self.img.?.bounds();
    var img = try image.CMYKImage.init(self.al, bounds);

    const Translations = struct {
        src: []u8,
        stride: usize,
    };
    var translations: [4]Translations = undefined;

    switch (self.img.?) {
        .YCbCr => |i| {
            translations = [_]Translations{
                .{ .src = i.y, .stride = i.y_stride },
                .{ .src = i.cb, .stride = i.c_stride },
                .{ .src = i.cr, .stride = i.c_stride },
                .{ .src = self.black_pixels.?, .stride = self.black_stride },
            };
        },
        else => unreachable,
    }

    for (translations, 0..) |translation, t| {
        const subsample = self.component[t].h != self.component[0].h or self.component[t].v != self.component[0].v;

        var i_base: usize = 0;
        var y: i32 = bounds.min.y;
        while (y < bounds.max.y) {
            var sy: usize = @intCast(y - bounds.min.y);
            if (subsample) {
                sy >>= 1;
            }

            var i: usize = i_base + t;
            var x: i32 = bounds.min.x;
            while (x < bounds.max.x) {
                var sx: usize = @intCast(x - bounds.min.x);
                if (subsample) {
                    sx >>= 1;
                }
                img.pixels[i] = 255 - translation.src[sy * translation.stride + sx];

                i += 4;
                x += 1;
            }

            y += 1;
            i_base += img.stride;
        }
    }

    return .{ .CMYK = img };
}

//===================================//
// # Huffman Processing
//===================================//
// decodeHuffman returns the next Huffman-coded value from the bit-stream,
// decoded according to h.
fn decodeHuffman(self: *Decoder, huff_table: *HuffTable) !u8 {
    if (huff_table.num_codes == 0) {
        return error.UninitializedHuffmanTable;
    }

    // Fast path: Check if enough bits are available.
    const goto_slow_path = if (self.bits.n < 8) blk: {
        self.ensureNBits(8) catch |err| {
            if (err != error.MissingFF00 and err != error.ShortHuffmanData) {
                return err;
            }
            // There are no more bytes of data in this segment, but we may still
            // be able to read the next symbol out of the previously read bits.
            // First, undo the readByte that the ensureNBits call made.
            if (self.bytes.num_unreadable != 0) {
                self.unreadByteStuffedByte();
            }
            break :blk true;
        };
        break :blk false;
    } else false;

    if (!goto_slow_path) {
        // This fast path tries to decode the Huffman code with a single lookup from the table.
        const lookupValue = huff_table.lut[(self.bits.a >> @intCast(self.bits.n - HuffTable.lut_size)) & 0xff];
        if (lookupValue != 0) {
            // The lower byte encodes how many bits to consume (minus one).
            const bitCount = @as(i32, @intCast(lookupValue & 0xff)) - 1;
            // Reduce the available bits and shift the bit mask to align with the next code boundary.
            self.bits.n -= @intCast(bitCount);
            self.bits.m >>= @intCast(bitCount);
            // The upper byte holds the symbol; return it as our decoded value.
            return @as(u8, @intCast(lookupValue >> 8));
        }
    }

    // Slow path: Bit-by-bit decoding.
    var code: i32 = 0;
    for (0..HuffTable.max_code_length) |i| {
        // Ensure at least one bit is loaded.
        if (self.bits.n == 0) {
            try self.ensureNBits(1);
        }

        // Check the top bit. If set, increment 'code' by 1.
        if ((self.bits.a & self.bits.m) != 0) {
            code |= 1;
        }

        // Move to the next bit position.
        self.bits.n -= 1;
        self.bits.m >>= 1;

        // If 'code' is within range for this code length, return the symbol.
        if (code <= huff_table.max_codes[i]) {
            return huff_table.vals[@as(usize, @intCast(huff_table.vals_indices[i] + code - huff_table.min_codes[i]))];
        }

        // Shift 'code' to match the next code length.
        code <<= 1;
    } else return error.BadHuffmanCode;
}

// ensureNBits reads bytes from the byte buffer to ensure that self.bits.n is at
// least n. For best performance (avoiding function calls inside hot loops),
// the caller is the one responsible for first checking that self.bits.n < n.
fn ensureNBits(self: *Decoder, n: i32) !void {
    while (true) {
        const c = try self.readByteStuffedByte();
        self.bits.a = (self.bits.a << 8) | @as(u32, c);
        self.bits.n += 8;

        if (self.bits.m == 0) {
            self.bits.m = 1 << 7;
        } else {
            self.bits.m <<= 8;
        }

        if (self.bits.n >= n) {
            break;
        }
    }
}

// decodeBit reads a single bit from the bit-stream.
fn decodeBit(self: *Decoder) !bool {
    if (self.bits.n == 0) {
        try self.ensureNBits(1);
    }
    // Determine if the current bit is set by masking the shift register.
    // This keeps the decoding logic straightforward by revealing only a single bit at a time.
    const ret = self.bits.a & self.bits.m != 0;

    // Consume one bit, ensuring future reads focus on the next available bit.
    self.bits.n -= 1;
    self.bits.m >>= 1;
    return ret;
}

// decodeBits reads n bits from the bit-stream.
fn decodeBits(self: *Decoder, n: i32) !u32 {
    if (self.bits.n < n) {
        try self.ensureNBits(n);
    }
    // Shifts the accumulated bits to target only the earliest unread portion
    var ret: u32 = self.bits.a >> @as(u5, @intCast(self.bits.n - n));
    // Masks out just the relevant bits for the final extracted value
    ret &= (@as(u32, 1) << @as(u5, @intCast(n))) - 1;
    // Reduces the available bit count
    self.bits.n -= n;
    // Shifts the bit mask so the next read aligns correctly
    self.bits.m >>= @as(u5, @intCast(n));
    return ret;
}

// processDHT processes a Define Huffman Table marker, and initializes a huffman
// struct from its contents. Specified in section B.2.4.2.
fn processDht(self: *Decoder, n: i32) !void {
    // make a mutable copy of n
    var local_n = n;

    while (local_n > 0) {
        if (local_n < HuffTable.max_code_length + 1) {
            return error.DhtWrongLength;
        }
        try self.readFull(self.tmp[0 .. HuffTable.max_code_length + 1]);
        const tc = self.tmp[0] >> 4;
        if (tc > max_tc) {
            return error.BadTcValue;
        }
        const th = self.tmp[0] & 0x0f;
        // The baseline th <= 1 restriction is specified in table B.5.
        if (th > max_th or (self.baseline and th > 1)) {
            return error.BadThValue;
        }
        const huff_table = &self.huff[tc][th];

        // Read num_codes and h.vals (and derive h.num_codes).
        // num_codes[i] is the number of codes with code length i.
        // h.num_codes is the total number of codes.
        huff_table.num_codes = 0;
        var num_codes = try self.al.alloc(i32, HuffTable.max_code_length);
        defer self.al.free(num_codes);

        for (0..num_codes.len) |i| {
            num_codes[i] = self.tmp[i + 1];
            huff_table.num_codes += num_codes[i];
        }
        if (huff_table.num_codes == 0) {
            return error.HuffZeroLength;
        }
        if (huff_table.num_codes > HuffTable.max_num_codes) {
            return error.HuffTooLong;
        }
        local_n -= huff_table.num_codes + HuffTable.max_code_length + 1;
        if (local_n < 0) {
            return error.DhtWrongLength;
        }
        const huff_codes_len: usize = @intCast(huff_table.num_codes);
        try self.readFull(huff_table.vals[0..huff_codes_len]);

        // Derive the look-up table.
        huff_table.clearLut();
        var code: u32 = 0;
        var val_index: usize = 0;
        for (0..HuffTable.lut_size) |i| {
            code <<= 1;
            var j: usize = 0;
            while (j < num_codes[i]) : (j += 1) {
                // The codeLength is 1+i, so shift code by 8-(1+i) to
                // calculate the high bits for every 8-bit sequence
                // whose codeLength's high bits matches code.
                // The high 8 bits of lut_value are the encoded value.
                // The low 8 bits are 1 plus the codeLength.
                const base: u32 = code << @intCast(7 - i);
                const lut_value: u16 = @as(u16, huff_table.vals[val_index]) << @intCast(8) | @as(u16, @intCast(2 + i));
                for (0..@as(u16, 1) << @intCast(7 - i)) |k| {
                    huff_table.lut[base | k] = lut_value;
                }
                code += 1;
                val_index += 1;
            }
        }

        // Derive min_codes, max_codes, and vals_indices.
        var code_base: i32 = 0;
        var index: i32 = 0;
        for (num_codes, 0..) |num_code, i| {
            if (num_code == 0) {
                huff_table.min_codes[i] = -1;
                huff_table.max_codes[i] = -1;
                huff_table.vals_indices[i] = -1;
            } else {
                huff_table.min_codes[i] = code_base;
                huff_table.max_codes[i] = code_base + num_code - 1;
                huff_table.vals_indices[i] = index;
                code_base += num_code;
                index += num_code;
            }
            code_base <<= 1;
        }
    }
}

// receiveExtend is the composition of RECEIVE and EXTEND, specified in section
// F.2.2.1.
fn receiveExtend(self: *Decoder, bit_count: u8) !i32 {
    if (self.bits.n < @as(i32, bit_count)) {
        try self.ensureNBits(@as(i32, bit_count));
    }

    // Adjust bit count and shift
    self.bits.n -= @as(i32, bit_count);
    self.bits.m >>= @intCast(bit_count);

    // Perform RECEIVE step
    const threshold = @as(i32, 1) << @intCast(bit_count);
    var value: i32 = @intCast((self.bits.a >> @intCast(self.bits.n)) & @as(u32, @intCast(threshold - 1)));

    // Perform EXTEND step
    if (value < (threshold >> 1)) {
        value += ((@as(i32, -1) << @intCast(bit_count)) + 1);
    }

    return value;
}

//===================================//
// # End Huffman Processing
//===================================//
// ********************************* //
// ********************************* //
//===================================//
// # Scan Processing
//===================================//
// processSos decodes the pixel data. The scan portion of decoding is the most complex part. It's responsible for
// reading the image data, decoding the Huffman-encoded data, and performing the
// inverse DCT to reconstruct the image data.
// n is the number of bytes in the scan segment.
fn processSos(self: *Decoder, n: i32) !void {
    if (self.num_components == 0) {
        return error.MissingSosMarker;
    }

    if (n < 6 or 4 + 2 * self.num_components < n or @mod(n, 2) != 0) {
        std.log.err("n: {d}, num_comp: {d}", .{ n, self.num_components });
        return error.SosWrongLength;
    }

    try self.readFull(self.tmp[0..@intCast(n)]);

    // top of buffer is the number of components in the scan
    const n_comp = self.tmp[0];

    if (n != 4 + 2 * n_comp) {
        return error.SosWrongLength;
    }

    const ScanComponent = struct {
        id: u8 = 0,
        td: u8 = 0, // DC table selector.
        ta: u8 = 0, // AC table selector.
    };
    var scan = [_]ScanComponent{.{}} ** max_components;
    // defer self.al.free(scan);

    // Accumulating horizontal and vertical sampling factors
    var total_hv_sampling_factors: i32 = 0;
    for (0..n_comp) |i| {
        const component_selector = self.tmp[1 + 2 * i];
        var component_index: ?usize = null;
        for (self.component[0..self.num_components], 0..) |comp, j| {
            if (component_selector == comp.id) {
                component_index = j;
                break;
            }
        }
        if (component_index == null) {
            return error.UnknownComponentSelector;
        }
        scan[i].id = @intCast(component_index.?);
        // Section B.2.3 states that "the value of Cs_j shall be different from
        // the values of Cs_1 through Cs_(j-1)". Since we have previously
        // verified that a frame's component identifiers (C_i values in section
        // B.2.2) are unique, it suffices to check that the implicit indexes
        // into self.comp are unique.
        for (0..i) |j| {
            if (scan[i].id == scan[j].id) {
                return error.RepeatedComponentIdentifier;
            }
        }
        total_hv_sampling_factors += self.component[component_index.?].h * self.component[component_index.?].v;
        // The baseline t <= 1 restriction is specified in table B.3.
        scan[i].td = self.tmp[2 + 2 * i] >> 4;
        var table_elector = scan[i].td;
        if (table_elector > max_th or (self.baseline and table_elector > 1)) {
            return error.BadTdValue;
        }
        scan[i].ta = self.tmp[2 + 2 * i] & 0x0f;
        table_elector = scan[i].ta;
        if (table_elector > max_th or (self.baseline and table_elector > 1)) {
            return error.BadTaValue;
        }
    }

    // Section B.2.3 states that if there is more than one component then the
    // total H*V values in a scan must be <= 10.
    if (self.num_components > 1 and total_hv_sampling_factors > 10) {
        return error.SamplingFactorsTooLarge;
    }

    // zig_start and zig_end are the spectral selection bounds.
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
            return error.BadSpectralSelection;
        }
        if (zig_start != 0 and n_comp != 1) {
            return error.ProgressiveACCoefficientsForMoreThanOneComponent;
        }
        if (ah != 0 and ah != al + 1) {
            return error.BadSuccessiveApproximation;
        }
    }

    // mxx and myy are the number of MCUs (Minimum Coded Units) in the image.
    const h0 = self.component[0].h;
    const v0 = self.component[0].v;
    const w: i32 = @intCast(self.width);
    const h: i32 = @intCast(self.height);
    const mxx = @divTrunc(w + 8 * h0 - 1, 8 * h0);
    const myy = @divTrunc(h + 8 * v0 - 1, 8 * v0);
    if (self.img == null) {
        try self.makeImg(mxx, myy);
    }

    if (self.progressive) {
        var i: usize = 0;
        while (i < self.num_components) : (i += 1) {
            const component_index = scan[i].id;
            if (self.progressive_coefficients[component_index] == null) {
                const prog_block_size: usize = @intCast(mxx * myy * self.component[component_index].h * self.component[component_index].v);
                self.progressive_coefficients[component_index] = try self.al.alloc(
                    idct.Block,
                    prog_block_size,
                );
                for (0..prog_block_size) |j| {
                    self.progressive_coefficients[component_index].?[j] = idct.emptyBlock();
                }
            }
        }
    }

    self.bits = Bits{};
    var mcu: i32 = 0;
    var expected_rst = Marker.rst0;
    var bx: i32 = 0;
    var by: i32 = 0;
    var block_count: i32 = 0;
    var dc: [max_components]i32 = [_]i32{0} ** max_components;
    var b: idct.Block = undefined;

    for (0..@intCast(myy)) |my| {
        for (0..@intCast(mxx)) |mx| {
            for (0..n_comp) |k| {
                const c_index = scan[k].id;
                const hi = self.component[c_index].h;
                const vi = self.component[c_index].v;

                for (0..@intCast(hi * vi)) |j| {
                    // The blocks are traversed one MCU at a time. For 4:2:0 chroma
                    // subsampling, there are four Y 8x8 blocks in every 16x16 MCU.
                    //
                    // For a sequential 32x16 pixel image, the Y blocks visiting order is:
                    //	0 1 4 5
                    //	2 3 6 7
                    //
                    // For progressive images, the interleaved scans (those with nComp > 1)
                    // are traversed as above, but non-interleaved scans are traversed left
                    // to right, top to bottom:
                    //	0 1 2 3
                    //	4 5 6 7
                    // Only DC scans (zigStart == 0) can be interleaved. AC scans must have
                    // only one component.
                    //
                    // To further complicate matters, for non-interleaved scans, there is no
                    // data for any blocks that are inside the image at the MCU level but
                    // outside the image at the pixel level. For example, a 24x16 pixel 4:2:0
                    // progressive image consists of two 16x16 MCUs. The interleaved scans
                    // will process 8 Y blocks:
                    //	0 1 4 5
                    //	2 3 6 7
                    // The non-interleaved scans will process only 6 Y blocks:
                    //	0 1 2
                    //	3 4 5
                    if (n_comp != 1) {
                        bx = hi * @as(i32, @intCast(mx)) + @mod(@as(i32, @intCast(j)), hi);
                        by = vi * @as(i32, @intCast(my)) + @divTrunc(@as(i32, @intCast(j)), hi);
                    } else {
                        bx = @mod(block_count, mxx * hi);
                        by = @divTrunc(block_count, mxx * hi);
                        block_count += 1;
                        if (bx * 8 >= self.width or by * 8 >= self.height) {
                            continue;
                        }
                    }

                    // Load the previous partially decoded coefficients, if applicable.
                    if (self.progressive) {
                        const block_index: usize = @intCast(by * mxx * hi + bx);
                        b = self.progressive_coefficients[c_index].?[block_index];
                    } else {
                        b = idct.emptyBlock();
                    }

                    // Reconstruct each 8x8 block’s DC/AC coefficients.
                    // It references a standard zigzag array (unzig) to place coefficients in the correct
                    // order for the subsequent dequantization and IDCT steps. By reordering from low to
                    // high frequency, it supports more efficient skipping of zero blocks and ensures the
                    // final image is accurately reconstructed.
                    //
                    // Refine existing DC bits instead of decoding new ones.
                    if (ah != 0) {
                        try self.refine(
                            &b,
                            &self.huff[ac_table][scan[k].ta],
                            zig_start,
                            zig_end,
                            @as(i32, 1) << @intCast(al),
                        );
                    } else {
                        var zig: i32 = zig_start;

                        // Decode DC coefficient for the first slot.
                        if (zig == 0) {
                            zig += 1;
                            // Decode the DC coefficient, as specified in section F.2.2.1.
                            const value = try self.decodeHuffman(&self.huff[dc_table][scan[k].td]);
                            if (value > 16) {
                                return error.ExcessiveDCComponent;
                            }
                            const dc_delta = try self.receiveExtend(value);
                            dc[c_index] += dc_delta;
                            b[0] = dc[c_index] << @intCast(al);
                        }

                        // Use remaining EOB run if available, else decode AC.
                        if (zig <= zig_end and self.eob_run > 0) {
                            self.eob_run -= 1;
                        } else {
                            // Decode the AC coefficients, as specified in section F.2.2.2.
                            const huff = &self.huff[ac_table][scan[k].ta];

                            // Decode AC coefficients until EOB or end of range.
                            while (zig <= zig_end) : (zig += 1) {
                                const value = try self.decodeHuffman(huff);
                                const val0 = value >> 4;
                                const val1 = value & 0x0f;

                                if (val1 != 0) {
                                    zig += @intCast(val0);
                                    if (zig > zig_end) {
                                        break;
                                    }
                                    const ac = try self.receiveExtend(val1);
                                    b[unzig[@intCast(zig)]] = ac << @intCast(al);
                                } else {
                                    if (val0 != 0x0f) {
                                        self.eob_run = @as(u16, 1) << @intCast(val0);
                                        if (val0 != 0) {
                                            const bits = try self.decodeBits(@intCast(val0));
                                            self.eob_run |= @intCast(bits);
                                        }
                                        self.eob_run -= 1;
                                        break;
                                    }
                                    zig += 0x0f;
                                }
                            }
                        }
                    }

                    if (self.progressive) {
                        const block_index: usize = @intCast(by * mxx * hi + bx);
                        self.progressive_coefficients[c_index].?[block_index] = b;
                        // At this point, we could call reconstructBlock to dequantize and perform the
                        // inverse DCT, to save early stages of a progressive image to the *image.YCbCr
                        // buffers (the whole point of progressive encoding), but in Go, the jpeg.Decode
                        // function does not return until the entire image is decoded, so we "continue"
                        // here to avoid wasted computation. Instead, reconstructBlock is called on each
                        // accumulated block by the reconstructProgressiveImage method after all of the
                        // SOS markers are processed.
                        continue;
                    }
                    try self.reconstructBlock(&b, bx, by, @intCast(c_index));
                }
            }

            mcu += 1;

            if (self.restart_interval > 0 and @mod(mcu, self.restart_interval) == 0 and mcu < mxx * myy) {
                // For well-formed input, the RST[0-7] restart marker follows
                // immediately. For corrupt input, call findRST to try to
                // resynchronize.
                try self.readFull(self.tmp[0..2]);
                if (self.tmp[0] != 0xff or self.tmp[1] != @intFromEnum(expected_rst)) {
                    try self.findRst(@intFromEnum(expected_rst));
                }
                expected_rst = @enumFromInt(@intFromEnum(expected_rst) + 1);
                const max_rst: Marker = @enumFromInt(@intFromEnum(Marker.rst7) + 1);
                // wrap around
                if (expected_rst == max_rst) {
                    expected_rst = Marker.rst0;
                }
                // Reset the Huffman decoder.
                self.bits = Bits{};
                // Reset the DC components, as per section F.2.1.3.1.
                dc = [_]i32{0} ** max_components;
                // Reset the progressive decoder state, as per section G.1.2.2.
                self.eob_run = 0;
            }
        }
    }
}

// refine decodes a successive approximation refinement block, as specified in
// section G.1.2.
fn refine(self: *Decoder, b: *idct.Block, h: *HuffTable, zig_start: i32, zig_end: i32, delta: i32) !void {
    // Refining a DC component is trivial.
    if (zig_start == 0) {
        if (zig_end != 0) unreachable;

        const bit = try self.decodeBit();
        if (bit) {
            b[0] |= delta;
        }
        return;
    }

    // Refining AC components is more complicated; see sections G.1.2.2 and G.1.2.3.
    var zig: i32 = zig_start;

    if (self.eob_run == 0) {
        loop: while (zig <= zig_end) : (zig += 1) {
            var z: i32 = 0;
            const value = try self.decodeHuffman(h);
            const val0 = value >> 4;
            const val1 = value & 0x0F;

            switch (val1) {
                0 => {
                    if (val0 != 0x0F) {
                        self.eob_run = @as(u16, 1) << @intCast(val0);
                        if (val0 != 0) {
                            const bits = try self.decodeBits(@intCast(val0));
                            self.eob_run |= @as(u16, @intCast(bits));
                        }
                        break :loop;
                    }
                },
                1 => {
                    z = delta;
                    const bit = try self.decodeBit();
                    if (!bit) {
                        z = -z;
                    }
                },
                else => {
                    return error.UnexpectedHuffmanCode;
                },
            }

            zig = try self.refineNonZeroes(b, zig, zig_end, @intCast(val0), delta);
            if (zig > zig_end) {
                return error.TooManyCoefficients;
            }
            if (z != 0) {
                b[unzig[@intCast(zig)]] = z;
            }
        }
    }

    if (self.eob_run > 0) {
        self.eob_run -= 1;
        _ = try self.refineNonZeroes(b, zig, zig_end, -1, delta);
    }
}

// refineNonZeroes refines non-zero entries of b in zig-zag order. If nz >= 0,
// the first nz zero entries are skipped over.
fn refineNonZeroes(self: *Decoder, b: *idct.Block, zig: i32, zig_end: i32, nz: i32, delta: i32) !i32 {
    // make mutable copies
    var local_nz = nz;
    var local_zig = zig;

    while (local_zig <= zig_end) : (local_zig += 1) {
        const index = unzig[@intCast(local_zig)];
        if (b[index] == 0) {
            if (local_nz == 0) {
                break;
            }
            local_nz -= 1;
            continue;
        }

        const bit = try self.decodeBit();
        if (!bit) {
            continue;
        }

        if (b[index] >= 0) {
            b[index] += delta;
        } else {
            b[index] -= delta;
        }
    }
    return local_zig;
}

// reconstructBlock dequantizes, performs the inverse DCT and stores the block
// to the image.
pub fn reconstructBlock(
    self: *Decoder,
    b: *idct.Block,
    block_x: i32,
    block_y: i32,
    component_index: usize,
) !void {
    const bx: usize = @intCast(block_x);
    const by: usize = @intCast(block_y);

    // Step 1: Dequantize the block.
    const qt = &self.quant[self.component[component_index].tq];
    for (0..idct.block_size) |zig| {
        b[unzig[zig]] *= qt[zig];
    }

    // Step 2: Perform the Inverse Discrete Cosine Transform (IDCT).
    idct.transform(b);

    // Step 3: Map the dequantized block to the destination buffer.
    var dest_pixels: []u8 = undefined;
    var stride: usize = 0;
    if (self.num_components == 1) {
        // Single-component (Grayscale)
        switch (self.img.?) {
            .Gray => |gray_img| {
                dest_pixels = gray_img.pixels[8 * (by * gray_img.stride + bx) ..];
                stride = gray_img.stride;
            },
            else => {
                std.debug.panic("Expected Gray image for single-component JPEG", .{});
            },
        }
    } else {
        // Multi-component (YCbCr or additional black channel)
        switch (self.img.?) {
            .YCbCr => |ycbcr_img| {
                switch (component_index) {
                    0 => {
                        dest_pixels = ycbcr_img.y[8 * (by * ycbcr_img.y_stride + bx) ..];
                        stride = ycbcr_img.y_stride;
                    },
                    1 => {
                        dest_pixels = ycbcr_img.cb[8 * (by * ycbcr_img.c_stride + bx) ..];
                        stride = ycbcr_img.c_stride;
                    },
                    2 => {
                        dest_pixels = ycbcr_img.cr[8 * (by * ycbcr_img.c_stride + bx) ..];
                        stride = ycbcr_img.c_stride;
                    },
                    3 => {
                        dest_pixels = self.black_pixels.?[8 * (by * self.black_stride + bx) ..];
                        stride = self.black_stride;
                    },
                    else => return error.UnsupportedComponent,
                }
            },
            else => {
                std.debug.panic("Expected YCbCr image for multi-component JPEG", .{});
            },
        }
    }

    // Step 4: Level shift by +128, clip to [0, 255], and write to destination pixels.
    for (0..8) |y| {
        // Row offset in the block.
        const y8 = y * 8;
        // Row offset in the image buffer.
        const y_stride = y * stride;

        for (0..8) |x| {
            // Get coefficient from the block.
            var coefficient: i32 = b[y8 + x];

            // Level shift and clipping.
            if (coefficient < -128) {
                coefficient = 0;
            } else if (coefficient > 127) {
                coefficient = 255;
            } else {
                coefficient += 128;
            }

            // Write clipped value to the buffer.
            dest_pixels[y_stride + x] = @as(u8, @intCast(coefficient));
        }
    }
}

pub fn reconstructProgressiveImage(self: *Decoder) !void {
    // The h0, mxx, by and bx variables have the same meaning as in the
    // processSos method.
    const h0 = self.component[0].h;
    const mxx = @divTrunc(@as(i32, @intCast(self.width)) + 8 * h0 - 1, 8 * h0);

    var i: usize = 0;
    while (i < self.num_components) : (i += 1) {
        if (self.progressive_coefficients[i] == null) continue;
        const v: usize = @intCast(8 * @divTrunc(self.component[0].v, self.component[i].v));
        const h: usize = @intCast(8 * @divTrunc(self.component[0].h, self.component[i].h));
        const stride: usize = @intCast(mxx * self.component[i].h);
        var by: usize = 0;
        while (by * v < self.height) : (by += 1) {
            var bx: usize = 0;
            while (bx * h < self.width) : (bx += 1) {
                try self.reconstructBlock(
                    &self.progressive_coefficients[i].?[by * stride + bx],
                    @intCast(bx),
                    @intCast(by),
                    i,
                );
            }
        }
    }
}
// findRST advances past the next RST restart marker that matches expected_rst.
// Other than I/O errors, it is also an error if we encounter an {0xFF, M}
// two-byte marker sequence where M is not 0x00, 0xFF or the expected_rst.
//
// This is similar to libjpeg's jdmarker.c's next_marker function.
// https://github.com/libjpeg-turbo/libjpeg-turbo/blob/2dfe6c0fe9e18671105e94f7cbf044d4a1d157e6/jdmarker.c#L892-L935
//
// Precondition: self.tmp[:2] holds the next two bytes of JPEG-encoded input
// (input in the self.readFull sense).
pub fn findRst(self: *Decoder, expected_rst: u8) !void {
    while (true) {
        // i is the index such that, at the bottom of the loop, we read 2-i
        // bytes into d.tmp[i:2], maintaining the invariant that self.tmp[:2]
        // holds the next two bytes of JPEG-encoded input. It is either 0 or 1,
        // so that each iteration advances by 1 or 2 bytes (or returns).
        var i: usize = 0;

        if (self.tmp[0] == 0xFF) {
            if (self.tmp[1] == expected_rst) {
                // Found the expected RST marker, return successfully.
                return;
            } else if (self.tmp[1] == 0xFF) {
                // Encountered another `0xFF`, skip one byte and continue.
                i = 1;
            } else if (self.tmp[1] != 0x00) {
                // libjpeg's jdmarker.c's jpeg_resync_to_restart does something
                // fancy here, treating RST markers within two (modulo 8) of
                // expected_rst differently from RST markers that are 'more
                // distant'. Until we see evidence that recovering from such
                // cases is frequent enough to be worth the complexity, we take
                // a simpler approach for now. Any marker that's not 0x00, 0xff
                // or expected_rst is an error.
                return error.BadRSTMarker;
            }
        } else if (self.tmp[1] == 0xFF) {
            // Shift the second byte to the first position and read a new second byte.
            self.tmp[0] = 0xFF;
            i = 1;
        }

        // Read the next byte(s) into `self.tmp[i..2]`, ensuring the invariant holds.
        try self.readFull(self.tmp[i..2]);
    }
}

// makeImg allocates and initializes the destination image.
fn makeImg(self: *Decoder, mxx: i32, myy: i32) !void {
    if (self.num_components == 1) {
        // Allocate a grayscale image if there's only one component.
        const gray_image: *image.GrayImage = try image.GrayImage.init(self.al, image.Rectangle.init(
            0,
            0,
            8 * mxx,
            8 * myy,
        ));
        // Create a sub-image with the exact dimensions of the JPEG.
        self.img = image.Image{ .Gray = try gray_image.subImage(self.al, image.Rectangle.init(
            0,
            0,
            @intCast(self.width),
            @intCast(self.height),
        )) orelse return error.CreateImageFailed };
        return;
    }

    // Calculate the subsampling ratio.
    // The subsampling ratio is the ratio between the dimensions of the Y component
    // and the dimensions of the Cb and Cr components.
    const h0 = self.component[0].h;
    const v0 = self.component[0].v;
    const h_ratio = @divExact(h0, self.component[1].h);
    const v_ratio = @divExact(v0, self.component[1].v);
    const subsample_ratio: image.YCbCrSubsample = switch (h_ratio << 4 | v_ratio) {
        0x11 => .Ratio444, // No subsampling.
        0x12 => .Ratio440, // Vertical subsampling.
        0x21 => .Ratio422, // Horizontal subsampling.
        0x22 => .Ratio420, // Both horizontal and vertical subsampling.
        0x41 => .Ratio411, // More horizontal subsampling.
        0x42 => .Ratio410, // More vertical subsampling.
        else => unreachable,
    };
    // Allocate a YCbCr image with the calculated subsampling ratio.
    var img: image.YCbCrImage = try image.YCbCrImage.init(self.al, image.Rectangle.init(
        0,
        0,
        8 * h0 * mxx,
        8 * v0 * myy,
    ), subsample_ratio);

    // Create a sub-image with the exact dimensions of the JPEG.
    self.img = image.Image{ .YCbCr = try img.subImage(image.Rectangle.init(
        0,
        0,
        @intCast(self.width),
        @intCast(self.height),
    )) orelse return error.CreateImageFailed };

    if (self.num_components == 4) {
        // Allocate space for the black channel if there are four components (CMYK).
        const h3 = self.component[3].h;
        const v3 = self.component[3].v;
        self.black_pixels = try self.al.alloc(u8, @intCast(8 * h3 * mxx * 8 * v3 * myy));
        self.black_stride = @intCast(8 * h3 * mxx);
    }
}
//===================================//
// # End Scan Processing
//===================================//
