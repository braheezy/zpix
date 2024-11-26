const std = @import("std");

const idct = @import("idct.zig");
const HuffTable = @import("HuffTable.zig");

const dbg = std.debug.print;

fn dbgln(comptime fmt: []const u8) void {
    std.debug.print("{s}\n", .{fmt});
}

const max_components = 4;
const maxTc = 1;
const maxTh = 3;
const maxTq = 3;

pub const FormatError = error{
    /// SOI marker missing or incorrect
    InvalidSOIMarker,
    /// EOI marker missing or incorrect
    InvalidEOIMarker,
    /// Unexpected end of data in stream
    UnexpectedEndOfData,
    /// Expected marker but failed to read it
    InvalidMarker,
    InvalidHuffmanCode,
    NotEnoughBits,
    BlockOverflow,
    EndOfEntropyData,
    UnexpectedEof,
    UnsupportedMarker,
    UnknownMarker,
    ShortSegmentLength,
    /// General format issue
    InvalidJPEGFormat,
};

// [Table B.1] Marker code assignments
pub const Marker = enum(u16) {
    // Start Of Frame (Baseline Sequential).
    SOF0 = 0xC0,
    // Start Of Frame (Extended Sequential).
    SOF1 = 0xC1,
    // Start Of Frame (Progressive).
    SOF2 = 0xC2,
    // Define Huffman Table.
    DHT = 0xC4,
    // ReSTart (0).
    RST0 = 0xD0,
    // ReSTart (7).
    RST7 = 0xD7,
    // Start Of Image.
    SOI = 0xD8,
    // End Of Image.
    EOI = 0xD9,
    // Start Of Scan.
    SOS = 0xDA,
    // Define Quantization Table.
    DQT = 0xDB,
    // Define Restart Interval.
    DRI = 0xDD,
    // COMment
    COM = 0xFE,
    // "APPlication specific" markers aren't part of the JPEG spec per se,
    // but in practice, their use is described at
    // https://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/JPEG.html
    APP0 = 0xE0,
    APP14 = 0xEE,
    APP15 = 0xEF,
    _,
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

// As per section 4.5, there are four modes of operation (selected by the
// SOF? markers): sequential DCT, progressive DCT, lossless and
// hierarchical, although this implementation does not support the latter
// two non-DCT modes. Sequential DCT is further split into baseline and
// extended, as per section 4.11.
baseline: bool = false,
progressive: bool = false,
jfif: bool = false,
adobe_transform_valid: bool = false,
adobe_transform: u8 = 0,
// End-of-Band run, specified in section G.1.2.2.
eob_run: u16 = 0,

comp: [max_components]Component,
huff: [maxTc + 1][maxTh + 1]HuffTable,
// Quantization table, in zig-zag order.
quant: [maxTq + 1]idct.Block,
tmp: [2 * idct.block_size]u8 = [_]u8{0} ** (2 * idct.block_size),

// TODO decode reads a JPEG image from r and returns it as an
pub fn decode(al: std.mem.Allocator, r: std.io.AnyReader) !void {
    _ = al;
    var d = Decoder{
        .r = r,
        .bytes = .{ .buffer = [_]u8{0} ** 4096 },
        .comp = [_]Component{.{}} ** max_components,
        .huff = undefined,
        .quant = undefined,
        .bits = undefined,
    };
    try d._decode(false);
}

pub fn decodeConfig(r: std.io.AnyReader) !void {
    const d = Decoder{};
    d._decode(r, true);
}

// TODO _decode reads a JPEG image from r and returns it as an ??
fn _decode(self: *Decoder, config_only: bool) !void {
    _ = config_only;

    try self.readFull(self.tmp[0..2]);
    // Check for the Start of Image marker.
    if (self.tmp[0] != 0xFF or self.tmp[1] != @intFromEnum(Marker.SOI)) {
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
        if (marker == @intFromEnum(Marker.EOI)) {
            // Done!
            break;
        }
        if (@intFromEnum(Marker.RST0) <= marker and marker <= @intFromEnum(Marker.RST7)) {
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
            Marker.SOF0 => {
                dbgln("found sof0");
            },
            else => {
                if (@intFromEnum(Marker.APP0) <= marker and marker <= @intFromEnum(Marker.APP15) or marker == @intFromEnum(Marker.COM)) {
                    try self.ignore(n);
                } else if (marker < 0xc0) {
                    // See Table B.1 "Marker code assignments".
                    return FormatError.UnknownMarker;
                } else {
                    dbg("unsupported marked: {x}\n", .{marker});
                    return FormatError.UnsupportedMarker;
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
