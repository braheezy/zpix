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

pub const FileError = error{
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
    /// General format issue
    InvalidJPEGFormat,
};

// [Table B.1] Marker code assignments
pub const Marker = enum(u16) {
    SOF0 = 0xC0,
    SOF1 = 0xC1,
    SOF2 = 0xC2,
    DHT = 0xC4,
    SOI = 0xD8,
    SOS = 0xDA,
    EOI = 0xD9,
    DQT = 0xDB,
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
        return FileError.InvalidSOIMarker;
    }

    while (true) {
        try self.readFull(self.tmp[0..2]);
    }
}

// readFull reads exactly len(p) bytes into p. It does not care about byte
// stuffing.
fn readFull(self: *Decoder, p: []u8) !void {
    var local_p = p; // Create a mutable local copy of p

    // Unread the overshot bytes, if any.
    if (self.bytes.num_unreadable > 0) {
        if (self.bits.n >= 8) {
            self.unreadByteStuffedByte();
        }
        self.bytes.num_unreadable = 0;
    }

    while (local_p.len > 0) {
        if (self.bytes.i == self.bytes.j) {
            // Buffer is empty; fill it with more data.
            try self.fill();
        }

        const available = self.bytes.j - self.bytes.i;
        const n = @min(local_p.len, available);

        // Zig idiom: Copy data manually when sizes are small or mismatched.
        for (0..n) |i| {
            local_p[i] = self.bytes.buffer[self.bytes.i + i];
        }

        // Update indices and slice pointers.
        self.bytes.i += n;
        local_p = local_p[n..]; // Advance local_p to point to the remaining space.
    }
}

// fill fills up the d.bytes.buf buffer from the underlying io.Reader. It
// should only be called when there are no unread bytes in d.bytes.
fn fill(self: *Decoder) !void {
    if (self.bytes.i != self.bytes.j) {
        dbg("{d} {d}\n", .{ self.bytes.i, self.bytes.j });
        @panic("jpeg: fill called when unread bytes exist");
    }

    // Move the last 2 bytes to the start of the buffer, in case we need
    // to call unreadByteStuffedByte.
    if (self.bytes.j > 2) {
        self.bytes.buffer[0] = self.bytes.buffer[self.bytes.j - 2];
        self.bytes.buffer[1] = self.bytes.buffer[self.bytes.j - 1];
        self.bytes.i = 2;
        self.bytes.j = 2;
    }

    // Fill in the rest of the buffer.
    const n = try self.r.read(self.bytes.buffer[self.bytes.j..]);
    self.bytes.j += n;

    if (n > 0) {
        return; // Successfully read bytes, return without error
    }

    // Handle EOF as an unexpected error
    return error.UnexpectedEOF;
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
