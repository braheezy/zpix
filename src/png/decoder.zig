const std = @import("std");
const image = @import("image");

const png_header = "\x89PNG\r\n\x1a\n";

// Constants for PNG chunk structure
const chunk_header_size = 8; // 4 bytes length + 4 bytes type
const chunk_crc_size = 4;
const chunk_base_size = chunk_header_size + chunk_crc_size;

// Decoding stage.
// The PNG specification says that the IHDR, PLTE (if present), tRNS (if
// present), IDAT and IEND chunks must appear in that order. There may be
// multiple IDAT chunks, and IDAT chunks must be sequential (i.e. they may not
// have any other chunks between them).
// https://www.w3.org/TR/PNG/#5ChunkOrdering
const Stage = enum(u8) {
    start,
    seen_ihdr,
    seen_plte,
    seen_trns,
    seen_idat,
    seen_iend,
};

const DecoderFlags = packed struct {
    read_ihdr: bool = false,
    read_cgbi: bool = false,
    read_actl: bool = false,
    read_plte: bool = false,
    read_trns: bool = false,
    read_fctl: bool = false,
    reserved_6: bool = false,
    reserved_7: bool = false,
    has_color_key: bool = false,
    first_frame_is_default: bool = false,
    // pad to 32 bits
    padding: u22 = 0,
};

const Decoder = @This();

// memory allocator
al: std.mem.Allocator,
// reader into provided PNG data
r: std.io.AnyReader,
img: image.Image = undefined,
crc: std.hash.Crc32,
stage: Stage = .start,
palette: []image.Color = undefined,
scratch: [3 * 256]u8 = [_]u8{0} ** (3 * 256),

pub fn decode(al: std.mem.Allocator, r: std.io.AnyReader) !image.Image {
    var d = Decoder{
        .al = al,
        .r = r,
        .crc = std.hash.Crc32.init(),
    };

    try d.checkHeader();

    try d.parseChunk();

    try d.readMetadata();

    return d.img;
}

fn checkHeader(self: *Decoder) !void {
    const bytes_read = try self.r.readAtLeast(&self.scratch, png_header.len);
    if (bytes_read < png_header.len) {
        return error.UnexpectedEof;
    }
    if (!std.mem.eql(u8, self.scratch[0..png_header.len], png_header)) {
        return error.InvalidPngHeader;
    }
}

fn parseChunk(self: *Decoder) !void {
    const chunk_header = try self.readChunkHeader();

    self.crc.update(&chunk_header.chunk_type.bytes());
    switch (chunk_header.chunk_type) {
        .ihdr => {},
    }
}

// Reads initial chunks and stops at the beginning of pixel data ('IDAT' and 'fdAT') or 'IEND'.
fn readMetadata(self: *Decoder) !void {
    while (true) {
        const header = try self.readChunkHeader();

        // Process chunk based on type...
        switch (header.chunk_type) {
            .plte => {
                // The PLTE chunk is only read once.
                // 1. There must not be more than one PLTE chunk.
                // 2. It must precede the first IDAT chunk (also tRNS chunk).
                // 3. Contains 1...256 RGB palette entries.
                if (self.flags.read_plte or self.flags.read_trns) {
                    return error.InvalidPng;
                }

                if (header.length == 0 or header.length > 768 or (header.length % 3) != 0) {
                    return error.PngInvalidPLTE;
                }
                self.flags.read_plte = true;

                const num_palette_entries = header.length / 3;
                std.debug.print("num_palette_entries: {d}\n", .{num_palette_entries});
            },
            .trns => return error.UnhandledtRNSChunk,
            .ihdr, .idat, .iend => return,
            else => unreachable,
        }
    }
}

fn skipChunk(self: *Decoder, length: u32) !void {
    // Skip chunk data
    try self.r.skipBytes(length, .{});
}

const ChunkHeader = struct {
    length: u32,
    chunk_type: ChunkType,
};

fn readChunkHeader(self: *Decoder) !ChunkHeader {
    var header: ChunkHeader = undefined;

    // Read chunk length (4 bytes, big endian)
    header.length = try self.r.readStructEndian(u32, .big);

    // Read chunk type (4 bytes, big endian)
    const tag = try self.r.readStructEndian(u32, .big);
    header.chunk_type = ChunkType.fromTag(tag);

    return header;
}

const ChunkType = enum(u32) {
    ihdr,
    plte,
    idat,
    iend,
    trns,
    unknown,

    // Convert 4 ASCII characters into a big-endian u32 tag
    fn makeTag(comptime a: u8, b: u8, c: u8, d: u8) u32 {
        return (@as(u32, a) << 24) | (@as(u32, b) << 16) | (@as(u32, c) << 8) | @as(u32, d);
    }

    pub fn fromTag(tag: u32) ChunkType {
        return switch (tag) {
            makeTag('I', 'H', 'D', 'R') => .ihdr,
            makeTag('P', 'L', 'T', 'E') => .plte,
            makeTag('I', 'D', 'A', 'T') => .idat,
            makeTag('I', 'E', 'N', 'D') => .iend,
            makeTag('t', 'R', 'N', 'S') => .trns,
            else => .unknown,
        };
    }

    fn bytes(self: ChunkType) [4]u8 {
        var b: [4]u8 = undefined;
        std.mem.writeInt(u32, &b, @intFromEnum(self), .big);
        return b;
    }
};
