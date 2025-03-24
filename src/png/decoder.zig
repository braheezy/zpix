const std = @import("std");
const image = @import("image");

const png_header = "\x89PNG\r\n\x1a\n";

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

const Decoder = @This();

// memory allocator
al: std.mem.Allocator,
// reader into provided PNG data
r: std.io.AnyReader,
img: image.Image = undefined,
crc: std.hash.Crc32,
stage: Stage = .start,
scratch: [3 * 256]u8 = [_]u8{0} ** (3 * 256),

pub fn decode(al: std.mem.Allocator, r: std.io.AnyReader) !image.Image {
    var d = Decoder{
        .al = al,
        .r = r,
        .crc = std.hash.Crc32.init(),
    };

    try d.checkHeader();

    while (d.stage != .seen_iend) {
        try d.parseChunk();
    }

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
    _ = self;
    return error.NotImplemented;
}
