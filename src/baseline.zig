const std = @import("std");

const jpeg = @import("jpeg.zig");
// Baseline DCT Decoder
const Decoder = struct {};

fn decode(allocator: std.mem.Allocator, jpeg_file: std.fs.File) jpeg.DecodedImage {
    _ = allocator;
    _ = jpeg_file;
    // Placeholder dimensions
    return jpeg.DecodedImage{ .width = 800, .height = 600 };
}

// interface for Baseline Decoder
pub var BaselineAPI = jpeg.DecoderAPI{
    .decode = decode,
};
