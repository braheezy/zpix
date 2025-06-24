const std = @import("std");
const image = @import("image");

/// QOI image decoder.
pub const Decoder = @import("decoder.zig");
/// Alias for the decoder submodule.
pub const decoder = Decoder;
/// Decode a QOI image from a reader.
pub const decode = Decoder.decode;

/// QOI image encoder.
pub const Encoder = @import("encoder.zig");
/// Alias for the encoder submodule.
pub const encoder = Encoder;
/// Encode pixel data into QOI format.
pub const encode = Encoder.encode;

/// Load and decode a QOI image from the file at `path`.
pub fn load(allocator: std.mem.Allocator, path: []const u8) !image.Image {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.log.err("Failed to open qoi file {s}: {any}", .{path, err});
        return err;
    };
    defer file.close();

    var buffered = std.io.bufferedReader(file.reader());
    const reader = buffered.reader().any();
    return try decode(allocator, reader);
}

/// Decode a QOI image from the given memory buffer.
pub fn loadFromBuffer(allocator: std.mem.Allocator, buffer: []const u8) !image.Image {
    var buffer_reader = std.io.fixedBufferStream(buffer);
    const reader = buffer_reader.reader().any();
    return try decode(allocator, reader);
}
