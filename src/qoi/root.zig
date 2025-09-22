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
pub const Desc = Encoder.Desc;

/// Load and decode a QOI image from the file at `path`.
pub fn load(allocator: std.mem.Allocator, path: []const u8) !image.Image {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.log.err("Failed to open qoi file {s}: {any}", .{ path, err });
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

/// Probe whether the provided memory buffer looks like a QOI file.
pub fn probeBuffer(buffer: []const u8) bool {
    // QOI magic is 0x71 0x6F 0x69 0x66 ("qoif") at bytes 0..3
    if (buffer.len < 4) return false;
    const m0: u8 = 0x71; // 'q'
    const m1: u8 = 0x6F; // 'o'
    const m2: u8 = 0x69; // 'i'
    const m3: u8 = 0x66; // 'f'
    return buffer[0] == m0 and buffer[1] == m1 and buffer[2] == m2 and buffer[3] == m3;
}

/// Probe whether the file at `path` looks like a QOI file.
pub fn probePath(path: []const u8) !bool {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return err;
    };
    defer file.close();
    var buf: [4]u8 = undefined;
    const n = try file.reader().read(&buf);
    if (n < 4) return false;
    return probeBuffer(buf[0..]);
}
