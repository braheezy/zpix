const std = @import("std");
const image = @import("image");

pub const Decoder = @import("decoder.zig");

pub const decode = Decoder.decode;

pub fn load(allocator: std.mem.Allocator, path: []const u8) !image.Image {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.log.err("Failed to open bmp file {s}: {any}", .{ path, err });
        return err;
    };
    defer file.close();

    var buffered = std.io.bufferedReader(file.reader());
    const reader = buffered.reader().any();
    return try decode(allocator, reader);
}

pub fn loadFromBuffer(allocator: std.mem.Allocator, buffer: []const u8) !image.Image {
    var buffer_reader = std.io.fixedBufferStream(buffer);
    const reader = buffer_reader.reader().any();
    return try decode(allocator, reader);
}

/// Probe whether the provided memory buffer looks like a BMP file.
pub fn probeBuffer(buffer: []const u8) bool {
    // BMP signature is ASCII 'B''M'
    return buffer.len >= 2 and buffer[0] == 'B' and buffer[1] == 'M';
}

/// Probe whether the file at `path` looks like a BMP file.
pub fn probePath(path: []const u8) !bool {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return err;
    };
    defer file.close();
    var buf: [2]u8 = undefined;
    const n = try file.reader().read(&buf);
    return n >= 2 and buf[0] == 'B' and buf[1] == 'M';
}

comptime {
    _ = @import("decoder_test.zig");
}
