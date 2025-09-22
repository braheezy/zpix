const std = @import("std");
const image = @import("image");

pub const Decoder = @import("decoder.zig");

pub const decode = Decoder.decode;
pub const sng = @import("sng.zig").sng;

// PNG 8-byte signature
const png_signature = "\x89PNG\r\n\x1a\n";

pub fn load(allocator: std.mem.Allocator, path: []const u8) !image.Image {
    const png_file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.log.err("Failed to open png file {s}: {any}", .{ path, err });
        return err;
    };
    defer png_file.close();

    var bufferedReader = std.io.bufferedReader(png_file.reader());
    const reader = bufferedReader.reader().any();

    const img = try decode(allocator, reader);

    return img;
}

pub fn loadFromBuffer(allocator: std.mem.Allocator, buffer: []const u8) !image.Image {
    // create a buffer reader from the buffer
    var buffer_reader = std.io.fixedBufferStream(buffer);
    const reader = buffer_reader.reader().any();
    return try decode(allocator, reader);
}

/// Probe whether the provided memory buffer looks like a PNG file.
pub fn probeBuffer(buffer: []const u8) bool {
    return buffer.len >= png_signature.len and std.mem.eql(u8, buffer[0..png_signature.len], png_signature);
}

/// Probe whether the file at `path` looks like a PNG file.
pub fn probePath(path: []const u8) !bool {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return err;
    };
    defer file.close();
    var buf: [8]u8 = undefined;
    const n = try file.reader().read(&buf);
    return n >= 8 and std.mem.eql(u8, buf[0..8], png_signature);
}

comptime {
    _ = @import("decoder_test.zig");
}
