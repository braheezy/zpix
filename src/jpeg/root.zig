const std = @import("std");
const image = @import("image");

pub const Decoder = @import("decoder.zig");

pub const decode = Decoder.decode;

/// Convenience API to decode from an in-memory buffer.
pub fn loadFromBuffer(allocator: std.mem.Allocator, buffer: []const u8) !image.Image {
    var buffer_reader = std.io.fixedBufferStream(buffer);
    const reader = buffer_reader.reader().any();
    return try decode(allocator, reader);
}

/// Probe whether the provided memory buffer looks like a JPEG file.
pub fn probeBuffer(buffer: []const u8) bool {
    // JPEG starts with FF D8 and ends with FF D9 typically; we only check SOI.
    return buffer.len >= 2 and buffer[0] == 0xFF and buffer[1] == 0xD8;
}

/// Probe whether the file at `path` looks like a JPEG file.
pub fn probePath(path: []const u8) !bool {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return err;
    };
    defer file.close();
    var buf: [2]u8 = undefined;
    const n = try file.reader().read(&buf);
    return n >= 2 and buf[0] == 0xFF and buf[1] == 0xD8;
}

pub fn load(allocator: std.mem.Allocator, path: []const u8) !image.Image {
    const jpeg_file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.log.err("Failed to open jpeg file {s}: {any}", .{ path, err });
        return err;
    };
    defer jpeg_file.close();

    var bufferedReader = std.io.bufferedReader(jpeg_file.reader());
    const reader = bufferedReader.reader().any();

    const img = decode(allocator, reader) catch |err| {
        std.log.err("Failed to decode jpeg file: {any}", .{err});
        return err;
    };

    return img;
}

comptime {
    std.testing.refAllDecls(@This());
}
