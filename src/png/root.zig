const std = @import("std");
const image = @import("image");

pub const Decoder = @import("decoder.zig");

pub const decode = Decoder.decode;
pub const sng = @import("sng.zig").sng;

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

comptime {
    _ = @import("decoder_test.zig");
}
