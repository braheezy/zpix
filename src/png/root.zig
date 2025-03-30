const std = @import("std");
const image = @import("image");

pub const Decoder = @import("decoder.zig");

pub const decode = Decoder.decode;

pub fn load(allocator: std.mem.Allocator, path: []const u8) !image.Image {
    const png_file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.log.err("Failed to open png file {s}: {any}", .{ path, err });
        return err;
    };
    defer png_file.close();

    var bufferedReader = std.io.bufferedReader(png_file.reader());
    const reader = bufferedReader.reader().any();

    const img = decode(allocator, reader) catch |err| {
        std.log.err("Failed to decode png file: {any}", .{err});
        return err;
    };

    return img;
}

comptime {
    _ = @import("decoder_test.zig");
}
