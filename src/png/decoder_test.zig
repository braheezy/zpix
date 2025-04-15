const std = @import("std");

const load = @import("root.zig").load;
const Decoder = @import("decoder.zig");

const filenames = [_][]const u8{
    "basn0g01",
    "basn0g01-30",
    "basn0g02",
    "basn0g02-29",
    "basn0g04",
    "basn0g04-31",
    "basn0g08",
    "basn0g16",
    "basn2c08",
    "basn2c16",
    "basn3p01",
    "basn3p02",
    "basn3p04",
    "basn3p04-31i",
    "basn3p08",
    "basn3p08-trns",
    "basn4a08",
    "basn4a16",
    "basn6a08",
    "basn6a16",
    "ftbbn0g01",
    "ftbbn0g02",
    "ftbbn0g04",
    "ftbbn2c16",
    "ftbbn3p08",
    "ftbgn2c16",
    "ftbgn3p08",
    "ftbrn2c08",
    "ftbwn0g16",
    "ftbwn3p08",
    "ftbyn3p08",
    "ftp0n0g08",
    "ftp0n2c08",
    "ftp0n3p08",
    "ftp1n3p08",
};

test "decode" {
    const allocator = std.testing.allocator;
    for (filenames) |filename| {
        const path = std.fmt.allocPrint(allocator, "src/testdata/png/{s}.png", .{filename}) catch unreachable;
        defer allocator.free(path);
        const img = load(allocator, path) catch |err| {
            std.log.err("Failed to load {s}: {!}", .{ path, err });
            std.process.exit(0);
        };
        defer img.free(allocator);
    }
}
