const std = @import("std");
const Image = @import("image").Image;
const Color = @import("color").Color;
const loadFromBuffer = @import("root.zig").loadFromBuffer;

// fakeIHDRUsings maps from filenames to fake IHDR "using" lines for our
// approximation to the sng command-line tool. The PNG model is that
// transparency (in the tRNS chunk) is separate to the color/grayscale/palette
// color model (in the IHDR chunk). The Go model is that the concrete
// image.Image type returned by png.Decode, such as image.RGBA (with all pixels
// having 100% alpha) or image.NRGBA, encapsulates whether or not the image has
// transparency. This map is a hack to work around the fact that the Go model
// can't otherwise discriminate PNG's "IHDR says color (with no alpha) but tRNS
// says alpha" and "IHDR says color with alpha".
var fake_ihdr_usings = std.StaticStringMap([]const u8).initComptime(.{
    .{ "ftbbn0g01", "    using grayscale;\n" },
    .{ "ftbbn0g02", "    using grayscale;\n" },
    .{ "ftbbn0g04", "    using grayscale;\n" },
    .{ "ftbbn2c16", "    using color;\n" },
    .{ "ftbgn2c16", "    using color;\n" },
    .{ "ftbrn2c08", "    using color;\n" },
    .{ "ftbwn0g16", "    using grayscale;\n" },
});

pub fn sng(writer: anytype, filename: []const u8, img: Image) !void {
    const bounds = img.bounds();
    const bit_depth: u8 = switch (img) {
        .RGBA, .NRGBA, .Gray => 8,
        .Paletted => |p| blk: {
            switch (p.palette.len) {
                0, 1, 2 => break :blk 1,
                3, 4 => break :blk 2,
                5...16 => break :blk 4,
                else => break :blk 8,
            }
        },
        else => 16,
    };

    try writer.print("#SNG: from {s}.png\nIHDR {{\n", .{filename});
    try writer.print(
        "    width: {d}; height: {d}; bitdepth: {d};\n",
        .{ bounds.dX(), bounds.dY(), bit_depth },
    );

    if (fake_ihdr_usings.get(filename)) |using| {
        try writer.print("{s}", .{using});
    } else {
        switch (img) {
            .Gray, .Gray16 => {
                try writer.print("    using grayscale;\n", .{});
            },
            .RGBA, .RGBA64 => {
                try writer.print("    using color;\n", .{});
            },
            .NRGBA, .NRGBA64 => {
                try writer.print("    using color alpha;\n", .{});
            },
            .Paletted => {
                try writer.print("    using color palette;\n", .{});
            },
            else => {
                try writer.print("unknown PNG decoder color model\n", .{});
            },
        }
    }

    try writer.print("}}\n", .{});
    try writer.print("gAMA {{1.0000}}\n", .{});
    switch (img) {
        .Paletted => {
            try writer.print("PLTE {{\n", .{});

            try writer.print("}}\n", .{});
        },
        else => {},
    }
    try writer.print("IMAGE {{\n    pixels hex\n", .{});

    const dy: usize = @intCast(bounds.dY());
    const dx: usize = @intCast(bounds.dX());
    for (0..dy) |y| {
        const y_int: i32 = @intCast(y);
        for (0..dx) |x| {
            const x_int: i32 = @intCast(x);
            const color = img.at(x_int, y_int);
            switch (img) {
                .Gray => {
                    try writer.print("{x:02}", .{color.gray.y});
                },
                .Gray16 => {
                    try writer.print("{x:04} ", .{color.gray16.y});
                },
                .RGBA => {
                    try writer.print("{x:02}{x:02}{x:02} ", .{
                        color.rgba.r,
                        color.rgba.g,
                        color.rgba.b,
                    });
                },
                .RGBA64 => {
                    try writer.print("{x:04}{x:04}{x:04}{x:04} ", .{
                        color.rgba64.r,
                        color.rgba64.g,
                        color.rgba64.b,
                        color.rgba64.a,
                    });
                },
                else => {},
            }
        }

        try writer.print("\n", .{});
    }
    try writer.print("}}\n", .{});
}
