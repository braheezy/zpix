const std = @import("std");

const load = @import("root.zig").load;
const Decoder = @import("decoder.zig");
const Image = @import("image").Image;
const sng = @import("sng.zig").sng;

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
    // "basn3p08-trns",
    "basn4a08",
    "basn4a16",
    "basn6a08",
    "basn6a16",
    // "ftbbn0g01",
    // "ftbbn0g02",
    // "ftbbn0g04",
    // "ftbbn2c16",
    // "ftbbn3p08",
    // "ftbgn2c16",
    // "ftbgn3p08",
    // "ftbrn2c08",
    // "ftbwn0g16",
    // "ftbwn3p08",
    // "ftbyn3p08",
    // "ftp0n0g08",
    "ftp0n2c08",
    // "ftp0n3p08",
    // "ftp1n3p08",
};

test "decode" {
    const allocator = std.testing.allocator;
    for (filenames) |filename| {
        // Load the PNG file
        const png_path = std.fmt.allocPrint(allocator, "src/testdata/png/{s}.png", .{filename}) catch unreachable;
        defer allocator.free(png_path);
        const img = load(allocator, png_path) catch |err| {
            std.log.err("Failed to load {s}: {!}", .{ png_path, err });
            std.process.exit(0);
        };
        defer img.free(allocator);

        if (std.mem.eql(u8, filename, "basn4a16")) {
            // basn4a16.sng is gray + alpha but sng() will produce true color + alpha
            // so we just check a single random pixel.
            const color = img.at(2, 1).nrgba64;

            try std.testing.expect(color.r == 0x11a7 and color.g == 0x11a7 and color.b == 0x11a7 and color.a == 0x1085);
            continue;
        }

        // Create a buffer for our SNG output
        var output_buf: [0x10000]u8 = undefined;
        var output_stream = std.io.fixedBufferStream(&output_buf);
        const writer = output_stream.writer();

        // Generate SNG output
        try sng(writer, filename, img);
        const written = output_stream.pos;
        const output_slice = output_buf[0..written];

        // Read the expected SNG file
        const sng_path = std.fmt.allocPrint(allocator, "src/testdata/png/{s}.sng", .{filename}) catch unreachable;
        defer allocator.free(sng_path);

        const expected_file = try std.fs.cwd().openFile(sng_path, .{});
        defer expected_file.close();

        var expected_buf = std.ArrayList(u8).init(allocator);
        defer expected_buf.deinit();
        try expected_file.reader().readAllArrayList(&expected_buf, 0x10000);

        // Compare line by line
        var output_lines = std.mem.splitSequence(u8, output_slice, "\n");
        var expected_lines = std.mem.splitSequence(u8, expected_buf.items, "\n");

        while (true) {
            const output_line = output_lines.next();
            const expected_line = expected_lines.next();

            if (output_line == null and expected_line == null) break;
            if (output_line == null or expected_line == null) {
                std.debug.print("Line count mismatch for {s}\n", .{filename});
                return error.TestUnexpectedResult;
            }

            std.testing.expectEqualStrings(expected_line.?, output_line.?) catch {
                std.debug.print("Line mismatch for {s}\n", .{filename});
                return error.TestUnexpectedResult;
            };
        }
    }
}
