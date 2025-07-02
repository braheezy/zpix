const std = @import("std");
const image = @import("zpix").image;
const jpeg = @import("zpix").jpeg;
const png = @import("zpix").png;
const qoi = @import("zpix").qoi;

const helpText =
    \\Usage: convert [options] <input file> <output file>
    \\Options:
    \\  -h, --help  Display this help message
    \\
    \\Supported input formats: JPEG, PNG, QOI
    \\Supported output formats: QOI
;

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    const allocator = debug_allocator.allocator();
    defer {
        if (debug_allocator.deinit() == .leak) {
            std.process.exit(1);
        }
    }

    const stdout = std.io.getStdOut().writer();

    // Read arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        try stdout.print("Error: Missing input or output file\n{s}", .{helpText});
        std.process.exit(64);
    }

    // handle CLI arguments
    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try stdout.print(helpText, .{});
            std.process.exit(0);
        }
    }

    const input_file = args[1];
    const output_file = args[2];

    // Check output is .qoi
    if (!std.mem.endsWith(u8, output_file, ".qoi")) {
        std.log.err("Error: Output file must be .qoi format\n", .{});
        std.process.exit(64);
    }

    // Load input image based on extension
    const file_ext = std.fs.path.extension(input_file);
    const img = if (std.mem.eql(u8, file_ext, ".jpg") or std.mem.eql(u8, file_ext, ".jpeg"))
        try jpeg.load(allocator, input_file)
    else if (std.mem.eql(u8, file_ext, ".png"))
        try png.load(allocator, input_file)
    else if (std.mem.eql(u8, file_ext, ".qoi"))
        try qoi.load(allocator, input_file)
    else
        return error.UnsupportedFormat;

    defer img.free(allocator);

    // Get raw RGBA pixels
    const pixels = try img.rgbaPixels(allocator);
    defer allocator.free(pixels);

    // Create QOI descriptor
    const desc = qoi.Desc{
        .width = @intCast(img.bounds().dX()),
        .height = @intCast(img.bounds().dY()),
        .channels = 4, // Always use RGBA
        .colorspace = 0, // sRGB with linear alpha
    };

    // Encode to QOI
    const encoded = try qoi.encode(allocator, pixels, desc);
    defer allocator.free(encoded);

    // Write to output file
    const file = try std.fs.cwd().createFile(output_file, .{});
    defer file.close();
    try file.writeAll(encoded);

    std.log.info("Successfully converted {s} to {s}", .{ input_file, output_file });
}
