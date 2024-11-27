//! Checks that the basename of the given path matches a string.
//!
//! Usage:
//!

const std = @import("std");

pub const jpeg = @import("jpeg/reader.zig");
pub const Decoder = jpeg.Decoder;

const print = std.debug.print;

const helpText =
    \\Usage: zjpeg [options]] <jpeg file>
    \\Options:
    \\  -h, --help  Display this help message
    \\  -c, --config-only  Decode and print the image configuration
;

pub fn main() !void {
    // Memory allocation setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer if (gpa.deinit() == .leak) {
        std.process.exit(1);
    };

    const stdout = std.io.getStdOut().writer();

    // Read arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        // no args, exit
        std.log.err("Missing input file\n", .{});
        // EX_USAGE: command line usage error
        std.process.exit(64);
    }
    var isConfigOnlyFlag: bool = false;

    // handle CLI arguments
    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try stdout.print(helpText, .{});
            std.process.exit(0);
        } else if (std.mem.eql(u8, arg, "--config-only") or std.mem.eql(u8, arg, "-c")) {
            isConfigOnlyFlag = true;
        } else {
            // assume input file
            const jpeg_file = std.fs.cwd().openFile(arg, .{}) catch |err| {
                std.log.err("Failed to open jpeg file {s}: {any}", .{ arg, err });
                // EX_NOINPUT: cannot open input
                std.process.exit(66);
            };
            defer jpeg_file.close();

            var bufferedReader = std.io.bufferedReader(jpeg_file.reader());
            const reader = bufferedReader.reader().any();

            if (isConfigOnlyFlag) {
                const img_config = try jpeg.decodeConfig(reader);
                print("Image config: {any}\n", .{img_config});
                std.process.exit(0);
            }

            _ = jpeg.decode(allocator, reader) catch |err| {
                std.log.err("Failed to decode jpeg file: {any}", .{err});
            };
        }
    }
}
