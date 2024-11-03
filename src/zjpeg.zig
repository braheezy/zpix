//! Checks that the basename of the given path matches a string.
//!
//! Usage:
//!

const std = @import("std");

pub const jpeg = @import("jpeg.zig");
pub const Decoder = jpeg.Decoder;

const print = std.debug.print;

const helpText = "Usage: zlox [-h/--help] <jpeg file>\n";

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

    if (args.len == 1) {
        // no args, exit
        std.log.err("Missing input file\n", .{});
        // EX_USAGE: command line usage error
        std.process.exit(64);
    }

    // handle CLI arguments
    for (args[1..]) |arg| {
        const isHelpFlag = std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h");
        if (isHelpFlag) {
            try stdout.print(helpText, .{});
            std.process.exit(0);
        } else {
            // assume input file
            const jpeg_file = std.fs.cwd().openFile(arg, .{}) catch |err| {
                std.log.err("Failed to open jpeg file: {any}", .{err});
                // EX_NOINPUT: cannot open input
                std.process.exit(66);
            };
            defer jpeg_file.close();

            jpeg.validateFile(jpeg_file) catch |err| {
                std.log.err("Failed to validate jpeg file: {any}", .{err});
                // EX_DATAERR: data format error
                std.process.exit(65);
            };

            _ = jpeg.decode(allocator, jpeg_file) catch |err| {
                std.log.err("Failed to decode jpeg file: {any}", .{err});
            };
        }
    }
}
