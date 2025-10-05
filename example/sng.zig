const std = @import("std");
const load = @import("zpix").png.load;
const sng = @import("zpix").png.sng;

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

    // Memory allocation setup
    const allocator = debug_allocator.allocator();
    defer {
        if (debug_allocator.deinit() == .leak) {
            std.process.exit(1);
        }
    }
    // args parsing
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var filename: ?[]const u8 = null;
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout: *std.Io.Writer = &stdout_writer.interface;
    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try stdout.print("Usage: sng <filename>\n", .{});
            try stdout.flush();
            std.process.exit(0);
        } else {
            filename = arg;
        }
    }
    if (filename == null) {
        try stdout.print("Usage: sng <filename>\n", .{});
        try stdout.flush();
        std.process.exit(0);
    }

    const img = try load(allocator, filename.?);
    defer img.free(allocator);
    try sng(stdout, filename.?, img);
    try stdout.flush();
}
