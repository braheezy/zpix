const std = @import("std");
const loadFromBuffer = @import("zpix").png.loadFromBuffer;
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
    const file = @embedFile("basn0g01.png");
    const img = try loadFromBuffer(allocator, file);
    defer img.free(allocator);
    const stdout = std.io.getStdOut().writer();
    try sng(stdout, "basn0g01", img);
}
