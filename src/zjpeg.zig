//! Checks that the basename of the given path matches a string.
//!
//! Usage:
//!

const std = @import("std");
const image = @import("image.zig");

const sdl = @cImport({
    @cInclude("SDL2/SDL.h");
});

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

            const img = jpeg.decode(allocator, reader) catch |err| {
                std.log.err("Failed to decode jpeg file: {any}", .{err});
                return err;
            };

            const bounds = img.bounds();
            // const color_space = img.getColorSpace();

            std.log.info("Got image!\nBounds: {d}x{d}", .{
                bounds.dX(),
                bounds.dY(),
            });

            try draw(img);
        }
    }
}

fn draw(img: image.Image) !void {
    if (sdl.SDL_Init(sdl.SDL_INIT_VIDEO) != 0) {
        std.debug.print("Failed to initialize SDL: {s}\n", .{sdl.SDL_GetError()});
        return;
    }
    defer sdl.SDL_Quit();

    const window = sdl.SDL_CreateWindow(
        "Image Viewer",
        sdl.SDL_WINDOWPOS_CENTERED,
        sdl.SDL_WINDOWPOS_CENTERED,
        800,
        600,
        sdl.SDL_WINDOW_SHOWN,
    );
    if (window == null) {
        std.debug.print("Failed to create window: {s}\n", .{sdl.SDL_GetError()});
        return;
    }
    defer sdl.SDL_DestroyWindow(window);

    const renderer = sdl.SDL_CreateRenderer(
        window,
        -1,
        sdl.SDL_RENDERER_ACCELERATED,
    );
    if (renderer == null) {
        std.debug.print("Failed to create renderer: {s}\n", .{sdl.SDL_GetError()});
        return;
    }
    defer sdl.SDL_DestroyRenderer(renderer);

    const bounds = img.bounds();
    const width = bounds.dX();
    const height = bounds.dY();

    const texture = sdl.SDL_CreateTexture(
        renderer,
        sdl.SDL_PIXELFORMAT_RGBA32,
        sdl.SDL_TEXTUREACCESS_STREAMING,
        width,
        height,
    );
    if (texture == null) {
        std.debug.print("Failed to create texture: {s}\n", .{sdl.SDL_GetError()});
        return;
    }
    defer sdl.SDL_DestroyTexture(texture);

    var pixels: ?*anyopaque = null;
    var pitch: i32 = 0;
    if (sdl.SDL_LockTexture(texture, null, &pixels, &pitch) != 0) {
        std.debug.print("Failed to lock texture: {s}\n", .{sdl.SDL_GetError()});
        return;
    }

    const pixel_data: [*]u8 = @ptrCast(pixels);
    for (0..@intCast(height)) |y| {
        for (0..@intCast(width)) |x| {
            const color = img.at(@intCast(x), @intCast(y)).rgba();
            const pitch_u: usize = @intCast(pitch);
            const index = (y * pitch_u + x * 4);
            pixel_data[index + 0] = @as(u8, @intCast(color[0] >> 8)); // Red
            pixel_data[index + 1] = @as(u8, @intCast(color[1] >> 8)); // Green
            pixel_data[index + 2] = @as(u8, @intCast(color[2] >> 8)); // Blue
            pixel_data[index + 3] = @as(u8, @intCast(color[3] >> 8)); // Alpha
        }
    }

    sdl.SDL_UnlockTexture(texture);

    var event: sdl.SDL_Event = undefined;
    var running = true;
    while (running) {
        while (sdl.SDL_PollEvent(&event) != 0) {
            switch (event.type) {
                sdl.SDL_QUIT => running = false,
                else => {},
            }
        }

        _ = sdl.SDL_RenderClear(renderer);
        _ = sdl.SDL_RenderCopy(renderer, texture, null, null);
        sdl.SDL_RenderPresent(renderer);
    }
}
