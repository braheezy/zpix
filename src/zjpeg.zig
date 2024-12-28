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

            // print("****************\n", .{});

            // switch (img) {
            //     .YCbCr => |i| std.debug.print("i.y[0]: {d}\n", .{i.y[0]}),
            //     else => return error.NotReadyYet,
            // }

            // const pixels = dumpPixels(allocator, img);

            switch (img) {
                .YCbCr => |i| {
                    // try dumpImgPixelsToFile(img);
                    try draw(i);
                },
                else => return error.NotReadyYet,
            }
        }
    }
}

fn draw(img: *image.YCbCrImage) !void {
    if (sdl.SDL_Init(sdl.SDL_INIT_VIDEO) != 0) {
        std.debug.print("Failed to initialize SDL: {s}\n", .{sdl.SDL_GetError()});
        return;
    }
    defer sdl.SDL_Quit();

    const window = sdl.SDL_CreateWindow(
        "Image Viewer",
        sdl.SDL_WINDOWPOS_CENTERED,
        sdl.SDL_WINDOWPOS_CENTERED,
        @intCast(2048),
        @intCast(2048),
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
        sdl.SDL_RENDERER_ACCELERATED | sdl.SDL_RENDERER_PRESENTVSYNC,
    );
    if (renderer == null) {
        std.debug.print("Failed to create renderer: {s}\n", .{sdl.SDL_GetError()});
        return;
    }
    defer sdl.SDL_DestroyRenderer(renderer);

    const texture = sdl.SDL_CreateTexture(
        renderer,
        sdl.SDL_PIXELFORMAT_RGBA32,
        sdl.SDL_TEXTUREACCESS_STREAMING,
        @intCast(2048),
        @intCast(2048),
    );
    if (texture == null) {
        std.debug.print("Failed to create texture: {s}\n", .{sdl.SDL_GetError()});
        return;
    }
    defer sdl.SDL_DestroyTexture(texture);

    var tex_pixels: ?*anyopaque = null;
    var pitch: i32 = 0;
    if (sdl.SDL_LockTexture(texture, null, &tex_pixels, &pitch) != 0) {
        std.debug.print("Failed to lock texture: {s}\n", .{sdl.SDL_GetError()});
        return;
    }

    const tex_data: [*]u8 = @ptrCast(tex_pixels);
    const row_length = @as(usize, @intCast(pitch));
    const pixel_stride = 4; // RGBA is 4 bytes per pixel
    // print("pixels len: {d}\n", .{pixels.len});

    var y = img.bounds().min.y;
    while (y < img.bounds().max.y) : (y += 1) {
        var x = img.bounds().min.x;
        while (x < img.bounds().max.x) : (x += 1) {
            var color = img.YCbCrAt(x, y);
            const r, const g, const b, const a = color.rgba();

            const row_offset = @as(usize, @intCast(y - img.bounds().min.y)) * row_length;
            const col_offset = @as(usize, @intCast(x - img.bounds().min.x)) * pixel_stride;
            const dst_index = row_offset + col_offset;

            tex_data[dst_index + 0] = @intCast(r >> 8);
            tex_data[dst_index + 1] = @intCast(g >> 8);
            tex_data[dst_index + 2] = @intCast(b >> 8);
            tex_data[dst_index + 3] = @intCast(a >> 8);
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

fn dumpImgPixelsToFile(img: image.ImageType) !void {
    const pixel_file = try std.fs.cwd().createFile("pixel_dump.zig.raw", .{});
    defer pixel_file.close();

    var writer = pixel_file.writer();
    switch (img) {
        .YCbCr => |ycbcr| {
            // std.debug.print("dumpPixels: YCbCr ptr = {*}\n", .{ycbcr});
            print("dumping {d} x {d} pixels\n", .{ ycbcr.bounds().dX(), ycbcr.bounds().dY() });

            var y = ycbcr.bounds().min.y;

            while (y < ycbcr.bounds().max.y) : (y += 1) {
                var x = ycbcr.bounds().min.x;
                while (x < ycbcr.bounds().max.x) : (x += 1) {
                    var color = ycbcr.YCbCrAt(x, y);
                    const r, const g, const b, const a = color.rgba();
                    try writer.writeAll(&[_]u8{
                        @intCast(r >> 8),
                        @intCast(g >> 8),
                        @intCast(b >> 8),
                        @intCast(a >> 8),
                    });
                }
            }
        },
        else => @panic("Unsupported image type for dumping pixels"),
    }

    std.log.info("Pixel data written to 'pixel_dump.zig.raw'", .{});
}

fn dumpPixelsToFile(pixels: []u8, width: i32, height: i32) !void {
    const pixel_file = try std.fs.cwd().createFile("pixel_dump.zig.raw", .{});
    defer pixel_file.close();

    var writer = pixel_file.writer();
    const pixel_stride = 4; // 4 bytes per pixel (RGBA format)

    outer: for (0..@intCast(height)) |y| {
        const row_start = y * @as(usize, @intCast(width)) * pixel_stride;

        for (0..@intCast(width)) |x| {
            const index = row_start + (x * pixel_stride);
            if (index == pixels.len) {
                break :outer;
            }

            try writer.writeAll(&[_]u8{
                pixels[index + 0], // Red
                pixels[index + 1], // Green
                pixels[index + 2], // Blue
                pixels[index + 3], // Alpha
            });
        }
    }

    std.log.info("Pixel data written to 'pixel_dump.zig.raw'", .{});
}

pub fn dumpPixels(allocator: std.mem.Allocator, img: image.ImageType) []u8 {
    // print("dumpPixels\n", .{});
    var pixelBuffer: []u8 = undefined;
    switch (img) {
        .YCbCr => |ycbcr| {
            // std.debug.print("dumpPixels: YCbCr ptr = {*}\n", .{ycbcr});

            const width: usize = @intCast(ycbcr.rect.max.x);
            const height: usize = @intCast(ycbcr.rect.max.y);
            const size: usize = @intCast(width * height * 3); // Y, Cb, Cr
            pixelBuffer = allocator.alloc(u8, size) catch unreachable;

            var index: usize = 0;
            for (0..height) |y| {
                for (0..width) |x| {
                    const color = ycbcr.YCbCrAt(@intCast(x), @intCast(y));
                    pixelBuffer[index + 0] = color.y;
                    pixelBuffer[index + 1] = color.cb;
                    pixelBuffer[index + 2] = color.cr;

                    index += 3;
                }
            }
        },
        else => @panic("Unsupported image type for dumping pixels"),
    }

    return pixelBuffer;
}
