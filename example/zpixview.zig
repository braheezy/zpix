const std = @import("std");
const builtin = std.builtin;
const image = @import("zpix").image;
const jpeg = @import("zpix").jpeg;
const png = @import("zpix").png;
const qoi = @import("zpix").qoi;

const sdl = @cImport({
    @cInclude("SDL2/SDL.h");
});

const print = std.debug.print;

const helpText =
    \\Usage: zpixview [options] <image file>
    \\Options:
    \\  -h, --help  Display this help message
;

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

    // Memory allocation setup
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

    if (args.len < 2) {
        // no args, exit
        std.log.err("Missing input file\n", .{});
        // EX_USAGE: command line usage error
        std.process.exit(64);
    }

    // handle CLI arguments
    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try stdout.print(helpText, .{});
            std.process.exit(0);
        } else {
            const file_ext = std.fs.path.extension(arg);
            const img = if (std.mem.eql(u8, file_ext, ".jpg") or std.mem.eql(u8, file_ext, ".jpeg"))
                try jpeg.load(allocator, arg)
            else if (std.mem.eql(u8, file_ext, ".png")) png: {
                const img = png.load(allocator, arg) catch {
                    std.process.exit(0);
                };
                break :png img;
            } else if (std.mem.eql(u8, file_ext, ".qoi")) qoi: {
                const img = qoi.load(allocator, arg) catch |err| {
                    std.log.err("Error loading QOI file: {s}", .{@errorName(err)});
                    std.process.exit(1);
                };
                break :qoi img;
            } else return error.UnsupportedFileExtension;

            defer {
                img.free(allocator);
            }

            try draw(allocator, arg, img);
        }
    }
}

fn draw(al: std.mem.Allocator, file_name: []const u8, img: image.Image) !void {
    if (sdl.SDL_Init(sdl.SDL_INIT_VIDEO) != 0) {
        std.debug.print("Failed to initialize SDL: {s}\n", .{sdl.SDL_GetError()});
        return;
    }
    defer sdl.SDL_Quit();

    const width = img.bounds().dX();
    const height = img.bounds().dY();
    const desired_window_width = 400;
    const desired_window_height = 300;
    const scale_factor = @min(@as(f32, @floatFromInt(width)) / desired_window_width, @as(f32, @floatFromInt(height)) / desired_window_height);

    const window_title = try std.fmt.allocPrintZ(al, "zjpeg view - {s}", .{file_name});
    defer al.free(window_title);

    // Ensure scale_factor is not too small to avoid division issues
    const safe_scale_factor = if (scale_factor < 0.001) 1.0 else scale_factor;

    // Calculate window dimensions with bounds checking
    const window_width = @min(1920, @as(i32, @intFromFloat(@as(f32, @floatFromInt(width)) / safe_scale_factor)));
    const window_height = @min(1080, @as(i32, @intFromFloat(@as(f32, @floatFromInt(height)) / safe_scale_factor)));

    const window = sdl.SDL_CreateWindow(
        window_title,
        sdl.SDL_WINDOWPOS_CENTERED,
        sdl.SDL_WINDOWPOS_CENTERED,
        window_width,
        window_height,
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
        @intCast(width),
        @intCast(height),
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

    const pixels = try img.rgbaPixels(al);
    defer al.free(pixels);

    const tex_data: [*]u8 = @ptrCast(tex_pixels);

    var i: usize = 0;
    while (i < pixels.len) : (i += 4) {
        tex_data[i + 0] = pixels[i + 0];
        tex_data[i + 1] = pixels[i + 1];
        tex_data[i + 2] = pixels[i + 2];
        tex_data[i + 3] = pixels[i + 3];
    }

    sdl.SDL_UnlockTexture(texture);

    var event: sdl.SDL_Event = undefined;
    var running = true;
    while (running) {
        while (sdl.SDL_PollEvent(&event) != 0) {
            switch (event.type) {
                sdl.SDL_QUIT => running = false,
                sdl.SDL_KEYDOWN => {
                    if (event.key.keysym.sym == sdl.SDLK_ESCAPE) {
                        running = false;
                    }
                },
                else => {},
            }
        }

        _ = sdl.SDL_RenderClear(renderer);
        _ = sdl.SDL_RenderCopy(renderer, texture, null, null);
        sdl.SDL_RenderPresent(renderer);
    }
}
