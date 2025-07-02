# zpix

Image decoding library in pure Zig. It supports:

**JPEG**

- Baseline and Progressive formats
- Gray, YCbCr, RGBA, and YCMK color formats.

**PNG**

- Gray 1, 2, 4, 8, and 16 bit
- Gray + alpha 8 bit and 16 bit
- Truecolor 8 bit and 16 bit
- Truecolor + alpha, 8 bit and 16 bit
- Paletted 1, 2, 4, and 8 bit
- Interlaced

**QOI**

- Encoding and decoding Quite OK Image (QOI) file format.

Here's proof! The Mac image viewer on the left, and a SDL image viewer in Zig using `zpix` to view a JPEG file:
![demo](demo.png)

## Usage

Add to project:

    zig fetch --save git+https://github.com/braheezy/zpix

In your `build.zig`:

    const zpix = b.dependency("zpix", .{});
    root_module.addImport("zjpeg", zpix.module("jpeg"));
    root_module.addImport("png", zpix.module("png"));
    // Or the whole module that has everything
    exe.root_module.addImport("zpix", zpix.module("zpix"));

In your program, load an image file

```zig
const jpeg = @import("jpeg");
const png  = @import("png");
const qoi  = @import("qoi");
// or const jpeg = @import("zpix").jpeg;
// or const png  = @import("zpix").png;
// or const qoi  = @import("zpix").qoi;

const img = if (std.mem.eql(u8, file_ext, ".jpg") or std.mem.eql(u8, file_ext, ".jpeg"))
    try jpeg.load(allocator, arg)
else if (std.mem.eql(u8, file_ext, ".png")) png: {
    const img = png.load(allocator, arg) catch {
        std.process.exit(0);
    };
    break :png img;
} else if (std.mem.eql(u8, file_ext, ".qoi")) qoi: {
    const img = qoi.load(allocator, arg) catch {
        std.process.exit(0);
    };
    break :qoi img;
} else return error.UnsupportedFileExtension;

defer {
    img.free(allocator);
}
// Do something with pixels
```

See [`example/zpixview.zig`](./example/zpixview.zig) for an example with SDL.

## Development

Run using `zig`:

    zig build run -- <input image>

Or build and run:

    zig build
    ./zig-out/bin/zpixview <input image>
