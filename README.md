# zpix

Image decoding library in pure Zig. It supports:

**JPEG**

- Baseline and Progressive formats
- Gray, YCbCr, RGBA, and YCMK color formats.

**PNG**

- Basic truecolor, some gray

Here's proof! The Mac image viewer on the left, and a SDL image viewer in Zig using `zpix` to view a JPEG file:
![demo](demo.png)

## Usage

See [`zjpeg.zig`](./src/zjpeg.zig) for an example with SDL.

## Development

Run using `zig`:

    zig build run -- <input jpeg>

Or build and run:

    zig build
    ./zig-out/bin/zjpeg <input jpeg>

---
