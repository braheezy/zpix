const std = @import("std");
const geom = @import("geometry.zig");
const Rectangle = geom.Rectangle;
const Point = geom.Point;
const Color = @import("color.zig").Color;
const Model = @import("color.zig").Model;

/// Config holds an image's color model and dimensions.
pub const Config = struct {
    width: u32,
    height: u32,
    color_model: Model,
};

/// Image is a finite rectangular grid of [Color] values taken from a color
/// model.
pub const Image = union(enum) {
    Gray: GrayImage,
    YCbCr: YCbCrImage,
    RGBA: RGBAImage,
    CMYK: CMYKImage,

    // bounds returns the domain for which At can return non-zero color.
    // The bounds do not necessarily contain the point (0, 0).
    pub fn bounds(self: Image) Rectangle {
        return switch (self) {
            .Gray => |img| img.bounds(),
            .YCbCr => |img| img.bounds(),
            .RGBA => |img| img.bounds(),
            .CMYK => |img| img.bounds(),
        };
    }

    // at returns the color of the pixel at (x, y).
    // at(bounds().min.X, bounds().min.Y) returns the upper-left pixel of the grid.
    // at(Bounds().max.X-1, bounds().max.Y-1) returns the lower-right one.
    pub fn at(self: Image, x: i32, y: i32) Color {
        return switch (self) {
            .Gray => |img| img.at(x, y),
            .YCbCr => |img| img.at(x, y),
            .RGBA => |img| img.at(x, y),
            .CMYK => |img| img.at(x, y),
        };
    }

    pub fn free(self: Image, al: std.mem.Allocator) void {
        switch (self) {
            .Gray => |img| {
                al.free(img.pixels);
            },
            .YCbCr => |img| {
                al.free(img.pixels);
            },
            .RGBA => |img| {
                al.free(img.pixels);
            },
            .CMYK => |img| {
                al.free(img.pixels);
            },
        }
    }

    /// Returns the pixels in RGBA format (8 bits per channel), regardless of the source format.
    /// Caller owns the returned memory and must free it.
    pub fn rgbaPixels(self: Image, al: std.mem.Allocator) ![]u8 {
        const rect = self.bounds();
        const width = rect.dX();
        const height = rect.dY();
        const pixel_count = @as(usize, @intCast(width * height));
        const rgba_len = pixel_count * 4;

        var rgba_pixels = try al.alloc(u8, rgba_len);
        errdefer al.free(rgba_pixels);

        var y: i32 = rect.min.y;
        while (y < rect.max.y) : (y += 1) {
            var x: i32 = rect.min.x;
            while (x < rect.max.x) : (x += 1) {
                const color = self.at(x, y);
                const rgba = color.toRGBA();
                const i = @as(usize, @intCast((y - rect.min.y) * width + (x - rect.min.x))) * 4;

                // Convert from 16-bit per channel to 8-bit per channel
                rgba_pixels[i + 0] = @intCast(rgba[0] >> 8); // R
                rgba_pixels[i + 1] = @intCast(rgba[1] >> 8); // G
                rgba_pixels[i + 2] = @intCast(rgba[2] >> 8); // B
                rgba_pixels[i + 3] = @intCast(rgba[3] >> 8); // A
            }
        }

        return rgba_pixels;
    }
};

pub const RGBAImage = struct {
    pixels: []u8 = undefined,
    stride: usize = 0,
    rect: Rectangle = undefined,

    pub fn init(
        al: std.mem.Allocator,
        rect: Rectangle,
    ) !RGBAImage {
        const pixel_len = pixelBufferLength(4, rect, "RGBA");
        const pixels = try al.alloc(u8, pixel_len);
        return RGBAImage{
            .pixels = pixels,
            .stride = @intCast(rect.dX() * 4),
            .rect = rect,
        };
    }

    pub fn subImage(self: *RGBAImage, rect: Rectangle) !?RGBAImage {
        if (rect.Intersect(self.rect)) |r| {
            const i: usize = @intCast(self.pixOffset(r.min.x, r.min.y));
            const sub_img = RGBAImage{
                .pixels = self.pixels[i..],
                .stride = self.stride,
                .rect = r,
            };
            return sub_img;
        } else {
            return null;
        }
    }

    // pixOffset returns the index of the first element of Pix that corresponds to
    // the pixel at (x, y).
    pub fn pixOffset(self: RGBAImage, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.stride);
        return (y - self.rect.min.y) * i + (x - self.rect.min.x) * 4;
    }

    pub fn bounds(self: RGBAImage) Rectangle {
        return self.rect;
    }

    pub fn at(self: RGBAImage, x: i32, y: i32) Color {
        return self.rgbaAt(x, y);
    }

    pub fn rgbaAt(self: RGBAImage, x: i32, y: i32) Color {
        const pt = Point{ .x = x, .y = y };
        if (!pt.In(self.rect)) {
            return Color{ .RGBA = .{} };
        }
        const i: usize = @intCast(self.pixOffset(x, y));
        const s = self.pixels[i .. i + 4];
        return Color.rgba(
            s[0],
            s[1],
            s[2],
            s[3],
        );
    }
};

pub const YCbCrSubsample = enum {
    Ratio444,
    Ratio422,
    Ratio420,
    Ratio440,
    Ratio411,
    Ratio410,
};

pub const YCbCrImage = struct {
    y: []u8 = undefined,
    cb: []u8 = undefined,
    cr: []u8 = undefined,
    y_stride: usize = 0,
    c_stride: usize = 0,
    subsample_ratio: YCbCrSubsample,
    rect: Rectangle = undefined,
    pixels: []u8 = undefined,

    pub fn init(
        al: std.mem.Allocator,
        rect: Rectangle,
        subsample_ratio: YCbCrSubsample,
    ) !YCbCrImage {
        const w, const h, const cw, const ch = yCbCrSize(rect, subsample_ratio);

        // totalLength should be the same as i2, below, for a valid Rectangle rect.
        const total_length = add2NonNeg(
            mul3NonNeg(1, w, h),
            mul3NonNeg(2, cw, ch),
        );
        if (total_length < 0) {
            std.debug.panic("image: NewYCbCr Rectangle has huge or negative dimensions", .{});
        }

        const i_0: usize = @intCast((w * h) + (0 * cw * ch));
        const i_1: usize = @intCast((w * h) + (1 * cw * ch));
        const i_2: usize = @intCast((w * h) + (2 * cw * ch));
        const pixels = try al.alloc(u8, i_2);
        // set all values to zero
        for (pixels) |*p| {
            p.* = 0;
        }

        return YCbCrImage{
            .y = pixels[0..i_0],
            .cb = pixels[i_0..i_1],
            .cr = pixels[i_1..i_2],
            .y_stride = @intCast(w),
            .c_stride = @intCast(cw),
            .subsample_ratio = subsample_ratio,
            .rect = rect,
            .pixels = pixels,
        };
    }

    fn yCbCrSize(r: Rectangle, subsample_ratio: YCbCrSubsample) struct { i32, i32, i32, i32 } {
        const w = r.dX();
        const h = r.dY();
        var cw: i32 = 0;
        var ch: i32 = 0;

        switch (subsample_ratio) {
            .Ratio422 => {
                cw = @divTrunc((r.max.x + 1), 2) - @divTrunc(r.min.x, 2);
                ch = h;
            },
            .Ratio420 => {
                cw = @divTrunc((r.max.x + 1), 2) - @divTrunc(r.min.x, 2);
                ch = @divTrunc((r.max.y + 1), 2) - @divTrunc(r.min.y, 2);
            },
            .Ratio440 => {
                cw = w;
                ch = @divTrunc((r.max.y + 1), 2) - @divTrunc(r.min.y, 2);
            },
            .Ratio411 => {
                cw = @divTrunc((r.max.x + 3), 4) - @divTrunc(r.min.x, 4);
                ch = h;
            },
            .Ratio410 => {
                cw = @divTrunc((r.max.x + 3), 4) - @divTrunc(r.min.x, 4);
                ch = @divTrunc((r.max.y + 1), 2) - @divTrunc(r.min.y, 2);
            },
            else => { // Default to Ratio444
                cw = w;
                ch = h;
            },
        }

        return .{ w, h, cw, ch };
    }

    pub fn subImage(self: YCbCrImage, allocator: std.mem.Allocator, rect: Rectangle) !?YCbCrImage {
        if (rect.Intersect(self.rect)) |r| {
            const yi: usize = @intCast(self.yOffset(r.min.x, r.min.y));
            const ci: usize = @intCast(self.cOffset(r.min.x, r.min.y));

            const pixels = try allocator.alloc(u8, self.pixels.len);
            @memcpy(pixels, self.pixels);

            // Calculate the offsets in the original buffer
            const y_offset = @intFromPtr(self.y.ptr) - @intFromPtr(self.pixels.ptr);
            const cb_offset = @intFromPtr(self.cb.ptr) - @intFromPtr(self.pixels.ptr);
            const cr_offset = @intFromPtr(self.cr.ptr) - @intFromPtr(self.pixels.ptr);

            return YCbCrImage{
                .y = pixels[y_offset..][yi..],
                .cb = pixels[cb_offset..][ci..],
                .cr = pixels[cr_offset..][ci..],
                .y_stride = self.y_stride,
                .c_stride = self.c_stride,
                .subsample_ratio = self.subsample_ratio,
                .rect = r,
                .pixels = pixels,
            };
        } else {
            return null;
        }
    }

    // YOffset returns the index of the first element of Y that corresponds to
    // the pixel at (x, y).
    pub fn yOffset(self: YCbCrImage, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.y_stride);
        return (y - self.rect.min.y) * i + (x - self.rect.min.x);
    }

    // COffset returns the index of the first element of Cb or Cr that corresponds
    // to the pixel at (x, y).
    pub fn cOffset(self: YCbCrImage, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.c_stride);
        return switch (self.subsample_ratio) {
            .Ratio422 => (y - self.rect.min.y) * i + (@divTrunc(x, 2) - @divTrunc(self.rect.min.x, 2)),
            .Ratio420 => (@divTrunc(y, 2) - @divTrunc(self.rect.min.y, 2)) * i + (@divTrunc(x, 2) - @divTrunc(self.rect.min.x, 2)),
            .Ratio440 => (@divTrunc(y, 2) - @divTrunc(self.rect.min.y, 2)) * i + (x - self.rect.min.x),
            .Ratio411 => (y - self.rect.min.y) * i + (@divTrunc(x, 4) - @divTrunc(self.rect.min.x, 4)),
            .Ratio410 => (@divTrunc(y, 2) - @divTrunc(self.rect.min.y, 2)) * i + (@divTrunc(x, 4) - @divTrunc(self.rect.min.x, 4)),
            // Default to 4:4:4 subsampling.
            else => (y - self.rect.min.y) * i + (x - self.rect.min.x),
        };
    }

    pub fn bounds(self: YCbCrImage) Rectangle {
        return self.rect;
    }
    pub fn at(self: YCbCrImage, x: i32, y: i32) Color {
        return self.YCbCrAt(x, y);
    }

    pub fn YCbCrAt(self: YCbCrImage, x: i32, y: i32) Color {
        // Check if the point (x, y) is within the rectangle.
        const pt = Point{ .x = x, .y = y };
        if (!pt.In(self.rect)) {
            return Color{ .YCbCr = .{ .y = 0, .cb = 0, .cr = 0 } };
        }

        // Calculate offsets for Y and Cb/Cr.
        const yi: usize = @intCast(self.yOffset(x, y));
        const ci: usize = @intCast(self.cOffset(x, y));

        return Color{ .YCbCr = .{
            .y = self.y[yi],
            .cb = self.cb[ci],
            .cr = self.cr[ci],
        } };
    }
};

pub const GrayImage = struct {
    pixels: []u8 = undefined,
    stride: usize = 0,
    rect: Rectangle = undefined,

    pub fn init(
        al: std.mem.Allocator,
        rect: Rectangle,
    ) !GrayImage {
        const pixel_len = pixelBufferLength(1, rect, "Gray");
        const pixels = try al.alloc(u8, pixel_len);
        return GrayImage{
            .pixels = pixels,
            .stride = @intCast(rect.dX()),
            .rect = rect,
        };
    }

    pub fn subImage(self: GrayImage, rect: Rectangle) !?GrayImage {
        if (rect.Intersect(self.rect)) |r| {
            const i: usize = @intCast(self.pixOffset(r.min.x, r.min.y));
            return GrayImage{
                .pixels = self.pixels[i..],
                .stride = self.stride,
                .rect = r,
            };
        } else {
            return null;
        }
    }
    // PixOffset returns the index of the first element of Pix that corresponds to
    // the pixel at (x, y).
    pub fn pixOffset(self: GrayImage, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.stride);
        return (y - self.rect.min.y) * i + (x - self.rect.min.x) * 1;
    }

    pub fn bounds(self: GrayImage) Rectangle {
        return self.rect;
    }

    pub fn at(self: GrayImage, x: i32, y: i32) Color {
        return self.grayAt(x, y);
    }

    fn grayAt(self: GrayImage, x: i32, y: i32) Color {
        const pt = Point{ .x = x, .y = y };
        if (!pt.In(self.rect)) {
            return Color.gray(0);
        }
        const i = self.pixOffset(x, y);
        return Color.gray(self.pixels[@intCast(i)]);
    }
};

pub const CMYKImage = struct {
    pixels: []u8 = undefined,
    stride: usize = 0,
    rect: Rectangle = undefined,

    pub fn init(
        al: std.mem.Allocator,
        rect: Rectangle,
    ) !CMYKImage {
        const pixel_len = pixelBufferLength(4, rect, "CMYK");
        const pixels = try al.alloc(u8, pixel_len);
        const cmyk = CMYKImage{
            .pixels = pixels,
            .stride = @intCast(4 * rect.dX()),
            .rect = rect,
        };
        return cmyk;
    }

    pub fn subImage(self: *CMYKImage, rect: Rectangle) !?CMYKImage {
        if (rect.Intersect(self.rect)) |r| {
            const i: usize = @intCast(self.yOffset(r.min.x, r.min.y));

            return CMYKImage{
                .stride = self.stride,
                .rect = r,
                .pixels = self.pixels[i..],
            };
        } else {
            return null;
        }
    }

    // pixOffset returns the index of the first element of Pix that corresponds to
    // the pixel at (x, y).
    pub fn pixOffset(self: CMYKImage, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.stride);
        return (y - self.rect.min.y) * i + (x - self.rect.min.x) * 4;
    }

    pub fn bounds(self: CMYKImage) Rectangle {
        return self.rect;
    }
    pub fn at(self: CMYKImage, x: i32, y: i32) Color {
        return self.CMYKAt(x, y);
    }
    pub fn CMYKAt(self: CMYKImage, x: i32, y: i32) Color {
        // Check if the point (x, y) is within the rectangle.
        const pt = Point{ .x = x, .y = y };
        if (!pt.In(self.rect)) {
            return Color{ .CMYK = .{} };
        }
        const i: usize = @intCast(self.pixOffset(x, y));
        const s = self.pixels[i .. i + 4];
        return Color{ .CMYK = .{
            .c = s[0],
            .m = s[1],
            .y = s[2],
            .k = s[3],
        } };
    }
};

/// pixelBufferLength returns the length of the []u8 typed pixels slice field.
/// Conceptually, this is just (bpp * width * height),
/// but this function panics if at least one of those is negative or if the
/// computation would overflow the int type.
fn pixelBufferLength(bytes_per_pixel: usize, rect: Rectangle, image_type_name: []const u8) u32 {
    const total_length = mul3NonNeg(@intCast(bytes_per_pixel), rect.dX(), rect.dY());
    if (total_length < 0) {
        std.debug.panic("overflow in pixel buffer length calculation for image type '{s}'", .{image_type_name});
    }
    return @intCast(total_length);
}

/// mul3NonNeg returns (x * y * z), unless at least one argument is negative or
/// if the computation overflows the i32 type, in which case it returns -1.
fn mul3NonNeg(x: i32, y: i32, z: i32) i32 {
    if (x < 0 or y < 0 or z < 0) return -1;

    var hi: u64 = 0;
    var lo: u64 = 0;

    // Multiply x and y
    hi, lo = mul64(@intCast(x), @intCast(y));
    if (hi != 0) return -1;

    // Multiply the result with z
    hi, lo = mul64(lo, @intCast(z));
    if (hi != 0) return -1;

    return @intCast(lo);
}

pub fn mul64(x: u64, y: u64) struct { u64, u64 } {
    const mask32 = (1 << 32) - 1;

    const x0 = x & mask32;
    const x1 = x >> 32;
    const y0 = y & mask32;
    const y1 = y >> 32;

    const w0 = x0 * y0;
    const t = x1 * y0 + (w0 >> 32);
    const w1 = t & mask32;
    const w2 = t >> 32;

    const w1_with_x0_y1 = w1 + x0 * y1;
    const hi = x1 * y1 + w2 + (w1_with_x0_y1 >> 32);
    const lo = x * y;

    return .{ hi, lo };
}
// add2NonNeg returns (x + y), unless at least one argument is negative or if
// the computation overflows the i32 type, in which case it returns -1.
fn add2NonNeg(x: i32, y: i32) i32 {
    if (x < 0 or y < 0) return -1;

    const sum: i32 = x + y;
    if (sum < 0) return -1;

    return sum;
}
