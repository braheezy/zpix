//! This file is a Zig translation of portions of the Go image package.
//! The core logic is the same, but instead of using interfaces, tagged unions
//! are used to represent different types of images and colors.
const std = @import("std");
const assert = std.debug.assert;

/// Config holds an image's color model and dimensions.
pub const Config = struct {
    width: u32,
    height: u32,
    color_model: Model,
};

/// Model can convert any [Color] to one from its own color model. The conversion
/// may be lossy.
const Model = union(enum) {
    RGB: void,
    YCbCr: void,
    RGBA: void,
    Gray: void,

    pub fn convert(self: Model, c: Color) Color {
        return switch (self) {
            .RGB => c, // No conversion needed for RGB.
            .YCbCr => {
                const yuv = rgbToYCbCr(c.r, c.g, c.b);
                var ycbcr = YCbCrColor{
                    .y = yuv[0],
                    .cb = yuv[1],
                    .cr = yuv[2],
                };
                return ycbcr.color();
            },
            .RGBA => {
                const rgba = c.toRGBA();
                var rbga = RGBAColor{
                    .r = rgba[0],
                    .g = rgba[1],
                    .b = rgba[2],
                    .a = rgba[3],
                };
                return rbga.color();
            },
            .Gray => {
                const rgba = c.toRGBA();
                // Apply the grayscale formula (same coefficients as Go)
                const y = @as(u8, @intCast((19595 * rgba[0] + 38470 * rgba[1] + 7471 * rgba[2] + (1 << 15)) >> 24));
                var gc = GrayColor{ .y = y };
                return gc.color();
            },
        };
    }
};

/// Color can convert itself to alpha-premultiplied 16-bits per channel RGBA.
/// The conversion may be lossy.
const Color = union(enum) {
    RGB: struct { r: u8, g: u8, b: u8 },
    RGBA: struct { r: u8, g: u8, b: u8, a: u8 },
    YCbCr: struct { y: u8, cb: u8, cr: u8 },

    // RGBA returns the alpha-premultiplied red, green, blue and alpha values
    // for the color. Each value ranges within [0, 0xffff], but is represented
    // by a uint32 so that multiplying by a blend factor up to 0xffff will not
    // overflow.
    //
    // An alpha-premultiplied color component c has been scaled by alpha (a),
    // so has valid values 0 <= c <= a.
    pub fn toRGBA(self: Color) struct { u8, u8, u8, u8 } {
        return switch (self) {
            .RGB => |c| .{ c.r, c.g, c.b, 255 },
            .RGBA => |c| .{ c.r, c.g, c.b, c.a },
            .YCbCr => |c| {
                // This code returns values in the range [0, 0xffff] instead of [0, 0xff]. There is a
                // subtle difference between doing this and having YCbCr satisfy the Color
                // interface by first converting to an RGBA. The latter loses some
                // information by going to and from 8 bits per channel.
                //
                // For example, this code:
                //	const y, cb, cr = 0x7f, 0x7f, 0x7f
                //	r, g, b := color.YCbCrToRGB(y, cb, cr)
                //	r0, g0, b0, _ := color.YCbCr{y, cb, cr}.RGBA()
                //	r1, g1, b1, _ := color.RGBA{r, g, b, 0xff}.RGBA()
                //	fmt.Printf("0x%04x 0x%04x 0x%04x\n", r0, g0, b0)
                //	fmt.Printf("0x%04x 0x%04x 0x%04x\n", r1, g1, b1)
                // prints:
                //	0x7e18 0x808d 0x7db9
                //	0x7e7e 0x8080 0x7d7d
                const yy1 = @as(i32, c.y) * 0x10101;
                const cb1 = @as(i32, c.cb) - 128;
                const cr1 = @as(i32, c.cr) - 128;

                var r = yy1 + 91881 * cr1;
                r = if ((@as(u32, @bitCast(r)) & 0xff000000) == 0) r >> 8 else ~(@as(i32, r) >> 31) & 0xffff;

                var g = yy1 - 22554 * cb1 - 46802 * cr1;
                g = if ((@as(u32, @bitCast(g)) & 0xff000000) == 0) g >> 8 else ~(@as(i32, g) >> 31) & 0xffff;

                var b = yy1 + 116130 * cb1;
                b = if ((@as(u32, @bitCast(b)) & 0xff000000) == 0) b >> 8 else ~(@as(i32, b) >> 31) & 0xffff;

                return .{
                    @as(u8, @intCast(r >> 8)),
                    @as(u8, @intCast(g >> 8)),
                    @as(u8, @intCast(b >> 8)),
                    255,
                };
            },
        };
    }
};

/// Image is a finite rectangular grid of [Color] values taken from a color
/// model.
pub const Image = union(enum) {
    Gray: *GrayImage,
    YCbCr: YCbCrImage,
    RGBA: *RGBAImage,

    // bounds returns the domain for which At can return non-zero color.
    // The bounds do not necessarily contain the point (0, 0).
    pub fn bounds(self: Image) Rectangle {
        return switch (self) {
            .Gray => |img| img.bounds(),
            .YCbCr => |img| img.bounds(),
            .RGBA => |img| img.bounds(),
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
        };
    }

    pub fn free(self: Image, al: std.mem.Allocator) void {
        switch (self) {
            .Gray => {}, // Gray doesn't own memory explicitly
            .YCbCr => |img| {
                al.free(img.pixels);
            },
            .RGBA => |img| al.free(img.pixels),
        }
    }
};

pub const RGBAColor = struct {
    r: usize = 0,
    g: usize = 0,
    b: usize = 0,
    a: usize = 0,

    pub fn color(self: *RGBAColor) Color {
        return Color{ .RGBA = .{
            .r = self.r,
            .g = self.g,
            .b = self.b,
            .a = self.a,
        } };
    }
};

pub const RGBAImage = struct {
    pixels: []u8 = undefined,
    stride: usize = 0,
    rect: Rectangle = undefined,

    pub fn init(
        al: std.mem.Allocator,
        rect: Rectangle,
    ) !*RGBAImage {
        const pixel_len = pixelBufferLength(4, rect, "RGBA");
        const pixels = try al.alloc(u8, pixel_len);
        var rgba = RGBAImage{
            .pixels = pixels,
            .stride = @intCast(rect.dX() * 4),
            .rect = rect,
        };
        return &rgba;
    }

    pub fn subImage(self: *RGBAImage, al: std.mem.Allocator, rect: Rectangle) !?*RGBAImage {
        if (rect.Intersect(self.rect)) |r| {
            const i: usize = @intCast(self.pixOffset(r.min.x, r.min.y));
            const sub_img = try al.create(RGBAImage);
            sub_img.* = .{
                .pixels = self.pixels[i..],
                .stride = self.stride,
                .rect = r,
            };
            return sub_img;
        } else {
            return null;
        }
    }

    // PixOffset returns the index of the first element of Pix that corresponds to
    // the pixel at (x, y).
    pub fn pixOffset(self: *RGBAImage, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.stride);
        return (y - self.rect.min.y) * i + (x - self.rect.min.x) * 4;
    }

    pub fn bounds(self: *RGBAImage) Rectangle {
        return self.rect;
    }

    pub fn at(self: *RGBAImage, x: i32, y: i32) Color {
        var rgba = self.rgbaAt(x, y);
        return rgba.color();
    }

    pub fn rgbaAt(self: *RGBAImage, x: i32, y: i32) RGBAColor {
        const pt = Point{ .x = x, .y = y };
        if (!pt.In(self.rect)) {
            const rgba = RGBAColor{};
            return rgba;
        }
        const i: usize = @intCast(self.pixOffset(x, y));
        const s = self.pixels[i .. i + 4];
        return RGBAColor{
            .r = @as(u8, s[0]),
            .g = @as(u8, s[1]),
            .b = @as(u8, s[2]),
            .a = @as(u8, s[3]),
        };
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

    pub fn subImage(self: *YCbCrImage, rect: Rectangle) !?YCbCrImage {
        if (rect.Intersect(self.rect)) |r| {
            const yi: usize = @intCast(self.yOffset(r.min.x, r.min.y));
            const ci: usize = @intCast(self.cOffset(r.min.x, r.min.y));

            return YCbCrImage{
                .y = self.y[yi..],
                .cb = self.cb[ci..],
                .cr = self.cr[ci..],
                .y_stride = self.y_stride,
                .c_stride = self.c_stride,
                .subsample_ratio = self.subsample_ratio,
                .rect = r,
                .pixels = self.pixels,
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
    pub fn at(self: *YCbCrImage, x: i32, y: i32) Color {
        std.debug.print("at YCbCrImage\n", .{});
        var ycbcr = self.YCbCrAt(x, y);
        std.debug.print("here?\n", .{});
        return ycbcr.color();
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

const YCbCrColor = struct {
    y: u8 = 0,
    cb: u8 = 0,
    cr: u8 = 0,

    pub fn color(self: *YCbCrColor) Color {
        return Color{ .YCbCr = .{
            .y = self.y,
            .cb = self.cb,
            .cr = self.cr,
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
    ) !*GrayImage {
        const pixel_len = pixelBufferLength(1, rect, "Gray");
        const pixels = try al.alloc(u8, pixel_len);
        var gray = GrayImage{
            .pixels = pixels,
            .stride = @intCast(rect.dX()),
            .rect = rect,
        };
        return &gray;
    }

    pub fn subImage(self: *GrayImage, al: std.mem.Allocator, rect: Rectangle) !?*GrayImage {
        if (rect.Intersect(self.rect)) |r| {
            const i: usize = @intCast(self.pixOffset(r.min.x, r.min.y));
            const sub_img = try al.create(GrayImage);
            sub_img.* = .{
                .pixels = self.pixels[i..],
                .stride = self.stride,
                .rect = r,
            };
            return sub_img;
        } else {
            return null;
        }
    }
    // PixOffset returns the index of the first element of Pix that corresponds to
    // the pixel at (x, y).
    pub fn pixOffset(self: *GrayImage, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.stride);
        return (y - self.rect.min.y) * i + (x - self.rect.min.x) * 1;
    }

    pub fn bounds(self: *GrayImage) Rectangle {
        return self.rect;
    }

    pub fn at(self: *GrayImage, x: i32, y: i32) Color {
        return self.grayAt(x, y);
    }

    fn grayAt(self: *GrayImage, x: i32, y: i32) Color {
        const pt = Point{ .x = x, .y = y };
        if (!pt.In(self.rect)) {
            var gc = GrayColor{};
            return gc.color();
        }
        const i = self.pixOffset(x, y);
        var gc = GrayColor{ .y = self.pixels[@intCast(i)] };
        return gc.color();
    }
};

const GrayColor = struct {
    y: u8 = 0,

    pub fn color(self: *GrayColor) Color {
        return Color{ .RGB = .{
            .r = self.y,
            .g = self.y,
            .b = self.y,
        } };
    }

    pub fn rgba(self: *GrayColor) struct { u32, u32, u32, u32 } {
        const y: usize = @as(usize, self.y);
        const expanded_y: u32 = @intCast(y | (y << 8)); // Expand 8-bit value to 16-bit

        return .{
            expanded_y,
            expanded_y,
            expanded_y,
            0xFF,
        };
    }
};

/// rgbToYCbCr converts an RGB triple to a Y'CbCr triple.
pub fn rgbToYCbCr(r: u8, g: u8, b: u8) struct { u8, u8, u8 } {
    // The JFIF specification says:
    //	Y' =  0.2990*R + 0.5870*G + 0.1140*B
    //	Cb = -0.1687*R - 0.3313*G + 0.5000*B + 128
    //	Cr =  0.5000*R - 0.4187*G - 0.0813*B + 128
    // https://www.w3.org/Graphics/JPEG/jfif3.pdf says Y but means Y'.

    const r1: i32 = @intCast(r);
    const g1: i32 = @intCast(g);
    const b1: i32 = @intCast(b);

    // yy is in range [0,0xff].
    //
    // Note that 19595 + 38470 + 7471 equals 65536.
    const yy = (19595 * r1 + 38470 * g1 + 7471 * b1 + 1 << 15) >> 16;

    // The bit twiddling below is equivalent to
    //
    // cb := (-11056*r1 - 21712*g1 + 32768*b1 + 257<<15) >> 16
    // if cb < 0 {
    //     cb = 0
    // } else if cb > 0xff {
    //     cb = ^int32(0)
    // }
    //
    // but uses fewer branches and is faster.
    // Note that the uint8 type conversion in the return
    // statement will convert ^int32(0) to 0xff.
    // The code below to compute cr uses a similar pattern.
    //
    // Note that -11056 - 21712 + 32768 equals 0.
    var cb = -11056 * r1 - 21712 * g1 + 32768 * b1 + 257 << 15;
    if (@as(i64, cb) & 0xff000000 == 0) {
        cb >>= 16;
    } else {
        cb = ~(cb >> 31);
    }

    // Note that 32768 - 27440 - 5328 equals 0.
    var cr = 32768 * r1 - 27440 * g1 - 5328 * b1 + 257 << 15;
    if (@as(i64, cr) & 0xff000000 == 0) {
        cr >>= 16;
    } else {
        cr = ~(cr >> 31);
    }

    return .{ @intCast(yy), @intCast(cb), @intCast(cr) };
}

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

/// A Point is an X, Y coordinate pair. The axes increase right and down.
pub const Point = struct {
    x: i32,
    y: i32,

    pub fn In(self: Point, r: Rectangle) bool {
        return self.x >= r.min.x and self.x < r.max.x and self.y >= r.min.y and self.y < r.max.y;
    }
};

/// A Rectangle contains the points with Min.X <= X < Max.X, Min.Y <= Y < Max.Y.
/// It is well-formed if Min.X <= Max.X and likewise for Y. Points are always
/// well-formed. A rectangle's methods always return well-formed outputs for
/// well-formed inputs.
pub const Rectangle = struct {
    min: Point,
    max: Point,

    pub fn dX(self: Rectangle) i32 {
        return self.max.x - self.min.x;
    }
    pub fn dY(self: Rectangle) i32 {
        return self.max.y - self.min.y;
    }
    pub fn init(x0: i32, y0: i32, x1: i32, y1: i32) Rectangle {
        const x_min = if (x0 > x1) x1 else x0;
        const x_max = if (x0 > x1) x0 else x1;
        const y_min = if (y0 > y1) y1 else y0;
        const y_max = if (y0 > y1) y0 else y1;

        return Rectangle{
            .min = .{ .x = x_min, .y = y_min },
            .max = .{ .x = x_max, .y = y_max },
        };
    }
    // Intersect returns the largest rectangle contained by both self and other. If the
    // two rectangles do not overlap then null will be returned.
    pub fn Intersect(self: Rectangle, other: Rectangle) ?Rectangle {
        const x0 = if (self.min.x > other.min.x) self.min.x else other.min.x;
        const y0 = if (self.min.y > other.min.y) self.min.y else other.min.y;
        const x1 = if (self.max.x < other.max.x) self.max.x else other.max.x;
        const y1 = if (self.max.y < other.max.y) self.max.y else other.max.y;

        if (x0 >= x1 or y0 >= y1) return null;

        return Rectangle.init(x0, y0, x1, y1);
    }
};
