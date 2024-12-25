const std = @import("std");
const assert = std.debug.assert;

pub const RGBA = struct {
    r: usize,
    g: usize,
    b: usize,
    a: usize,
};

pub const Config = struct {
    width: u32,
    height: u32,
    color_model: ?Model = null,
};

pub const YCbCrSubsample = enum {
    Ratio444,
    Ratio422,
    Ratio420,
    Ratio440,
    Ratio411,
    Ratio410,
};

pub const YCbCr = struct {
    y: []u8 = undefined,
    cb: []u8 = undefined,
    cr: []u8 = undefined,
    y_stride: usize = 0,
    c_stride: usize = 0,
    subsample_ratio: YCbCrSubsample,
    rect: Rectangle = undefined,

    pub fn init(
        al: std.mem.Allocator,
        rect: Rectangle,
        subsample_ratio: YCbCrSubsample,
    ) !*YCbCr {
        const w, const h, const cw, const ch = yCbCrSize(rect, subsample_ratio);

        // totalLength should be the same as i2, below, for a valid Rectangle r.
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
        var ycbcr = YCbCr{
            .y = pixels[0..i_0],
            .cb = pixels[i_0..i_1],
            .cr = pixels[i_1..i_2],
            .y_stride = @intCast(w),
            .c_stride = @intCast(cw),
            .subsample_ratio = subsample_ratio,
            .rect = rect,
        };
        return &ycbcr;
    }

    fn yCbCrSize(r: Rectangle, subsample_ratio: YCbCrSubsample) struct { i32, i32, i32, i32 } {
        const w = r.dX();
        const h = r.dY();
        var cw: i32 = 0;
        var ch: i32 = 0;

        switch (subsample_ratio) {
            .Ratio422 => {
                cw = @divExact((r.max.x + 1), 2) - @divExact(r.min.x, 2);
                ch = h;
            },
            .Ratio420 => {
                cw = @divExact((r.max.x + 1), 2) - @divExact(r.min.x, 2);
                ch = @divExact((r.max.y + 1), 2) - @divExact(r.min.y, 2);
            },
            .Ratio440 => {
                cw = w;
                ch = @divExact((r.max.y + 1), 2) - @divExact(r.min.y, 2);
            },
            .Ratio411 => {
                cw = @divExact((r.max.x + 3), 4) - @divExact(r.min.x, 4);
                ch = h;
            },
            .Ratio410 => {
                cw = @divExact((r.max.x + 3), 4) - @divExact(r.min.x, 4);
                ch = @divExact((r.max.y + 1), 2) - @divExact(r.min.y, 2);
            },
            else => { // Default to Ratio444
                cw = w;
                ch = h;
            },
        }

        return .{ w, h, cw, ch };
    }

    pub fn subImage(self: *YCbCr, rect: Rectangle) ?YCbCr {
        if (rect.Intersect(self.rect)) |r| {
            const yi: usize = @intCast(self.yOffset(r.min.x, r.min.y));
            const ci: usize = @intCast(self.cOffset(r.min.x, r.min.y));
            return .{
                .y = self.y[yi..],
                .cb = self.cb[ci..],
                .cr = self.cr[ci..],
                .y_stride = self.y_stride,
                .c_stride = self.c_stride,
                .subsample_ratio = self.subsample_ratio,
                .rect = r,
            };
        } else {
            return .{
                .subsample_ratio = self.subsample_ratio,
            };
        }
    }

    // YOffset returns the index of the first element of Y that corresponds to
    // the pixel at (x, y).
    fn yOffset(self: *YCbCr, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.y_stride);
        return (y - self.rect.min.y) * i + (x - self.rect.min.x);
    }

    // COffset returns the index of the first element of Cb or Cr that corresponds
    // to the pixel at (x, y).
    fn cOffset(self: *YCbCr, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.c_stride);
        return switch (self.subsample_ratio) {
            .Ratio422 => (y - self.rect.min.y) * i + (@divExact(x, 2) - @divExact(self.rect.min.x, 2)),
            .Ratio420 => (@divExact(y, 2) - @divExact(self.rect.min.y, 2)) * i + (@divExact(x, 2) - @divExact(self.rect.min.x, 2)),
            .Ratio440 => (@divExact(y, 2) - @divExact(self.rect.min.y, 2)) * i + (x - self.rect.min.x),
            .Ratio411 => (y - self.rect.min.y) * i + (@divExact(x, 4) - @divExact(self.rect.min.x, 4)),
            .Ratio410 => (@divExact(y, 2) - @divExact(self.rect.min.y, 2)) * i + (@divExact(x, 4) - @divExact(self.rect.min.x, 4)),
            // Default to 4:4:4 subsampling.
            else => (y - self.rect.min.y) * i + (x - self.rect.min.x),
        };
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

    pub fn subImage(self: *GrayImage, rect: Rectangle) ?GrayImage {
        if (rect.Intersect(self.rect)) |r| {
            const i: usize = @intCast(self.pixOffset(r.min.x, r.min.y));
            return .{
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
    pub fn pixOffset(self: *GrayImage, x: i32, y: i32) i32 {
        const i: i32 = @intCast(self.stride);
        return (y - self.rect.min.y) * i + (x - self.rect.min.x) * 1;
    }
};

fn pixelBufferLength(bytes_per_pixel: usize, rect: Rectangle, image_type_name: []const u8) u32 {
    const total_length = mul3NonNeg(@intCast(bytes_per_pixel), rect.dX(), rect.dY());
    if (total_length < 0) {
        std.debug.panic("overflow in pixel buffer length calculation for image type '{s}'", .{image_type_name});
    }
    return @intCast(total_length);
}

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

const Image = struct {
    ptr: *anyopaque,
    colorModelFn: *const fn (ptr: *anyopaque) Model,
    boundsFn: *const fn (ptr: *anyopaque) Rectangle,
    atFn: *const fn (ptr: *anyopaque, x: i32, y: i32) Color,
};

// #################################
// # Geometry
// #################################
// A Point is an X, Y coordinate pair. The axes increase right and down.
const Point = struct {
    x: i32,
    y: i32,
};

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

// add2NonNeg returns (x + y), unless at least one argument is negative or if
// the computation overflows the int type, in which case it returns -1.
fn add2NonNeg(x: i32, y: i32) i32 {
    if (x < 0 or y < 0) return -1;

    const sum: i32 = x + y;
    if (sum < 0) return -1;

    return sum;
}
// #################################
// # Color
// #################################
const Model = struct {
    ptr: *anyopaque,
    convertFn: *const fn (ptr: *anyopaque, c: Color) Color,

    pub fn init(
        pointer: anytype,
        comptime convertFn: fn (ptr: @TypeOf(pointer), c: Color) Color,
    ) Model {
        const Ptr = @TypeOf(pointer);
        assert(@typeInfo(Ptr) == .Pointer);
        assert(@typeInfo(Ptr).Pointer.size == .One);
        assert(@typeInfo(@typeInfo(Ptr).Pointer.child) == .Struct);

        const impl = struct {
            fn convert(ptr: *anyopaque, c: Color) Color {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return convertFn(self, c);
            }
        };

        return .{
            .ptr = pointer,
            .convertFn = impl.convert,
        };
    }

    pub fn convert(self: Model, c: Color) Color {
        return self.convertFn(self.ptr, c);
    }
};

const Color = struct {
    ptr: *anyopaque,
    rgbaPtr: *const fn (ptr: *anyopaque) RGBA,

    pub fn init(
        pointer: anytype,
        comptime rgbaConvertFn: fn (ptr: @TypeOf(pointer)) RGBA,
    ) Color {
        const Ptr = @TypeOf(pointer);
        assert(@typeInfo(Ptr) == .Pointer);
        assert(@typeInfo(Ptr).Pointer.size == .One);
        assert(@typeInfo(@typeInfo(Ptr).Pointer.child) == .Struct);

        const impl = struct {
            fn rgba(ptr: *anyopaque) RGBA {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return rgbaConvertFn(self);
            }
        };

        return .{
            .ptr = pointer,
            .rgbaPtr = impl.rgba,
        };
    }

    pub fn rgba(self: Color) RGBA {
        return self.convertFn(self.ptr);
    }
};

pub const GrayModel = struct {
    pub fn init() GrayModel {
        return GrayModel{};
    }

    pub fn convert(_: *const GrayModel, c: Color) Color {
        const rgba = c.rgbaPtr(c.ptr);
        const r = rgba.r;
        const g = rgba.g;
        const b = rgba.b;

        // Apply the grayscale formula (same coefficients as Go)
        const y = @as(u8, @intCast((19595 * r + 38470 * g + 7471 * b + (1 << 15)) >> 24));

        var gc = GrayColor{ .y = y };
        return gc.color();
    }

    pub fn model(self: *GrayModel) Model {
        return Model.init(self, convert);
    }
};

const GrayColor = struct {
    y: u8,

    pub fn color(self: *GrayColor) Color {
        return Color.init(self, rgba);
    }

    pub fn rgba(self: *GrayColor) RGBA {
        const y: usize = @as(usize, self.y);
        const expanded_y: usize = y | (y << 8); // Expand 8-bit value to 16-bit

        return RGBA{
            .r = expanded_y,
            .g = expanded_y,
            .b = expanded_y,
            .a = 0xFF,
        };
    }
};
