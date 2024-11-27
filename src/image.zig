const std = @import("std");
const assert = std.debug.assert;

const Config = struct {
    width: u32,
    height: u32,
    color_model: Model,
};

const Gray = struct {
    pixels: []u8,
    stride: usize,
    rect: Rectangle,
};

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

const Rectangle = struct {
    min: Point,
    max: Point,
};

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
        assert(@typeInfo(Ptr) == .pointer);
        assert(@typeInfo(Ptr).Pointer.size == .One);
        assert(@typeInfo(@typeInfo(Ptr).Pointer.child) == .@"struct");

        const impl = struct {
            fn convert(ptr: *anyopaque, c: Color) Color {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                convertFn(self, c);
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
    rgbaPtr: fn (ptr: *anyopaque) .{ u8, u8, u8, u8 },
};
