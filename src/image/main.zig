//! This file is a Zig translation of portions of the Go image package.
//! The core logic is the same, but instead of using interfaces, tagged unions
//! are used to represent different types of images and colors.
const std = @import("std");
const assert = std.debug.assert;

pub const Color = @import("color.zig").Color;
pub const Model = @import("color.zig").Model;
pub const Point = @import("geometry.zig").Point;
pub const Rectangle = @import("geometry.zig").Rectangle;

const image = @import("image.zig");
pub const Image = image.Image;
pub const Config = image.Config;
pub const GrayImage = image.GrayImage;
pub const RGBAImage = image.RGBAImage;
pub const YCbCrImage = image.YCbCrImage;
pub const CMYKImage = image.CMYKImage;
pub const YCbCrSubsample = image.YCbCrSubsample;

pub const util = @import("util.zig");
