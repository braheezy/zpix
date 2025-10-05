//! Zpix — core image‑decoding building blocks

/// Color utilities
pub const color = @import("color");

/// High‑level image API
pub const image = @import("image");

/// JPEG support
pub const jpeg = @import("jpeg");

/// PNG support
pub const png = @import("png");

/// QOI (Quite OK Image) support
pub const qoi = @import("qoi");

/// BMP support
pub const bmp = @import("bmp");

const std = @import("std");

/// Try to decode an image from a file path, probing supported formats.
pub fn fromFilePath(allocator: std.mem.Allocator, path: []const u8) !image.Image {
    // Probe in order: PNG, JPEG, QOI, BMP
    if (png.probePath(path) catch false) return try png.load(allocator, path);
    if (jpeg.probePath(path) catch false) return try jpeg.load(allocator, path);
    if (qoi.probePath(path) catch false) return try qoi.load(allocator, path);
    if (bmp.probePath(path) catch false) return try bmp.load(allocator, path);
    return error.UnknownImageFormat;
}

/// Try to decode an image from a memory buffer, probing supported formats.
pub fn fromBuffer(allocator: std.mem.Allocator, buffer: []const u8) !image.Image {
    if (png.probeBuffer(buffer)) return try png.loadFromBuffer(allocator, buffer);
    if (jpeg.probeBuffer(buffer)) return try jpeg.loadFromBuffer(allocator, buffer);
    if (qoi.probeBuffer(buffer)) return try qoi.loadFromBuffer(allocator, buffer);
    if (bmp.probeBuffer(buffer)) return try bmp.loadFromBuffer(allocator, buffer);
    return error.UnknownImageFormat;
}
