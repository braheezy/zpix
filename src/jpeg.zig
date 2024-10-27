const std = @import("std");
const oneMB = 1.049E+6;
const decodeLimit = oneMB;

pub const JpegError = error{
    // SOI marker missing or incorrect
    InvalidSOIMarker,
    // EOI marker missing or incorrect
    InvalidEOIMarker,
    // Unexpected end of data in stream
    UnexpectedEndOfData,
    // General format issue
    InvalidJPEGFormat,
    // I/O error during validation
    IoError,
};

pub fn decodeFull(allocator: std.mem.Allocator, stream: std.fs.File.Reader) !void {
    try validateJpegStream(stream);
    _ = allocator;
    // const file_bytes = try stream.readAllAlloc(allocator, decodeLimit);
}

pub fn validateJpegStream(stream: std.fs.File.Reader) !void {
    // Buffer to read markers
    var buffer: [2]u8 = undefined;

    // Check for SOI marker (0xFFD8) at the start
    _ = try stream.readAtLeast(&buffer, 2);
    if (buffer[0] != 0xFF or buffer[1] != 0xD8) {
        return JpegError.InvalidSOIMarker;
    }

    // Reset stream to the start for further processing
    // try stream.seekTo(0);
}

pub const DecodedImage = struct {
    width: u16,
    height: u16,
};
