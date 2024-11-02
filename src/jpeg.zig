const std = @import("std");

const baseline = @import("baseline.zig");
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
    // Unsupported operation
    NotSupported,
};

// [Table B.1] Marker code assignments
pub const Marker = enum(u16) {
    SOF0 = 0xFFC0,
    SOF1 = 0xFFC1,
    SOF2 = 0xFFC2,
    SOI = 0xFFD8,
    EOI = 0xFFD9,
    _,
};

// decodeFull decodes the provided reader stream into a DecodedImage. The entire input is read into memory.
pub fn decodeFull(allocator: std.mem.Allocator, jpeg_file: std.fs.File) !DecodedImage {
    var decoder = try selectDecoder(jpeg_file.reader());
    std.debug.print("got decoder: {any}\n", .{decoder});
    return decoder.decode(allocator, jpeg_file);
}

// Define a generalized interface for all decoders
pub const DecoderAPI = struct {
    decode: *const fn (allocator: std.mem.Allocator, jpeg_file: std.fs.File) DecodedImage,
};

// main struct to hold the decoder process
const Decoder = struct {
    // To support different decoder implementations based on jpeg type
    decoder_api: *DecoderAPI,

    fn decode(self: *Decoder, allocator: std.mem.Allocator, jpeg_file: std.fs.File) DecodedImage {
        return self.decoder_api.decode(allocator, jpeg_file);
    }
};

// selectDecoder returns the appropriate decoder based on JPEG type
fn selectDecoder(stream: std.fs.File.Reader) !Decoder {
    // Read the first SOF marker to determine the JPEG type
    while (true) {
        const marker = try readMarker(stream);
        std.debug.print("read marker: {x}\n", .{marker});
        // [Table B.1]
        switch (marker) {
            .SOI => continue,
            // Baseline and Extended Sequential
            .SOF0, .SOF1 => {
                return Decoder{
                    .decoder_api = &baseline.BaselineAPI,
                };
            },
            // Progressive JPEG
            .SOF2 => {
                // TODO: Support progressive jpeg decoding
                return JpegError.NotSupported;
            },
            // Skip any other marker until we reach an SOF marker or end of data
            else => {
                // [B.1.1.4] The first parameter in a marker segment is the two-byte length parameter.
                // This length parameter encodes the number of bytes in the marker segment, including the length
                // parameter and excluding the two-byte marker.
                const length = try stream.readInt(u16, .big);
                try stream.skipBytes(length - 2, .{});
            },
        }
    }
}

// validateFile returns a JpegError is the provided file reader does not reference a valid JPEG file.
pub fn validateFile(file: std.fs.File) !void {
    // Buffer to read markers
    var buffer: [2]u8 = undefined;

    // Check for SOI marker (0xFFD8) at the start of the file
    try file.seekTo(0);
    _ = try file.readAll(&buffer);
    std.log.debug("SOI marker: {x}", .{buffer});
    if (buffer[0] != 0xFF or buffer[1] != 0xD8) {
        return JpegError.InvalidSOIMarker;
    }

    // Check for EOI marker (0xFFD9) at the end of the file
    try file.seekFromEnd(-2);
    _ = try file.readAll(&buffer);
    std.log.debug("EOI marker: {x}", .{buffer});
    if (buffer[0] != 0xFF or buffer[1] != 0xD9) {
        return JpegError.InvalidEOIMarker;
    }

    // restore file reader position
    try file.seekTo(0);
}

// Reads the next two bytes to find a JPEG marker
fn readMarker(stream: std.fs.File.Reader) !Marker {
    const byte1 = try stream.readByte();
    const byte2 = try stream.readByte();
    // [B.1.1.3] all markers start with FF
    if (byte1 != 0xFF) return JpegError.InvalidJPEGFormat;
    return @enumFromInt(@as(u16, byte1) << 8 | @as(u16, byte2));
}

pub const DecodedImage = struct {
    width: u16,
    height: u16,
};
