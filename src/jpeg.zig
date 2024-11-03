//! jpeg package provides a full JPEG decoder
const std = @import("std");

const oneMB = 1.049E+6;
const decodeLimit = oneMB;

const dbg = std.debug.print;

fn println(comptime fmt: []const u8) void {
    std.debug.print("{s}\n", .{fmt});
}

pub const FileError = error{
    /// SOI marker missing or incorrect
    InvalidSOIMarker,
    /// EOI marker missing or incorrect
    InvalidEOIMarker,
    /// Unexpected end of data in stream
    UnexpectedEndOfData,
    /// General format issue
    InvalidJPEGFormat,
};

pub const DecoderError = error{
    InitializationFailed,
    InvalidState,
    UnsupportedFeature,
};

/// [Table B.1] Marker code assignments
pub const Marker = enum(u16) {
    SOF0 = 0xFFC0,
    SOF1 = 0xFFC1,
    SOF2 = 0xFFC2,
    SOI = 0xFFD8,
    EOI = 0xFFD9,
    DQT = 0xFFDB,
    _,
};

pub const Decoder = struct {
    stream: Stream,

    /// Processes JPEG markers in the stream and decodes the image data.
    /// Reads markers sequentially and handles them according to their type.
    pub fn processMarkers(self: *Decoder) !DecodedImage {
        while (true) {
            const marker = try self.stream.readMarker();
            dbg("marker: {x}\n", .{marker});
            switch (marker) {
                .SOI => continue,
                .SOF0 => try self.decodeHeader(),
                else => try self.stream.readLengthAndSkip(),
            }
        }
    }

    /// Parses the stream to find the SOF marker and ensuing frame header info
    fn decodeHeader(self: *Decoder) !void {
        // [B.2.2] Frame header syntax
        // | SOFn | Lf | P | Y | X | Nf | Component-specification parameters |
        const frame_header_length = try self.stream.readU16();
        dbg("frame header length: {d}\n", .{frame_header_length});
    }
};

pub const Stream = struct {
    inner: std.fs.File.SeekableStream,

    pub fn readByte(self: *Stream) !u8 {
        return try self.inner.context.reader().readByte();
    }

    pub fn readU16(self: *Stream) !u16 {
        return try self.inner.context.reader().readInt(u16, .big);
    }

    pub fn skipBytes(self: *Stream, num_bytes: usize) !void {
        return try self.inner.context.reader().skipBytes(num_bytes, .{});
    }

    // Reads the next two bytes to find a JPEG marker
    pub fn readMarker(self: *Stream) !Marker {
        const byte1 = try self.readByte();
        const byte2 = try self.readByte();
        // [B.1.1.3] all markers start with FF
        if (byte1 != 0xFF) {
            return FileError.InvalidJPEGFormat;
        }
        return @enumFromInt(@as(u16, byte1) << 8 | @as(u16, byte2));
    }

    pub fn readLengthAndSkip(self: *Stream) !void {
        // [B.1.1.4] The first parameter in a marker segment is the two-byte length parameter.
        // This length parameter encodes the number of bytes in the marker segment, including the length
        // parameter and excluding the two-byte marker.
        const length = try self.readU16();
        try self.skipBytes(length - 2);
    }
};

// decodeFull decodes the provided reader stream into a DecodedImage.
pub fn decode(allocator: std.mem.Allocator, jpeg_file: std.fs.File) !DecodedImage {
    _ = allocator;
    try validateFile(jpeg_file);

    const stream = Stream{ .inner = jpeg_file.seekableStream() };
    var decoder = Decoder{
        .stream = stream,
    };

    return decoder.processMarkers();
}

// validateFile returns an Error is the provided file reader does not reference a valid JPEG file.
pub fn validateFile(file: std.fs.File) !void {
    // Buffer to read markers
    var buffer: [2]u8 = undefined;

    // Check for SOI marker (0xFFD8) at the start of the file
    try file.seekTo(0);
    _ = try file.readAll(&buffer);
    if (buffer[0] != 0xFF or buffer[1] != 0xD8) {
        return FileError.InvalidSOIMarker;
    }

    // Check for EOI marker (0xFFD9) at the end of the file
    try file.seekFromEnd(-2);
    _ = try file.readAll(&buffer);
    if (buffer[0] != 0xFF or buffer[1] != 0xD9) {
        return FileError.InvalidEOIMarker;
    }

    // restore file reader position
    try file.seekTo(0);
}

pub const DecodedImage = struct {
    width: u16,
    height: u16,
};
