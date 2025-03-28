const std = @import("std");
const image = @import("image");
const InverseFilterTable = @import("filter.zig").InverseFilterTable;

const png_header = "\x89PNG\r\n\x1a\n";

// Constants for PNG chunk structure
const chunk_header_size = 8; // 4 bytes length + 4 bytes type
const chunk_crc_size = 4;
const chunk_base_size = chunk_header_size + chunk_crc_size;

// Custom reader for IDAT chunks
const IdatReader = struct {
    decoder: *Decoder,
    idat_length: u32 = 0,

    const Reader = std.io.Reader(*IdatReader, anyerror, read);

    fn reader(self: *IdatReader) Reader {
        return .{ .context = self };
    }

    fn read(self: *IdatReader, buffer: []u8) !usize {
        if (buffer.len == 0) return 0;

        // Keep reading until we have IDAT data
        while (self.idat_length == 0) {
            // Verify checksum of previous chunk if any
            try self.decoder.verifyChecksum();

            // Read next chunk header
            var tmp: [8]u8 = undefined;
            try self.decoder.r.readNoEof(&tmp);

            // Get chunk length and type
            self.idat_length = std.mem.readInt(u32, tmp[0..4], .big);

            // Verify it's an IDAT chunk
            if (!std.mem.eql(u8, tmp[4..8], "IDAT")) {
                return error.InvalidPngData;
            }

            // Reset CRC and update with chunk type
            self.decoder.crc.update(tmp[4..8]);
        }

        // Check for length overflow
        if (self.idat_length > std.math.maxInt(i32)) {
            return error.Overflow;
        }

        // Read the actual data
        const to_read = @min(buffer.len, self.idat_length);
        const n = try self.decoder.r.read(buffer[0..to_read]);

        // Update CRC and remaining length
        self.decoder.crc.update(buffer[0..n]);
        self.idat_length -= @intCast(n);

        return n;
    }
};

// Decoding stage.
// The PNG specification says that the IHDR, PLTE (if present), tRNS (if
// present), IDAT and IEND chunks must appear in that order. There may be
// multiple IDAT chunks, and IDAT chunks must be sequential (i.e. they may not
// have any other chunks between them).
// https://www.w3.org/TR/PNG/#5ChunkOrdering
const Stage = enum(u8) {
    start,
    seen_ihdr,
    seen_plte,
    seen_trns,
    seen_idat,
    seen_iend,
};

const DecoderFlags = packed struct {
    read_ihdr: bool = false,
    read_cgbi: bool = false,
    read_actl: bool = false,
    read_plte: bool = false,
    read_trns: bool = false,
    read_fctl: bool = false,
    reserved_6: bool = false,
    reserved_7: bool = false,
    has_color_key: bool = false,
    first_frame_is_default: bool = false,
    // pad to 32 bits
    padding: u22 = 0,
};

const Interlace = enum {
    none,
    adam7,
};

const ColorType = enum(u8) {
    grayscale = 0,
    truecolor = 2,
    paletted = 3,
    grayscale_alpha = 4,
    truecolor_alpha = 6,

    pub fn fromInt(val: u8) !ColorType {
        return switch (val) {
            0 => .grayscale,
            2 => .truecolor,
            3 => .paletted,
            4 => .grayscale_alpha,
            6 => .truecolor_alpha,
            else => error.InvalidColorType,
        };
    }
};

const ColorBitDepth = enum {
    g1, // grayscale, 1 bit
    g2, // grayscale, 2 bits
    g4, // grayscale, 4 bits
    g8, // grayscale, 8 bits
    ga8, // grayscale+alpha, 8 bits
    tc8, // truecolor, 8 bits
    p1, // paletted, 1 bit
    p2, // paletted, 2 bits
    p4, // paletted, 4 bits
    p8, // paletted, 8 bits
    tca8, // truecolor+alpha, 8 bits
    g16, // grayscale, 16 bits
    ga16, // grayscale+alpha, 16 bits
    tc16, // truecolor, 16 bits
    tca16, // truecolor+alpha, 16 bits

    pub fn bytesPerPixel(self: ColorBitDepth) u8 {
        return switch (self) {
            .g1, .g2, .g4, .g8, .p1, .p2, .p4, .p8 => 1,
            .ga8 => 2,
            .tc8 => 3,
            .tca8 => 4,
            .g16 => 2,
            .ga16 => 4,
            .tc16 => 6,
            .tca16 => 8,
        };
    }
};

const Decoder = @This();

// memory allocator
al: std.mem.Allocator,
// reader into provided PNG data
r: std.io.AnyReader,
img: image.Image = undefined,
width: u32 = 0,
height: u32 = 0,
depth: u8 = 0,
color_type: ColorType = undefined,
color_depth: ColorBitDepth = undefined,
crc: std.hash.Crc32,
stage: Stage = .start,
palette: []image.Color = undefined,
idat_length: u32 = 0,
filters_table: InverseFilterTable = undefined,
scratch: [3 * 256]u8 = [_]u8{0} ** (3 * 256),

pub fn decode(al: std.mem.Allocator, r: std.io.AnyReader) !image.Image {
    var d = Decoder{
        .al = al,
        .r = r,
        .crc = std.hash.Crc32.init(),
        .filters_table = InverseFilterTable.init(),
    };

    try d.checkHeader();

    while (d.stage != .seen_iend) {
        try d.parseChunk();
    }

    return d.img;
}

fn checkHeader(self: *Decoder) !void {
    try self.r.readNoEof(self.scratch[0..png_header.len]);

    if (!std.mem.eql(u8, self.scratch[0..png_header.len], png_header)) {
        return error.InvalidPngHeader;
    }
}

fn parseChunk(self: *Decoder) !void {
    const chunk_header = try self.readChunkHeader();
    self.crc.update(&chunk_header.type_bytes);
    switch (chunk_header.chunk_type) {
        .ihdr => {
            if (self.stage != .start) {
                return error.ChunkOrderError;
            }
            self.stage = .seen_ihdr;
            return try self.parseIhdr(chunk_header.length);
        },
        .idat => {
            const stage_int = @intFromEnum(self.stage);
            const seen_ihdr = @intFromEnum(Stage.seen_ihdr);
            const seen_idat = @intFromEnum(Stage.seen_idat);
            if (stage_int < seen_ihdr or
                stage_int > seen_idat or
                (stage_int == seen_ihdr and
                    colorDepthPaletted(self.color_depth)))
            {
                return error.ChunkOrderError;
            } else if (self.stage == .seen_idat) {
                // Ignore trailing zero-length or garbage IDAT chunks.
                //
                // This does not affect valid PNG images that contain multiple IDAT
                // chunks, since the first call to parseIDAT below will consume all
                // consecutive IDAT chunks required for decoding the image.
            } else {
                // Handle first IDAT chunk
                self.stage = .seen_idat;
                return try self.parseIdat(chunk_header.length);
            }
        },
        else => {
            std.debug.print("not implemented: {s}\n", .{@tagName(chunk_header.chunk_type)});
            return error.NotImplementedYet;
        },
    }
}

fn parseIhdr(self: *Decoder, length: u32) !void {
    if (length != 13) {
        return error.InvalidIHDRLength;
    }

    const bytes = try self.r.readBytesNoEof(13);
    self.crc.update(&bytes);
    if (bytes[10] != 0) {
        return error.UnsupportedCompressionMethod;
    }
    if (bytes[11] != 0) {
        return error.UnsupportedFilterMethod;
    }
    const interlace: Interlace = @enumFromInt(bytes[12]);
    if (interlace != .none and interlace != .adam7) {
        return error.UnsupportedInterlaceMethod;
    }

    const width = std.mem.readInt(u32, bytes[0..4], .big);
    const height = std.mem.readInt(u32, bytes[4..8], .big);
    if (width <= 0 or height <= 0) {
        return error.InvalidDimension;
    }
    const num_pixels, const overflow = @mulWithOverflow(width, height);
    if (overflow == 1) {
        return error.DimensionOverflow;
    }
    // There can be up to 8 bytes per pixel, for 16 bits per channel RGBA.
    if (num_pixels != (num_pixels * 8) / 8) {
        return error.DimensionOverflow;
    }

    self.depth = bytes[8];
    self.color_type = try ColorType.fromInt(bytes[9]);
    self.width = width;
    self.height = height;

    self.color_depth = try switch (self.depth) {
        1 => switch (self.color_type) {
            .grayscale => .g1,
            .paletted => .p1,
            else => error.InvalidColorTypeDepthCombo,
        },
        2 => switch (self.color_type) {
            .grayscale => .g2,
            .paletted => .p2,
            else => error.InvalidColorTypeDepthCombo,
        },
        4 => switch (self.color_type) {
            .grayscale => .g4,
            .paletted => .p4,
            else => error.InvalidColorTypeDepthCombo,
        },
        8 => switch (self.color_type) {
            .grayscale => .g8,
            .truecolor => .tc8,
            .paletted => .p8,
            .grayscale_alpha => .ga8,
            .truecolor_alpha => .tca8,
        },
        16 => switch (self.color_type) {
            .grayscale => .g16,
            .truecolor => .tc16,
            .grayscale_alpha => .ga16,
            .truecolor_alpha => .tca16,
            else => error.InvalidColorTypeDepthCombo,
        },
        else => error.UnsupportedBitDepth,
    };

    std.debug.print("ihdr: {d}x{d} {s} {s}\n", .{ self.width, self.height, @tagName(self.color_type), @tagName(self.color_depth) });
    return try self.verifyChecksum();
}

fn parseIdat(self: *Decoder, length: u32) !void {
    self.idat_length = length;
    _ = try self.decodeIdat();
}

// decode decodes the IDAT data into an image.
fn decodeIdat(self: *Decoder) !image.Image {
    // Create our IDAT reader
    var idat_reader = IdatReader{ .decoder = self };

    // Create the zlib decompressor with our custom reader
    var decompressed = std.compress.zlib.decompressor(idat_reader.reader());
    const decompressed_reader = decompressed.reader();

    return try self.readImagePass(decompressed_reader, 0, false);
}

// readImagePass reads a single image pass, sized according to the pass number.
fn readImagePass(
    self: *Decoder,
    reader: anytype,
    pass: u8,
    allocate_only: bool,
) !image.Image {
    _ = pass;
    _ = allocate_only;
    var bits_per_pixel: u8 = 0;
    var img: image.Image = undefined;
    switch (self.color_depth) {
        .tc8 => {
            bits_per_pixel = 24;
            var rgba = try image.RGBAImage.init(self.al, .{
                .min = .{ .x = 0, .y = 0 },
                .max = .{ .x = @intCast(self.width), .y = @intCast(self.height) },
            });
            img = .{ .RGBA = &rgba };
        },
        else => return error.Unimplemented,
    }
    const bytes_per_pixel = (bits_per_pixel + 7) / 8;
    const bits_per_row, const overflow = @mulWithOverflow(bits_per_pixel, self.width);
    if (overflow == 1) {
        return error.DimensionOverflow;
    }
    // The +1 is for the per-row filter type, which is at cr[0].
    const row_size = 1 + (bits_per_row + 7) / 8;

    var current_row = try self.al.alloc(u8, row_size);
    // var previous_row: [row_size]u8 = undefined;

    for (0..self.height) |y| {
        _ = y;
        // Read the filter type
        const filter_type = try reader.readByte();
        if (filter_type >= 5) {
            return error.InvalidFilterType;
        }

        // Read the row data
        _ = try reader.readAll(current_row[1..]);

        // Apply the inverse filter using Blend2D's optimized routine
        try self.filters_table.filters[filter_type](current_row[1..], bytes_per_pixel, row_size - 1, // subtract 1 for filter byte
            1 // height is 1 since we're processing one row at a time
        );

        // TODO: Convert the filtered data into the image
        // This will depend on your image.Image implementation
    }
    return error.Unimplemented;
}

fn skipChunk(self: *Decoder, length: u32) !void {
    // Skip chunk data
    try self.r.skipBytes(length, .{});
}

const ChunkHeader = struct {
    length: u32,
    chunk_type: ChunkType,
    type_bytes: [4]u8, // Store original bytes for CRC
};

fn readChunkHeader(self: *Decoder) !ChunkHeader {
    var header: ChunkHeader = undefined;

    try self.r.readNoEof(self.scratch[0..8]);

    header.length = std.mem.readInt(u32, self.scratch[0..4], .big);

    @memcpy(&header.type_bytes, self.scratch[4..8]);
    header.chunk_type = ChunkType.fromBytes(&header.type_bytes);

    return header;
}

const ChunkType = enum(u32) {
    ihdr,
    plte,
    idat,
    iend,
    trns,
    unknown,

    pub fn fromBytes(data: *const [4]u8) ChunkType {
        if (std.mem.eql(u8, data, "IHDR")) return .ihdr;
        if (std.mem.eql(u8, data, "PLTE")) return .plte;
        if (std.mem.eql(u8, data, "IDAT")) return .idat;
        if (std.mem.eql(u8, data, "IEND")) return .iend;
        if (std.mem.eql(u8, data, "tRNS")) return .trns;
        return .unknown;
    }

    fn bytes(self: ChunkType) [4]u8 {
        var b: [4]u8 = undefined;
        std.mem.writeInt(u32, &b, @intFromEnum(self), .big);
        return b;
    }
};

fn verifyChecksum(self: *Decoder) !void {
    // Read the 4-byte CRC from the file
    var crc_bytes: [4]u8 = undefined;
    _ = try self.r.readAll(&crc_bytes);

    // Get the expected CRC value (big endian)
    const expected_crc = std.mem.readInt(u32, &crc_bytes, .big);

    // Compare with our calculated CRC
    if (expected_crc != self.crc.final()) {
        return error.InvalidChecksum;
    }
}

fn colorDepthPaletted(color_depth: ColorBitDepth) bool {
    return @intFromEnum(ColorBitDepth.p1) <= @intFromEnum(color_depth) and @intFromEnum(color_depth) <= @intFromEnum(ColorBitDepth.p8);
}
