const std = @import("std");
const image = @import("image");
const IdatReader = @import("idat.zig").IdatReader;
const mem = std.mem;

const png_header = "\x89PNG\r\n\x1a\n";

// Constants for PNG chunk structure
const chunk_header_size = 8; // 4 bytes length + 4 bytes type
const chunk_crc_size = 4;
const chunk_base_size = chunk_header_size + chunk_crc_size;

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

pub const Decoder = @This();

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
scratch: [3 * 256]u8 = [_]u8{0} ** (3 * 256),
next_chunk: ?ChunkHeader = null,

pub fn decode(al: std.mem.Allocator, r: std.io.AnyReader) !image.Image {
    var d = Decoder{
        .al = al,
        .r = r,
        .crc = std.hash.Crc32.init(),
    };

    try d.checkHeader();

    while (d.stage != .seen_iend) {
        try d.parseChunk();
    }

    // Check for valid image dimensions before returning
    const bounds = d.img.bounds();
    if (bounds.dX() == 0 or bounds.dY() == 0) {
        std.debug.print("ERROR: Invalid final image dimensions: {d}x{d}\n", .{ bounds.dX(), bounds.dY() });
        return error.InvalidImageDimensions;
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
    // Read the chunk header directly, or use stored next_chunk if available
    const chunk_header = if (self.next_chunk) |header| blk: {
        std.debug.print("Using stored next_chunk: {s} (type={d})\n", .{ header.type_bytes, @intFromEnum(header.chunk_type) });
        const temp = header;
        self.next_chunk = null; // Clear it after use
        break :blk temp;
    } else try self.readChunkHeader();

    // Reset CRC for each chunk and update with chunk type
    self.crc = std.hash.Crc32.init();
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
                // Not the first IDAT chunk, just skip it for now
                // The parseIdat will handle reading all IDAT chunks
                try self.skipChunk(chunk_header.length);
                return try self.verifyChecksum();
            } else {
                // First IDAT chunk
                self.stage = .seen_idat;
                self.idat_length = chunk_header.length;
                return try self.parseIdat(chunk_header.length);
            }
        },
        .iend => {
            std.debug.print("iend\n", .{});
            // IEND must be the last chunk
            if (self.stage != .seen_idat) {
                return error.ChunkOrderError;
            }
            self.stage = .seen_iend;
            // IEND has no data, just verify the checksum
            return try self.verifyChecksum();
        },
        else => {
            std.debug.print("not implemented: {s}\n", .{@tagName(chunk_header.chunk_type)});
            // For now, just skip unknown chunks instead of failing
            try self.skipChunk(chunk_header.length);
            return try self.verifyChecksum();
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

    // Debug current dimensions being parsed
    std.debug.print("Parsed dimensions from IHDR: width={d}, height={d}\n", .{ width, height });

    if (width == 0 or height == 0) {
        std.debug.print("ERROR: Invalid dimensions: width={d}, height={d}\n", .{ width, height });
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
    std.debug.print("parseIdat: length={d}\n", .{length});

    // Collect all IDAT chunks
    var all_data = std.ArrayList(u8).init(self.al);
    defer all_data.deinit();

    // Read the first IDAT chunk data and add it to our buffer
    if (length > 0) {
        // Read the chunk data in chunks to handle large IDAT chunks
        var remaining = length;
        while (remaining > 0) {
            const bytes_to_read = @min(remaining, self.scratch.len);
            try self.r.readNoEof(self.scratch[0..bytes_to_read]);

            // Add to our collected data
            try all_data.appendSlice(self.scratch[0..bytes_to_read]);

            self.crc.update(self.scratch[0..bytes_to_read]);
            remaining -= bytes_to_read;
        }

        // First chunk read, verify CRC
        try self.verifyChecksum();
        std.debug.print("First IDAT chunk ({d} bytes) read and CRC verified\n", .{length});
    } else {
        // Empty chunk, just verify CRC
        try self.verifyChecksum();
        std.debug.print("Empty first IDAT chunk, CRC verified\n", .{});
    }

    // Continue reading additional IDAT chunks until we hit something else
    while (true) {
        // Try to read the next chunk header
        var header_buf: [8]u8 = undefined;
        self.r.readNoEof(&header_buf) catch |err| {
            std.debug.print("Error reading next chunk header: {any}\n", .{err});
            break;
        };

        // Check if it's an IDAT chunk
        if (!std.mem.eql(u8, header_buf[4..8], "IDAT")) {
            // Not IDAT, prepare for the next chunk reading
            // Store this chunk header for next parseChunk call
            var next_chunk = ChunkHeader{
                .length = std.mem.readInt(u32, header_buf[0..4], .big),
                .type_bytes = undefined,
                .chunk_type = undefined,
            };
            @memcpy(&next_chunk.type_bytes, header_buf[4..8]);
            next_chunk.chunk_type = ChunkType.fromBytes(&next_chunk.type_bytes);
            self.next_chunk = next_chunk;

            std.debug.print("Found non-IDAT chunk: {s}, length={d}\n", .{ next_chunk.type_bytes, next_chunk.length });
            break;
        }

        // It's another IDAT chunk, read it
        const chunk_length = std.mem.readInt(u32, header_buf[0..4], .big);
        std.debug.print("Found additional IDAT chunk, length={d}\n", .{chunk_length});

        // Initialize CRC for this chunk
        self.crc = std.hash.Crc32.init();
        self.crc.update(header_buf[4..8]); // Add chunk type to CRC

        // Read the chunk data
        if (chunk_length > 0) {
            // For large chunks, read in smaller parts
            var remaining = chunk_length;
            while (remaining > 0) {
                const bytes_to_read = @min(remaining, self.scratch.len);
                try self.r.readNoEof(self.scratch[0..bytes_to_read]);
                self.crc.update(self.scratch[0..bytes_to_read]);

                // Add to our collected data
                try all_data.appendSlice(self.scratch[0..bytes_to_read]);

                remaining -= bytes_to_read;
            }
        }

        // Verify CRC for this additional chunk
        try self.verifyChecksum();
    }

    std.debug.print("Collected {d} bytes of IDAT data\n", .{all_data.items.len});

    // Now we can decompress and process the data
    if (all_data.items.len > 0) {
        // Print the first few bytes to check for zlib header
        if (all_data.items.len >= 2) {
            std.debug.print("First bytes of compressed data: ", .{});
            for (all_data.items[0..@min(16, all_data.items.len)]) |b| {
                std.debug.print("{x:0>2} ", .{b});
            }
            std.debug.print("\n", .{});

            // Check for zlib header (0x78 is most common first byte)
            if (all_data.items[0] == 0x78) {
                std.debug.print("Valid zlib header detected (0x{x:0>2} 0x{x:0>2})\n", .{ all_data.items[0], all_data.items[1] });
            }
        }

        // Create fixed buffer stream from our collected data
        var data_stream = std.io.fixedBufferStream(all_data.items);

        // Create a decompressor for the zlib stream
        var decompress_stream = std.compress.zlib.decompressor(data_stream.reader());

        // Read the image data using the decompressed reader
        self.img = try self.readImagePass(decompress_stream.reader(), 0, false);
    } else {
        std.debug.print("Warning: No IDAT data found\n", .{});
        return error.EmptyIdatData;
    }
}

// readImagePass reads a single image pass, sized according to the pass number.
fn readImagePass(
    self: *Decoder,
    reader: anytype,
    pass: u8,
    allocate_only: bool,
) !image.Image {
    _ = pass;
    std.debug.print("readImagePass: starting with dimensions: width={d}, height={d}\n", .{ self.width, self.height });

    // Sanity check dimensions
    if (self.width == 0 or self.height == 0) {
        std.debug.print("ERROR: Invalid dimensions for image: width={d}, height={d}\n", .{ self.width, self.height });
        return error.InvalidDimensions;
    }

    // Create a properly sized image based on color type
    var img: image.Image = undefined;
    var rgba_image: *image.RGBAImage = undefined;
    const rect = image.Rectangle{
        .min = .{ .x = 0, .y = 0 },
        .max = .{ .x = @intCast(self.width), .y = @intCast(self.height) },
    };

    if (allocate_only) {
        // Just allocate the image and return it without reading any data
        switch (self.color_depth) {
            .tc8 => {
                const rgba = try self.al.create(image.RGBAImage);
                rgba.* = try image.RGBAImage.init(self.al, rect);
                img = .{ .RGBA = rgba };
            },
            else => return error.Unimplemented,
        }
        return img;
    }

    // Create the image based on color type
    switch (self.color_depth) {
        .tc8 => {
            const rgba = try self.al.create(image.RGBAImage);
            rgba.* = try image.RGBAImage.init(self.al, rect);
            rgba_image = rgba;
            img = .{ .RGBA = rgba };
            std.debug.print("Created RGBA image {d}x{d}\n", .{ self.width, self.height });
        },
        else => return error.Unimplemented,
    }

    // Calculate bytes per pixel based on bit depth and color type
    const bits_per_pixel: u8 = switch (self.color_depth) {
        .tc8 => 24,
        else => return error.Unimplemented,
    };

    const bytes_per_pixel = (bits_per_pixel + 7) / 8;

    // The +1 is for the per-row filter type, which is at cr[0]
    const row_size: usize = 1 + ((bits_per_pixel * self.width) + 7) / 8;
    std.debug.print("Row size: {d} bytes (including filter byte)\n", .{row_size});

    // Create current and previous row buffers (for filtering)
    var cr = try self.al.alloc(u8, row_size);
    defer self.al.free(cr);
    var pr = try self.al.alloc(u8, row_size);
    defer self.al.free(pr);

    // Read the image data row by row
    var pixel_offset: usize = 0;

    for (0..self.height) |y| {
        // Read a row of data with filter byte
        const bytes_read = try reader.readAll(cr);
        if (bytes_read != row_size) {
            std.debug.print("Error reading row {d}: expected {d} bytes, got {d}\n", .{ y, row_size, bytes_read });
            return error.IncompleteRowData;
        }

        // Apply filter
        const cdat = cr[1..]; // Skip the filter byte
        const pdat = pr[1..]; // Previous row data

        switch (cr[0]) { // Filter type
            0 => {
                // None filter - no action needed
            },
            1 => {
                // Sub filter
                var i: usize = bytes_per_pixel;
                while (i < cdat.len) : (i += 1) {
                    cdat[i] +%= cdat[i - bytes_per_pixel];
                }
            },
            2 => {
                // Up filter
                for (pdat, 0..) |p, i| {
                    cdat[i] +%= p;
                }
            },
            3 => {
                // Average filter
                // First bytes_per_pixel bytes
                for (0..bytes_per_pixel) |i| {
                    cdat[i] +%= pdat[i] / 2;
                }
                // Remaining bytes
                var i: usize = bytes_per_pixel;
                while (i < cdat.len) : (i += 1) {
                    cdat[i] +%= @as(u8, @intCast((@as(u16, cdat[i - bytes_per_pixel]) + @as(u16, pdat[i])) / 2));
                }
            },
            4 => {
                // Paeth filter
                filterPaeth(cdat, pdat, bytes_per_pixel);
            },
            else => {
                std.debug.print("ERROR: Unknown filter type: {d}\n", .{cr[0]});
                return error.InvalidFilterType;
            },
        }

        // Convert bytes to colors based on color type
        switch (self.color_depth) {
            .tc8 => {
                if (img == .RGBA) {
                    // RGB to RGBA conversion
                    const pix = rgba_image.pixels;
                    var i: usize = pixel_offset;
                    var j: usize = 0;

                    while (j < cdat.len) : ({
                        j += 3;
                        i += 4;
                    }) {
                        const out_of_bounds = (i + 3 >= pix.len) or (j + 2 >= cdat.len);
                        if (out_of_bounds) break;

                        pix[i + 0] = cdat[j + 0]; // R
                        pix[i + 1] = cdat[j + 1]; // G
                        pix[i + 2] = cdat[j + 2]; // B
                        pix[i + 3] = 0xFF; // A (fully opaque)
                    }

                    pixel_offset += rgba_image.stride;
                }
            },
            else => return error.Unimplemented,
        }

        // Swap current and previous row for next iteration
        const temp = pr;
        pr = cr;
        cr = temp;
    }

    return img;
}

// Implementation of the Paeth filter as described in the PNG specification
fn filterPaeth(cdat: []u8, pdat: []u8, bytes_per_pixel: usize) void {
    // First handle the bytes_per_pixel bytes, which only have "up" as predictor
    for (0..bytes_per_pixel) |i| {
        cdat[i] +%= pdat[i];
    }

    // For the remaining pixels, use the Paeth predictor
    var i: usize = bytes_per_pixel;
    while (i < cdat.len) : (i += 1) {
        const a: i16 = @intCast(cdat[i - bytes_per_pixel]); // Left
        const b: i16 = @intCast(pdat[i]); // Above
        const c: i16 = @intCast(pdat[i - bytes_per_pixel]); // Upper left

        // Paeth predictor formula from the PNG spec, with safe integer math
        const p: i16 = a + b - c;
        const pa: i16 = if (p > a) p - a else a - p;
        const pb: i16 = if (p > b) p - b else b - p;
        const pc: i16 = if (p > c) p - c else c - p;

        var predictor: u8 = undefined;
        if (pa <= pb and pa <= pc) {
            predictor = @intCast(a);
        } else if (pb <= pc) {
            predictor = @intCast(b);
        } else {
            predictor = @intCast(c);
        }

        cdat[i] +%= predictor;
    }
}

fn skipChunk(self: *Decoder, length: u32) !void {
    // Read the chunk data into a buffer and update CRC
    if (length > 0) {
        const buf_size = @min(length, self.scratch.len);
        var remaining = length;

        while (remaining > 0) {
            const to_read = @min(remaining, buf_size);
            try self.r.readNoEof(self.scratch[0..to_read]);
            self.crc.update(self.scratch[0..to_read]);
            remaining -= to_read;
        }
    }
}

const ChunkHeader = struct {
    length: u32,
    chunk_type: ChunkType,
    type_bytes: [4]u8, // Store original bytes for CRC
};

fn readChunkHeader(self: *Decoder) !ChunkHeader {
    var header: ChunkHeader = undefined;

    try self.r.readNoEof(self.scratch[0..8]);

    // Log the raw header bytes
    std.debug.print("Read chunk header: ", .{});
    for (self.scratch[0..8]) |b| {
        std.debug.print("{x:0>2} ", .{b});
    }
    std.debug.print("\n", .{});

    header.length = std.mem.readInt(u32, self.scratch[0..4], .big);

    @memcpy(&header.type_bytes, self.scratch[4..8]);
    header.chunk_type = ChunkType.fromBytes(&header.type_bytes);

    std.debug.print("Interpreted as: type='{s}' ({d}), length={d}\n", .{ header.type_bytes, @intFromEnum(header.chunk_type), header.length });

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

pub fn verifyChecksum(self: *Decoder) !void {
    // Read the 4-byte CRC from the file
    var crc_bytes: [4]u8 = undefined;
    const bytes_read = try self.r.readAll(&crc_bytes);

    if (bytes_read != 4) {
        std.debug.print("WARNING: Could only read {d} bytes for CRC\n", .{bytes_read});
        return error.IncompleteCrc;
    }

    // Get the expected CRC value (big endian)
    const expected_crc = std.mem.readInt(u32, &crc_bytes, .big);
    const actual_crc = self.crc.final();

    if (expected_crc != actual_crc) {
        std.debug.print("CRC MISMATCH!\n", .{});
        return error.InvalidChecksum;
    }
}

fn colorDepthPaletted(color_depth: ColorBitDepth) bool {
    return @intFromEnum(ColorBitDepth.p1) <= @intFromEnum(color_depth) and @intFromEnum(color_depth) <= @intFromEnum(ColorBitDepth.p8);
}
