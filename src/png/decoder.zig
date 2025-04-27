const std = @import("std");
const image = @import("image");
const color = @import("color");
const Color = color.Color;
const Palette = color.Palette;
const Gray = color.Gray;

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

// Adam7 interlacing pass information
const InterlacePass = struct {
    x_offset: u32,
    y_offset: u32,
    x_factor: u32,
    y_factor: u32,
};

// Adam7 interlacing pattern data
const interlacing = [7]InterlacePass{
    .{ .x_offset = 0, .y_offset = 0, .x_factor = 8, .y_factor = 8 }, // Pass 1
    .{ .x_offset = 4, .y_offset = 0, .x_factor = 8, .y_factor = 8 }, // Pass 2
    .{ .x_offset = 0, .y_offset = 4, .x_factor = 4, .y_factor = 8 }, // Pass 3
    .{ .x_offset = 2, .y_offset = 0, .x_factor = 4, .y_factor = 4 }, // Pass 4
    .{ .x_offset = 0, .y_offset = 2, .x_factor = 2, .y_factor = 4 }, // Pass 5
    .{ .x_offset = 1, .y_offset = 0, .x_factor = 2, .y_factor = 2 }, // Pass 6
    .{ .x_offset = 0, .y_offset = 1, .x_factor = 1, .y_factor = 2 }, // Pass 7
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
    invalid,
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
allocator: std.mem.Allocator,
// reader into provided PNG data
r: std.io.AnyReader,
img: image.Image = undefined,
width: u32 = 0,
height: u32 = 0,
depth: u8 = 0,
color_type: ColorType = undefined,
color_depth: ColorBitDepth = undefined,
interlace: Interlace = .none,
crc: std.hash.Crc32,
stage: Stage = .start,
palette: ?Palette = null,
idat_length: u32 = 0,
scratch: [3 * 256]u8 = [_]u8{0} ** (3 * 256),

pub fn decode(allocator: std.mem.Allocator, r: std.io.AnyReader) !image.Image {
    // We may do some allocations for some images that are easier to clean up if an arena is used.
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var d = Decoder{
        .allocator = arena.allocator(),
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
        std.log.info("[zpix] png: ERROR: Invalid final image dimensions: {d}x{d}", .{ bounds.dX(), bounds.dY() });
        return error.InvalidImageDimensions;
    }

    // Copy the image data so the caller owns it.
    switch (d.img) {
        .Gray => |gray| {
            const gray_img = try image.GrayImage.init(allocator, bounds);
            @memcpy(gray_img.pixels, gray.pixels);
            d.img = .{ .Gray = gray_img };
        },
        .Gray16 => |gray16| {
            const gray16_img = try image.Gray16Image.init(allocator, bounds);
            @memcpy(gray16_img.pixels, gray16.pixels);
            d.img = .{ .Gray16 = gray16_img };
        },
        .YCbCr => |ycbcr| {
            const ycbcr_img = try image.YCbCrImage.init(
                allocator,
                bounds,
                ycbcr.subsample_ratio,
            );
            @memcpy(ycbcr_img.pixels, ycbcr.pixels);
            d.img = .{ .YCbCr = ycbcr_img };
        },
        .RGBA => |rgba| {
            const rgba_img = try image.RGBAImage.init(allocator, bounds);
            @memcpy(rgba_img.pixels, rgba.pixels);
            d.img = .{ .RGBA = rgba_img };
        },
        .RGBA64 => |rgba64| {
            const rgba64_img = try image.RGBA64Image.init(allocator, bounds);
            @memcpy(rgba64_img.pixels, rgba64.pixels);
            d.img = .{ .RGBA64 = rgba64_img };
        },
        .NRGBA => |nrgba| {
            const nrgba_img = try image.NRGBAImage.init(allocator, bounds);
            @memcpy(nrgba_img.pixels, nrgba.pixels);
            d.img = .{ .NRGBA = nrgba_img };
        },
        .NRGBA64 => |nrgba64| {
            const nrgba64_img = try image.NRGBA64Image.init(allocator, bounds);
            @memcpy(nrgba64_img.pixels, nrgba64.pixels);
            d.img = .{ .NRGBA64 = nrgba64_img };
        },
        .CMYK => |cmyk| {
            const cmyk_img = try image.CMYKImage.init(allocator, bounds);
            @memcpy(cmyk_img.pixels, cmyk.pixels);
            d.img = .{ .CMYK = cmyk_img };
        },
        .Paletted => |paletted| {
            const paletted_img = try image.PalettedImage.init(allocator, bounds, paletted.palette);
            @memcpy(paletted_img.pixels, paletted.pixels);
            d.img = .{ .Paletted = paletted_img };
        },
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
    // Read the chunk header
    const chunk_header = try self.readChunkHeader();

    // Reset CRC for each chunk and update with chunk type
    self.crc = std.hash.Crc32.init();
    self.crc.update(&chunk_header.type_bytes);

    switch (chunk_header.chunk_type) {
        .ihdr => {
            if (self.stage != .start) {
                return error.ChunkOrderInHeaderError;
            }
            self.stage = .seen_ihdr;
            return try self.parseIhdr(chunk_header.length);
        },
        .plte => {
            if (self.stage != .seen_ihdr) {
                return error.ChunkOrderPlteError;
            }
            self.stage = .seen_plte;
            return try self.parsePlte(chunk_header.length);
        },
        .idat => {
            const stage_int = @intFromEnum(self.stage);
            const seen_ihdr = @intFromEnum(Stage.seen_ihdr);
            const seen_idat = @intFromEnum(Stage.seen_idat);

            const stage_1 = stage_int < seen_ihdr;
            const stage_2 = stage_int > seen_idat;
            const stage_3 = stage_int == seen_ihdr;
            const stage_4 = colorDepthPaletted(self.color_depth);

            if (stage_1 or stage_2 or (stage_3 and stage_4)) {
                std.log.info("[zpix] png: stage_1: {any} stage_2: {any} stage_3: {any} stage_4: {any}", .{
                    stage_1,
                    stage_2,
                    stage_3,
                    stage_4,
                });
                return error.ChunkOrderIdatError;
            }

            if (self.stage != .seen_idat) {
                // First IDAT chunk encountered
                self.stage = .seen_idat;
            }

            std.log.info("[zpix] png: idat: length - {d}", .{chunk_header.length});
            // Process all consecutive IDAT chunks
            return try self.parseIdat(chunk_header.length);
        },
        .iend => {
            // IEND must be the last chunk
            if (self.stage != .seen_idat) {
                return error.ChunkOrderIendError;
            }
            self.stage = .seen_iend;
            // IEND has no data, just verify the checksum
            return try self.verifyChecksum();
        },
        else => {
            if (chunk_header.chunk_type == .unknown) {
                std.log.info("[zpix] png: skipping unknown chunk: '{s}' (length: {d})", .{ chunk_header.type_bytes, chunk_header.length });
            } else {
                std.log.info("[zpix] png: skipping chunk: {s} (length: {d})", .{ @tagName(chunk_header.chunk_type), chunk_header.length });
            }
            // Skip chunk data
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
    self.interlace = @enumFromInt(bytes[12]);
    if (self.interlace != .none and self.interlace != .adam7) {
        return error.UnsupportedInterlaceMethod;
    }

    const width = std.mem.readInt(u32, bytes[0..4], .big);
    const height = std.mem.readInt(u32, bytes[4..8], .big);

    if (width == 0 or height == 0) {
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

    self.color_depth = switch (self.depth) {
        1 => switch (self.color_type) {
            .grayscale => .g1,
            .paletted => .p1,
            else => return error.InvalidColorTypeDepthCombo,
        },
        2 => switch (self.color_type) {
            .grayscale => .g2,
            .paletted => .p2,
            else => return error.InvalidColorTypeDepthCombo,
        },
        4 => switch (self.color_type) {
            .grayscale => .g4,
            .paletted => .p4,
            else => return error.InvalidColorTypeDepthCombo,
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
            else => return error.InvalidColorTypeDepthCombo,
        },
        else => return error.UnsupportedBitDepth,
    };

    std.log.info("[zpix] png: ihdr: {d}x{d} {s} {s}", .{ self.width, self.height, @tagName(self.color_type), @tagName(self.color_depth) });
    return try self.verifyChecksum();
}

// Process all consecutive IDAT chunks and decode the image
fn parseIdat(self: *Decoder, first_chunk_length: u32) !void {
    // Collect all IDAT chunks
    var all_data = std.ArrayList(u8).init(self.allocator);
    defer all_data.deinit();

    // Read the first IDAT chunk data and add it to our buffer
    if (first_chunk_length > 0) {
        // Read the chunk data in chunks to handle large IDAT chunks
        var remaining = first_chunk_length;
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
    } else {
        // Empty chunk, just verify CRC
        try self.verifyChecksum();
    }

    // Continue reading chunks until we hit a non-IDAT chunk
    while (true) {
        // Try to read the next chunk header
        var header_buf: [8]u8 = undefined;
        self.r.readNoEof(&header_buf) catch |err| {
            std.log.info("[zpix] png: Error reading next chunk header: {any}", .{err});
            break;
        };

        // Check if it's an IDAT chunk
        if (!std.mem.eql(u8, header_buf[4..8], "IDAT")) {
            // Not IDAT, log the chunk type we found but don't try to process it now

            // We need to stop reading IDAT chunks and let parseChunk handle
            // the next chunk. Since we can't put back what we've read,
            // we reset the stage to allow parseChunk to be called again
            self.stage = .seen_ihdr; // Temporarily revert to allow another chunk to be processed

            // Now call parseChunk with this header
            var next_header = ChunkHeader{
                .length = std.mem.readInt(u32, header_buf[0..4], .big),
                .type_bytes = undefined,
                .chunk_type = undefined,
            };
            @memcpy(&next_header.type_bytes, header_buf[4..8]);
            next_header.chunk_type = ChunkType.fromBytes(&next_header.type_bytes);

            // Reset the stage to seen_idat and process the chunk
            self.stage = .seen_idat;

            // Process the chunk based on its type
            self.crc = std.hash.Crc32.init();
            self.crc.update(&next_header.type_bytes);

            switch (next_header.chunk_type) {
                .iend => {
                    // IEND must be the last chunk
                    self.stage = .seen_iend;
                    // IEND has no data, just verify the checksum
                    try self.verifyChecksum();
                },
                else => {
                    std.log.info("[zpix] png: Found non-IDAT/IEND chunk: {s}", .{next_header.type_bytes});
                    // Skip this chunk for now and let the next parseChunk call handle it
                    try self.skipChunk(next_header.length);
                    try self.verifyChecksum();
                },
            }

            break;
        }

        // It's another IDAT chunk, read it
        const chunk_length = std.mem.readInt(u32, header_buf[0..4], .big);

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

    // Now we can decompress and process the data
    if (all_data.items.len > 0) {
        // Create fixed buffer stream from our collected data
        var data_stream = std.io.fixedBufferStream(all_data.items);

        // Create a decompressor for the zlib stream
        var decompress_stream = std.compress.zlib.decompressor(data_stream.reader());

        // Decode the image based on interlace type, similar to Go's implementation
        if (self.interlace == .none) {
            // Non-interlaced image: just read the image in a single pass
            self.img = try self.readImagePass(decompress_stream.reader(), 0, false);
        } else if (self.interlace == .adam7) {
            // Interlaced image: allocate the full image first
            self.img = try self.readImagePass(decompress_stream.reader(), 0, true);

            // Then read each of the 7 passes
            for (0..7) |p| {
                const pass: u8 = @intCast(p);
                const pass_img = self.readImagePass(decompress_stream.reader(), pass, false) catch |err| {
                    if (err == error.EmptyPass) {
                        // Skip empty passes
                        continue;
                    }
                    return err;
                };

                // TODO: We should merge the pass image into the main image
                // try self.mergePassInto(self.img, pass_img, pass);
                _ = pass_img; // Will use this when implementing mergePassInto
            }
        }
    } else {
        return error.EmptyIdatData;
    }
}

fn parsePlte(self: *Decoder, length: u32) !void {
    const num_palettes = length / 3;
    if (length % 3 != 0 or num_palettes <= 0 or num_palettes > 256 or num_palettes > @as(u32, 1) << @as(u5, @intCast(self.depth))) {
        return error.BadPlteLength;
    }

    try self.r.readNoEof(self.scratch[0 .. num_palettes * 3]);
    self.crc.update(self.scratch[0 .. num_palettes * 3]);

    switch (self.color_depth) {
        .p1, .p2, .p4, .p8 => {
            self.palette = try self.allocator.alloc(color.Color, num_palettes);
            for (0..num_palettes) |i| {
                self.palette.?[i] = .{ .rgba = .{
                    .r = self.scratch[i * 3 + 0],
                    .g = self.scratch[i * 3 + 1],
                    .b = self.scratch[i * 3 + 2],
                    .a = 0xFF,
                } };
            }

            // var i = num_palettes;
            // while (i < 256) : (i += 1) {
            //     // Initialize the rest of the palette to opaque black. The spec (section
            //     // 11.2.3) says that "any out-of-range pixel value found in the image data
            //     // is an error", but some real-world PNG files have out-of-range pixel
            //     // values. We fall back to opaque black, the same as libpng 1.5.13;
            //     // ImageMagick 6.5.7 returns an error.
            //     self.palette[i] = color.RGBA{ .r = 0, .g = 0, .b = 0, .a = 0xFF };
            // }
            // self.palette = self.palette[0..num_palettes];
        },
        .tc8, .tca8, .tc16, .tca16 => {
            // As per the PNG spec, a PLTE chunk is optional (and for practical purposes,
            // ignorable) for the ctTrueColor and ctTrueColorAlpha color types (section 4.1.2).
        },
        else => {
            return error.PlteColorTypeMismatch;
        },
    }

    return try self.verifyChecksum();
}

// readImagePass reads a single image pass, sized according to the pass number.
fn readImagePass(
    self: *Decoder,
    reader: anytype,
    pass: u8,
    allocate_only: bool,
) !image.Image {
    var width = self.width;
    var height = self.height;

    // For interlaced images, calculate the dimensions for this pass
    if (self.interlace == .adam7 and !allocate_only) {
        const p = interlacing[pass];

        // Calculate pass dimensions using the same formula as Go:
        // width = (width - p.x_offset + p.x_factor - 1) / p.x_factor
        // This handles rounding up when dividing
        width = (width -| p.x_offset +| p.x_factor -| 1) / p.x_factor;
        height = (height -| p.y_offset +| p.y_factor -| 1) / p.y_factor;

        // A PNG image can't have zero width or height, but for an interlaced
        // image, an individual pass might have zero width or height.
        if (width == 0 or height == 0) {
            return error.EmptyPass; // Skip empty passes
        }
    }

    // Sanity check dimensions
    if (width == 0 or height == 0) {
        return error.InvalidDimensions;
    }

    // Create the rectangle with proper dimensions for this pass
    const rect = image.Rectangle{
        .min = .{ .x = 0, .y = 0 },
        .max = .{ .x = @intCast(width), .y = @intCast(height) },
    };

    // Prepare variables for different image types, similar to Go's implementation
    var img: image.Image = undefined;
    var rgba: image.RGBAImage = undefined;
    var rgba64: image.RGBA64Image = undefined;
    var nrgba: image.NRGBAImage = undefined;
    var gray: image.GrayImage = undefined;
    var gray16: image.Gray16Image = undefined;
    var paletted: image.PalettedImage = undefined;
    var nrgba64: image.NRGBA64Image = undefined;

    // Calculate bits per pixel based on color depth
    const bits_per_pixel: u8 = switch (self.color_depth) {
        .g1, .p1 => 1,
        .g2, .p2 => 2,
        .g4, .p4 => 4,
        .g8, .p8 => 8,
        .ga8 => 16,
        .tc8 => 24,
        .tca8 => 32,
        .g16 => 16,
        .ga16 => 32,
        .tc16 => 48,
        .tca16 => 64,
        .invalid => return error.InvalidColorDepth,
    };

    // Create the image based on color type, setting the appropriate pointer
    switch (self.color_depth) {
        .g1, .g2, .g4, .g8 => {
            gray = try image.GrayImage.init(self.allocator, rect);
            img = .{ .Gray = gray };
        },
        .ga8 => {
            nrgba = try image.NRGBAImage.init(self.allocator, rect);
            img = .{ .NRGBA = nrgba };
        },
        .ga16 => {
            nrgba64 = try image.NRGBA64Image.init(self.allocator, rect);
            img = .{ .NRGBA64 = nrgba64 };
        },
        .g16 => {
            gray16 = try image.Gray16Image.init(self.allocator, rect);
            img = .{ .Gray16 = gray16 };
        },
        .tc8 => {
            rgba = try image.RGBAImage.init(self.allocator, rect);
            img = .{ .RGBA = rgba };
        },
        .tc16 => {
            rgba64 = try image.RGBA64Image.init(self.allocator, rect);
            img = .{ .RGBA64 = rgba64 };
        },
        .tca8 => {
            nrgba = try image.NRGBAImage.init(self.allocator, rect);
            img = .{ .NRGBA = nrgba };
        },
        .tca16 => {
            nrgba64 = try image.NRGBA64Image.init(self.allocator, rect);
            img = .{ .NRGBA64 = nrgba64 };
        },
        .p1, .p2, .p4, .p8 => {
            paletted = try image.PalettedImage.init(self.allocator, rect, self.palette.?);
            img = .{ .Paletted = paletted };
        },
        // Add cases for other color types as you implement them
        else => {
            std.log.err("Unimplemented color type 1: {s}", .{@tagName(self.color_depth)});
            return error.Unimplemented;
        },
    }

    // If this is allocate_only, the reader will not be used
    if (allocate_only) {
        return img; // Just return the allocated image
    }

    const bytes_per_pixel = (bits_per_pixel + 7) / 8;

    // The +1 is for the per-row filter type, which is at cr[0]
    const row_size: usize = 1 + ((bits_per_pixel * width) + 7) / 8;

    // Create current and previous row buffers (for filtering)
    var cr = try self.allocator.alloc(u8, row_size);
    defer self.allocator.free(cr);
    var pr = try self.allocator.alloc(u8, row_size);
    defer self.allocator.free(pr);
    @memset(pr, 0);
    @memset(cr, 0);

    // Read the image data row by row
    var pixel_offset: usize = 0;

    for (0..height) |y| {
        // Read a row of data with filter byte
        const bytes_read = try reader.readAll(cr);
        if (bytes_read != row_size) {
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
                return error.InvalidFilterType;
            },
        }

        // Convert bytes to colors based on color type
        switch (self.color_depth) {
            .tc8 => {
                const rgba_img = img.RGBA;
                // RGB to RGBA conversion
                const pix = rgba_img.pixels;
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

                pixel_offset += rgba_img.stride;
            },
            .tc16 => {
                for (0..width) |x| {
                    const r_col = @as(u16, @intCast(cdat[x * 6 + 0])) << 8 | @as(u16, @intCast(cdat[x * 6 + 1]));
                    const g_col = @as(u16, @intCast(cdat[x * 6 + 2])) << 8 | @as(u16, @intCast(cdat[x * 6 + 3]));
                    const b_col = @as(u16, @intCast(cdat[x * 6 + 4])) << 8 | @as(u16, @intCast(cdat[x * 6 + 5]));
                    rgba64.setRGBA64(@intCast(x), @intCast(y), .{ .r = r_col, .g = g_col, .b = b_col, .a = 0xFFFF });
                }
            },
            .g1 => {
                var x: usize = 0;
                while (x < width) : (x += 8) {
                    var bit_index = cdat[x / 8];
                    var x2: usize = 0;
                    while (x2 < 8 and x + x2 < width) : (x2 += 1) {
                        const bit_value = (bit_index >> 7) * 0xff;
                        gray.setGray(@intCast(x + x2), @intCast(y), .{ .y = (bit_value >> 7) * 0xff });
                        bit_index <<= 1;
                    }
                }
            },
            .g2 => {
                var x: usize = 0;
                while (x < width) : (x += 4) {
                    var bit_index = cdat[x / 4];
                    var x2: usize = 0;
                    while (x2 < 4 and x + x2 < width) : (x2 += 1) {
                        const bit_value = (bit_index >> 6) * 0x55;
                        gray.setGray(@intCast(x + x2), @intCast(y), .{ .y = bit_value });
                        bit_index <<= 2;
                    }
                }
            },
            .g4 => {
                var x: usize = 0;
                while (x < width) : (x += 2) {
                    var bit_index = cdat[x / 2];
                    var x2: usize = 0;
                    while (x2 < 2 and x + x2 < width) : (x2 += 1) {
                        const bit_value = (bit_index >> 4) * 0x11;
                        gray.setGray(@intCast(x + x2), @intCast(y), .{ .y = bit_value });
                        bit_index <<= 4;
                    }
                }
            },
            .g8 => {
                @memcpy(gray.pixels[pixel_offset..][0..cdat.len], cdat);
                pixel_offset += gray.stride;
            },
            .g16 => {
                for (0..width) |x| {
                    const y_column = @as(u16, @intCast(cdat[x * 2])) << 8 | @as(u16, @intCast(cdat[x * 2 + 1]));
                    gray16.setGray16(@intCast(x), @intCast(y), .{ .y = y_column });
                }
            },
            .ga8 => {
                for (0..width) |x| {
                    const y_col = cdat[2 * x + 0];
                    nrgba.setNRGBA(
                        @intCast(x),
                        @intCast(y),
                        .{ .r = y_col, .g = y_col, .b = y_col, .a = cdat[2 * x + 1] },
                    );
                }
            },
            .ga16 => {
                for (0..width) |x| {
                    const y_col = @as(u16, @intCast(cdat[4 * x + 0])) << 8 | @as(u16, @intCast(cdat[4 * x + 1]));
                    const a_col = @as(u16, @intCast(cdat[4 * x + 2])) << 8 | @as(u16, @intCast(cdat[4 * x + 3]));
                    nrgba64.setNRGBA64(
                        @intCast(x),
                        @intCast(y),
                        .{ .r = y_col, .g = y_col, .b = y_col, .a = a_col },
                    );
                }
            },
            .tca8 => {
                @memcpy(nrgba.pixels[pixel_offset..][0..cdat.len], cdat);
                pixel_offset += nrgba.stride;
            },
            .tca16 => {
                for (0..width) |x| {
                    const r_col = @as(u16, @intCast(cdat[8 * x + 0])) << 8 | @as(u16, @intCast(cdat[8 * x + 1]));
                    const g_col = @as(u16, @intCast(cdat[8 * x + 2])) << 8 | @as(u16, @intCast(cdat[8 * x + 3]));
                    const b_col = @as(u16, @intCast(cdat[8 * x + 4])) << 8 | @as(u16, @intCast(cdat[8 * x + 5]));
                    const a_col = @as(u16, @intCast(cdat[8 * x + 6])) << 8 | @as(u16, @intCast(cdat[8 * x + 7]));
                    nrgba64.setNRGBA64(
                        @intCast(x),
                        @intCast(y),
                        .{ .r = r_col, .g = g_col, .b = b_col, .a = a_col },
                    );
                }
            },
            .p1 => {
                for (0..width) |x| {
                    var bit_index = cdat[x / 8];
                    var x2: usize = 0;
                    while (x2 < 8 and x + x2 < width) : (x2 += 1) {
                        const bit_value = bit_index >> 7;
                        if (paletted.palette.len <= bit_value) {
                            paletted.palette = paletted.palette[0 .. bit_value + 1];
                        }
                        paletted.setColorIndex(@intCast(x + x2), @intCast(y), @intCast(bit_value));
                        bit_index <<= 1;
                    }
                }
            },
            .p2 => {
                var x: usize = 0;
                while (x < width) : (x += 4) {
                    var bit_index = cdat[x / 4];
                    var x2: usize = 0;
                    while (x2 < 4 and x + x2 < width) : (x2 += 1) {
                        const index = bit_index >> 6;
                        if (paletted.palette.len <= index) {
                            paletted.palette = paletted.palette[0 .. index + 1];
                        }
                        paletted.setColorIndex(@intCast(x + x2), @intCast(y), @intCast(index));
                        bit_index <<= 2;
                    }
                }
            },
            .p4 => {
                var x: usize = 0;
                while (x < width) : (x += 2) {
                    var bit_index = cdat[x / 2];
                    var x2: usize = 0;
                    while (x2 < 2 and x + x2 < width) : (x2 += 1) {
                        const index = bit_index >> 4;
                        if (paletted.palette.len <= index) {
                            paletted.palette = paletted.palette[0 .. index + 1];
                        }
                        paletted.setColorIndex(@intCast(x + x2), @intCast(y), @intCast(index));
                        bit_index <<= 4;
                    }
                }
            },
            .p8 => {
                if (paletted.palette.len != 256) {
                    for (0..width) |x| {
                        if (paletted.palette.len <= cdat[x]) {
                            paletted.palette = paletted.palette[0 .. cdat[x] + 1];
                        }
                    }
                }
                @memcpy(paletted.pixels[pixel_offset..][0..cdat.len], cdat);
                pixel_offset += paletted.stride;
            },
            // Add cases for other color types as you implement them
            else => {
                std.log.err("Unimplemented color type 2: {s}", .{@tagName(self.color_depth)});
                return error.Unimplemented;
            },
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
    // if (length > 0) {
    const buf_size = @min(length, self.scratch.len);
    var remaining = length;

    while (remaining > 0) {
        const to_read = @min(remaining, buf_size);
        try self.r.readNoEof(self.scratch[0..to_read]);
        self.crc.update(self.scratch[0..to_read]);
        remaining -= to_read;
    }
    // }
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
    chrm, // Chromaticity coordinates
    gama, // Image gamma
    iccp, // ICC profile
    sbit, // Significant bits
    srgb, // Standard RGB color space
    bkgd, // Background color
    phys, // Physical dimensions
    text, // Textual data
    ztxt, // Compressed textual data
    itxt, // International textual data
    time, // Last modification time
    unknown,

    pub fn fromBytes(data: *const [4]u8) ChunkType {
        if (std.mem.eql(u8, data, "IHDR")) return .ihdr;
        if (std.mem.eql(u8, data, "PLTE")) return .plte;
        if (std.mem.eql(u8, data, "IDAT")) return .idat;
        if (std.mem.eql(u8, data, "IEND")) return .iend;
        if (std.mem.eql(u8, data, "tRNS")) return .trns;
        if (std.mem.eql(u8, data, "cHRM")) return .chrm;
        if (std.mem.eql(u8, data, "gAMA")) return .gama;
        if (std.mem.eql(u8, data, "iCCP")) return .iccp;
        if (std.mem.eql(u8, data, "sBIT")) return .sbit;
        if (std.mem.eql(u8, data, "sRGB")) return .srgb;
        if (std.mem.eql(u8, data, "bKGD")) return .bkgd;
        if (std.mem.eql(u8, data, "pHYs")) return .phys;
        if (std.mem.eql(u8, data, "tEXt")) return .text;
        if (std.mem.eql(u8, data, "zTXt")) return .ztxt;
        if (std.mem.eql(u8, data, "iTXt")) return .itxt;
        if (std.mem.eql(u8, data, "tIME")) return .time;
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
        std.log.info("[zpix] png: WARNING: Could only read {d} bytes for CRC", .{bytes_read});
        return error.IncompleteCrc;
    }

    // Get the expected CRC value (big endian)
    const expected_crc = std.mem.readInt(u32, &crc_bytes, .big);
    const actual_crc = self.crc.final();

    if (expected_crc != actual_crc) {
        std.log.info("[zpix] png: CRC MISMATCH!", .{});
        return error.InvalidChecksum;
    }
}

fn colorDepthPaletted(color_depth: ColorBitDepth) bool {
    std.log.info("[zpix] png: p1: {d}, p8: {d}, color_depth: {d}", .{
        @intFromEnum(ColorBitDepth.p1),
        @intFromEnum(ColorBitDepth.p8),
        @intFromEnum(color_depth),
    });
    return @intFromEnum(ColorBitDepth.p1) <= @intFromEnum(color_depth) and @intFromEnum(color_depth) <= @intFromEnum(ColorBitDepth.p8);
}

// Implementation of mergePassInto to combine interlaced passes
fn mergePassInto(self: *Decoder, dst_img: image.Image, src_img: image.Image, pass: u8) !void {
    // Implement the logic to merge an interlaced pass into the main image
    // This is needed for Adam7 interlacing
    const p = interlacing[pass];

    switch (self.color_depth) {
        .tc8 => {
            if (dst_img == .RGBA and src_img == .RGBA) {
                const dst = dst_img.RGBA;
                const src = src_img.RGBA;
                const src_rect = src.bounds();
                const src_width = src_rect.dX();

                var y: usize = 0;
                var dy = p.y_offset;

                while (y < src_rect.dY()) : (y += 1) {
                    var x: usize = 0;
                    var dx = p.x_offset;

                    while (x < src_width) : (x += 1) {
                        const src_offset = y * src.stride + x * 4;
                        const dst_offset = dy * dst.stride + dx * 4;

                        dst.pixels[dst_offset + 0] = src.pixels[src_offset + 0]; // R
                        dst.pixels[dst_offset + 1] = src.pixels[src_offset + 1]; // G
                        dst.pixels[dst_offset + 2] = src.pixels[src_offset + 2]; // B
                        dst.pixels[dst_offset + 3] = src.pixels[src_offset + 3]; // A

                        dx += p.x_factor;
                    }

                    dy += p.y_factor;
                }
            }
        },
        // Add cases for other color types as you implement them
        else => return error.Unimplemented,
    }
}
