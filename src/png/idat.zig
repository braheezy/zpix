const std = @import("std");
const image = @import("image");
const Decoder = @import("decoder.zig").Decoder;
const InverseFilterTable = @import("filter.zig").InverseFilterTable;
const mem = std.mem;

// Custom reader for IDAT chunks
pub const IdatReader = struct {
    decoder: *Decoder,
    idatLength: u32,
    tmp: [8]u8 = undefined,
    first_chunk: bool,
    zlib_header_printed: bool = false,
    reached_end: bool = false, // Track if we've reached the end of IDAT chunks

    const Reader = std.io.Reader(*IdatReader, anyerror, read);

    pub fn init(decoder: *Decoder, initial_length: u32) !IdatReader {
        // Reset the CRC with IDAT identifier
        decoder.crc = std.hash.Crc32.init();
        var id_buf: [4]u8 = "IDAT".*;
        decoder.crc.update(&id_buf);

        return IdatReader{
            .decoder = decoder,
            .idatLength = initial_length,
            .first_chunk = initial_length > 0, // Only skip first chunk reading if we have initial data
        };
    }

    pub fn deinit(_: *IdatReader) void {
        // Nothing to free now
    }

    pub fn reader(self: *IdatReader) Reader {
        return .{ .context = self };
    }

    fn read(self: *IdatReader, buffer: []u8) !usize {
        if (buffer.len == 0 or self.reached_end) {
            return 0;
        }

        // If we've exhausted the current IDAT chunk, need to get next one
        if (self.idatLength == 0) {
            // We don't verify the CRC directly in the IdatReader
            // The decoder handles that separately before calling init
            // For subsequent chunks, we need to verify after each chunk
            if (!self.first_chunk) {
                // NOT the first chunk - consume 4-byte CRC from the previous chunk
                var crc_bytes: [4]u8 = undefined;
                const n = try self.decoder.r.readAll(&crc_bytes);
                if (n != 4) {
                    std.debug.print("Error: Could only read {d} CRC bytes\n", .{n});
                    self.reached_end = true;
                    return error.IncompleteCrc;
                }

                // Debug output the CRC
                std.debug.print("Read CRC bytes for previous chunk: ", .{});
                for (crc_bytes) |b| {
                    std.debug.print("{x:0>2} ", .{b});
                }
                std.debug.print("\n", .{});

                // Verify the CRC
                const expected_crc = std.mem.readInt(u32, &crc_bytes, .big);
                const actual_crc = self.decoder.crc.final();

                std.debug.print("CRC check: expected=0x{x:0>8}, actual=0x{x:0>8}, match={}\n", .{ expected_crc, actual_crc, expected_crc == actual_crc });

                if (expected_crc != actual_crc) {
                    std.debug.print("CRC MISMATCH for IDAT chunk!\n", .{});
                    self.reached_end = true;
                    return error.InvalidChecksum;
                }
                std.debug.print("CRC verified for IDAT chunk\n", .{});
            } else {
                self.first_chunk = false;
                std.debug.print("First IDAT chunk - skipping CRC verification (done by decoder)\n", .{});
            }

            // Try to read next chunk header
            const n = self.decoder.r.readAll(&self.tmp) catch |err| {
                std.debug.print("Error reading next chunk header: {any}\n", .{err});
                self.reached_end = true;
                return 0; // End of data
            };

            if (n != 8) {
                std.debug.print("Error: Incomplete chunk header ({d} bytes read)\n", .{n});
                self.reached_end = true;
                return error.IncompleteChunkHeader;
            }

            // Print the header bytes for debugging
            std.debug.print("Next chunk header: ", .{});
            for (self.tmp) |b| {
                std.debug.print("{x:0>2} ", .{b});
            }
            std.debug.print("\n", .{});

            // Check if it's still an IDAT chunk
            if (!mem.eql(u8, self.tmp[4..8], "IDAT")) {
                std.debug.print("End of IDAT chunks, found: {s}\n", .{self.tmp[4..8]});

                // Save the header for the next chunk
                if (self.decoder.next_chunk == null) {
                    // Copy the type bytes
                    var type_bytes: [4]u8 = undefined;
                    @memcpy(&type_bytes, self.tmp[4..8]);

                    // Debug the raw bytes
                    std.debug.print("Next chunk type bytes hex: ", .{});
                    for (self.tmp[4..8]) |b| {
                        std.debug.print("{x:0>2} ", .{b});
                    }
                    std.debug.print("\n", .{});

                    // Check if the next chunk looks valid (text characters)
                    var valid_chunk = true;
                    for (self.tmp[4..8]) |b| {
                        if (b < 32 or b > 126) {
                            valid_chunk = false;
                            std.debug.print("Warning: Invalid character in chunk type: {x:0>2}\n", .{b});
                        }
                    }

                    if (!valid_chunk) {
                        std.debug.print("ERROR: The next chunk doesn't look like a valid PNG chunk!\n", .{});
                        std.debug.print("This could be because we didn't properly read the CRC for the last IDAT.\n", .{});
                        self.reached_end = true;
                        return error.InvalidChunkType;
                    }

                    // Create the chunk header with properly initialized chunk_type
                    self.decoder.next_chunk = .{
                        .length = mem.readInt(u32, self.tmp[0..4], .big),
                        .type_bytes = type_bytes,
                        .chunk_type = if (mem.eql(u8, &type_bytes, "IEND"))
                            @enumFromInt(3) // IEND enum value
                        else if (mem.eql(u8, &type_bytes, "IHDR"))
                            @enumFromInt(0) // IHDR enum value
                        else if (mem.eql(u8, &type_bytes, "PLTE"))
                            @enumFromInt(1) // PLTE enum value
                        else if (mem.eql(u8, &type_bytes, "IDAT"))
                            @enumFromInt(2) // IDAT enum value
                        else if (mem.eql(u8, &type_bytes, "tRNS"))
                            @enumFromInt(4) // tRNS enum value
                        else
                            @enumFromInt(5), // unknown enum value
                    };

                    std.debug.print("Stored next chunk: {s} with length {d}, type: {d}\n", .{ self.decoder.next_chunk.?.type_bytes, self.decoder.next_chunk.?.length, @intFromEnum(self.decoder.next_chunk.?.chunk_type) });
                }

                self.reached_end = true;
                return 0; // No more IDAT data
            }

            // Get the length of the next IDAT chunk
            self.idatLength = mem.readInt(u32, self.tmp[0..4], .big);
            std.debug.print("Found additional IDAT chunk, length={d}\n", .{self.idatLength});

            // Sanity check the length
            if (self.idatLength > 10 * 1024 * 1024) { // 10 MB limit
                std.debug.print("Unreasonably large chunk length: {d}\n", .{self.idatLength});
                self.reached_end = true;
                return error.InvalidChunkLength;
            }

            // Reset CRC for the new chunk and include chunk type in the CRC
            self.decoder.crc = std.hash.Crc32.init();
            self.decoder.crc.update(self.tmp[4..8]); // Add chunk type to CRC

            // If this chunk is empty, return no data
            if (self.idatLength == 0) {
                return 0;
            }
        }

        // Read directly into the caller's buffer, up to the remaining chunk length
        const to_read = @min(buffer.len, self.idatLength);
        if (to_read == 0) return 0;

        // Attempt to read data from the current chunk
        const n = self.decoder.r.read(buffer[0..to_read]) catch |err| {
            std.debug.print("Error reading IDAT chunk data: {any}\n", .{err});
            self.reached_end = true;
            return err;
        };

        if (n == 0) {
            std.debug.print("Warning: Unexpected end of file when reading IDAT data\n", .{});
            self.reached_end = true;
            return error.UnexpectedEndOfFile;
        }

        // Check for zlib header in the first bytes we read
        if (!self.zlib_header_printed and n >= 2) {
            std.debug.print("Initial IDAT chunk data, first bytes: ", .{});
            const debug_bytes = @min(n, 16);
            for (buffer[0..debug_bytes]) |b| {
                std.debug.print("{x:0>2} ", .{b});
            }
            std.debug.print("\n", .{});

            // Check for zlib header (0x78 0x9C is most common)
            if (buffer[0] == 0x78) {
                std.debug.print("Valid zlib header detected (0x{x:0>2}{x:0>2})\n", .{ buffer[0], buffer[1] });
            } else {
                std.debug.print("WARNING: First bytes don't appear to be a standard zlib header\n", .{});
            }

            self.zlib_header_printed = true;
        }

        // Update the CRC and remaining length
        self.decoder.crc.update(buffer[0..n]);
        self.idatLength -= @as(u32, @intCast(n));

        return n;
    }
};
