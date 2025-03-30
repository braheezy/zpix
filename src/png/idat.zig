const std = @import("std");
const image = @import("image");
const Decoder = @import("decoder.zig").Decoder;
const mem = std.mem;

// Custom reader for IDAT chunks
// This reader presents multiple IDAT chunks as one continuous stream,
// similar to how Go's PNG reader works. It reads IDAT chunks sequentially,
// verifies their CRCs, and presents the data as a single continuous stream
// to the zlib decompressor.
pub const IdatReader = struct {
    decoder: *Decoder,
    idatLength: u32,
    tmp: [8]u8 = undefined,
    first_chunk: bool,
    end_of_data: bool = false,

    const Reader = std.io.Reader(*IdatReader, anyerror, read);

    pub fn init(decoder: *Decoder, initial_length: u32) !IdatReader {
        // Reset CRC for the first chunk and add "IDAT" to the CRC
        decoder.crc = std.hash.Crc32.init();
        decoder.crc.update("IDAT");

        return IdatReader{
            .decoder = decoder,
            .idatLength = initial_length,
            .first_chunk = true,
        };
    }

    pub fn deinit(_: *IdatReader) void {
        // Nothing to free
    }

    pub fn reader(self: *IdatReader) Reader {
        return .{ .context = self };
    }

    fn read(self: *IdatReader, buffer: []u8) !usize {
        if (buffer.len == 0 or self.end_of_data) {
            return 0;
        }

        // If we've exhausted the current IDAT chunk, need to get next one
        if (self.idatLength == 0) {
            if (!self.first_chunk) {
                // Verify CRC of completed chunk
                var crc_bytes: [4]u8 = undefined;
                const n = try self.decoder.r.readAll(&crc_bytes);
                if (n != 4) {
                    self.end_of_data = true;
                    return error.IncompleteCrc;
                }

                // Verify the CRC
                const expected_crc = std.mem.readInt(u32, &crc_bytes, .big);
                const actual_crc = self.decoder.crc.final();

                std.debug.print("CRC bytes: ", .{});
                for (crc_bytes) |b| {
                    std.debug.print("{x:0>2} ", .{b});
                }
                std.debug.print("\nCRC check: expected=0x{x:0>8}, actual=0x{x:0>8}, match={}\n", .{ expected_crc, actual_crc, expected_crc == actual_crc });

                if (expected_crc != actual_crc) {
                    self.end_of_data = true;
                    return error.InvalidChecksum;
                }
            } else {
                self.first_chunk = false;
            }

            // Try to read next chunk header
            const n = try self.decoder.r.readAll(&self.tmp);
            if (n != 8) {
                self.end_of_data = true;
                return 0; // End of data
            }

            // Check if it's still an IDAT chunk
            if (!mem.eql(u8, self.tmp[4..8], "IDAT")) {
                // This is not an IDAT chunk, so we're done reading IDAT data
                // We'll need to reposition the reader to read this chunk again
                // during normal chunk processing, but that's not our job here
                self.end_of_data = true;
                return 0;
            }

            // Get the length of the next IDAT chunk
            self.idatLength = mem.readInt(u32, self.tmp[0..4], .big);

            // Reset CRC for the new chunk and include chunk type in the CRC
            self.decoder.crc = std.hash.Crc32.init();
            self.decoder.crc.update(self.tmp[4..8]); // Add chunk type to CRC

            std.debug.print("Found additional IDAT chunk, length={d}\n", .{self.idatLength});

            // If this chunk is empty (valid but unusual), recursively try again
            if (self.idatLength == 0) {
                return self.read(buffer);
            }
        }

        // Read directly into the caller's buffer, up to the remaining chunk length
        const to_read = @min(buffer.len, self.idatLength);
        const n = try self.decoder.r.read(buffer[0..to_read]);

        if (n == 0) {
            self.end_of_data = true;
            return error.UnexpectedEndOfFile;
        }

        // Update the CRC and remaining length
        self.decoder.crc.update(buffer[0..n]);
        self.idatLength -= @as(u32, @intCast(n));

        return n;
    }
};
