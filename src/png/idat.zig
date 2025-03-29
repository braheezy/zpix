const std = @import("std");
const image = @import("image");
const Decoder = @import("decoder.zig").Decoder;
const InverseFilterTable = @import("filter.zig").InverseFilterTable;
const mem = std.mem;

// Custom reader for IDAT chunks
pub const IdatReader = struct {
    decoder: *Decoder,
    remaining_bytes: u32 = 0,
    current_chunk_data: []u8 = &[_]u8{},
    chunk_data: []u8 = undefined,
    chunk_started: bool = false,
    buffer: [8192]u8 = undefined,

    const Reader = std.io.Reader(*IdatReader, anyerror, read);

    pub fn init(decoder: *Decoder, initial_length: u32) !IdatReader {
        var idat_reader = IdatReader{
            .decoder = decoder,
            .remaining_bytes = initial_length,
            .chunk_started = true,
            .chunk_data = try decoder.al.alloc(u8, initial_length),
        };

        // Read the first chunk
        try decoder.r.readNoEof(idat_reader.chunk_data);
        decoder.crc.update(idat_reader.chunk_data);

        // Verify the CRC for the first chunk
        try decoder.verifyChecksum();

        // Reset the CRC for the next chunk and initialize with IDAT identifier
        decoder.crc = std.hash.Crc32.init();
        const idat_id = "IDAT";
        decoder.crc.update(idat_id);

        // Print first few bytes to confirm zlib header
        std.debug.print("Initial IDAT chunk ({d} bytes), first bytes: ", .{initial_length});
        const debug_bytes = @min(initial_length, 16);
        for (idat_reader.chunk_data[0..debug_bytes]) |b| {
            std.debug.print("{x:0>2} ", .{b});
        }
        std.debug.print("\n", .{});

        // Check for zlib header (0x78 0x9C is most common)
        if (idat_reader.chunk_data.len >= 2 and idat_reader.chunk_data[0] == 0x78) {
            std.debug.print("Valid zlib header detected (0x{x:0>2}{x:0>2})\n", .{ idat_reader.chunk_data[0], idat_reader.chunk_data[1] });
        } else {
            std.debug.print("WARNING: First bytes don't appear to be a standard zlib header\n", .{});
        }

        idat_reader.current_chunk_data = idat_reader.chunk_data;
        return idat_reader;
    }

    pub fn deinit(self: *IdatReader) void {
        self.decoder.al.free(self.chunk_data);
    }

    pub fn reader(self: *IdatReader) Reader {
        return .{ .context = self };
    }

    fn read(self: *IdatReader, buffer: []u8) !usize {
        if (buffer.len == 0) return 0;

        // If we've used all the data in the current chunk
        if (self.current_chunk_data.len == 0) {
            // Verify CRC of the chunk we just finished
            try self.decoder.verifyChecksum();
            std.debug.print("CRC verified for IDAT chunk\n", .{});

            // Try to read the next chunk header
            var header_buf: [8]u8 = undefined;
            self.decoder.r.readNoEof(&header_buf) catch |err| {
                std.debug.print("Error reading next chunk header: {any}\n", .{err});
                return 0; // End of data
            };

            // Print the header bytes
            std.debug.print("Next chunk header: ", .{});
            for (header_buf) |b| {
                std.debug.print("{x:0>2} ", .{b});
            }
            std.debug.print("\n", .{});

            // Check if this is still an IDAT chunk
            if (!std.mem.eql(u8, header_buf[4..8], "IDAT")) {
                std.debug.print("End of IDAT chunks, found: {s}\n", .{header_buf[4..8]});
                return 0; // No more IDAT data
            }

            // Reset CRC for the new chunk and include chunk type in the CRC
            self.decoder.crc = std.hash.Crc32.init();
            self.decoder.crc.update(header_buf[4..8]); // Add chunk type to CRC

            // Read the chunk length
            const chunk_length = std.mem.readInt(u32, header_buf[0..4], .big);
            std.debug.print("Found additional IDAT chunk, length={d}\n", .{chunk_length});

            // Sanity check the length
            if (chunk_length > 10 * 1024 * 1024) { // 10 MB limit
                std.debug.print("Unreasonably large chunk length: {d}\n", .{chunk_length});
                return error.InvalidChunkLength;
            }

            // Read the chunk data
            self.decoder.al.free(self.chunk_data); // Free the old data
            self.chunk_data = try self.decoder.al.alloc(u8, chunk_length);
            try self.decoder.r.readNoEof(self.chunk_data);
            self.decoder.crc.update(self.chunk_data);

            // Debug output
            if (chunk_length > 0) {
                std.debug.print("Additional IDAT data, first bytes: ", .{});
                const debug_len = @min(chunk_length, 16);
                for (self.chunk_data[0..debug_len]) |b| {
                    std.debug.print("{x:0>2} ", .{b});
                }
                std.debug.print("\n", .{});
            }

            self.current_chunk_data = self.chunk_data;
        }

        // Copy data to the output buffer
        const to_copy = @min(buffer.len, self.current_chunk_data.len);
        if (to_copy == 0) return 0;

        @memcpy(buffer[0..to_copy], self.current_chunk_data[0..to_copy]);
        self.current_chunk_data = self.current_chunk_data[to_copy..];

        return to_copy;
    }
};
