const std = @import("std");
const jpeg = @import("jpeg.zig");
const print = @import("std").debug.print;

const HuffTable = @This();

// false = DC (lossless), true = AC
table_class: bool,
// Huffman table destination identifier
id: u8,
// Number of codes for each length (1-16)
code_lengths: [16]u8,
// Values associated with Huffman codes (max 162 values)
values: [162]u8,

num_codes: usize,
// Array of code entries
codes: []CodeEntry,

const CodeEntry = struct {
    // The Huffman code
    code: u16,
    // The length of the code in bits
    length: u8,
    // The value associated with the code
    value: u8,
};

pub fn init(allocator: std.mem.Allocator, tables: *[4]?HuffTable, stream: *jpeg.Stream) !void {
    const table_length = try stream.readU16();

    // Start with 2 bytes for the length
    var bytes_read: usize = 2;
    while (bytes_read < table_length) {
        // Read the class and ID
        const class_and_id = try stream.readByte();
        bytes_read += 1;
        const table_class = (class_and_id & 0b1111_0000) >> 4;
        const id = class_and_id & 0b0000_1111;

        if (id >= 4) {
            return error.InvalidTableIdentifier;
        }

        var ht = HuffTable{
            .table_class = table_class == 1,
            .id = id,
            .code_lengths = undefined,
            .values = undefined,
            .num_codes = 0,
            .codes = undefined, // Initialize as an empty slice
        };

        // Read code lengths
        var total_values: usize = 0;
        for (0..16) |i| {
            ht.code_lengths[i] = try stream.readByte();
            bytes_read += 1;
            total_values += ht.code_lengths[i];
        }

        // Read values
        for (0..total_values) |i| {
            ht.values[i] = try stream.readByte();
            bytes_read += 1;
        }

        // Verify that we haven't read beyond the declared table length
        if (bytes_read > table_length) {
            return error.InvalidTableLength;
        }

        ht.num_codes = total_values;

        // Build the codes array
        ht.codes = try allocator.alloc(HuffTable.CodeEntry, total_values);
        errdefer ht.free(allocator);

        // Corrected Huffman code generation according to the JPEG standard
        // Step 1: Generate huffsize table
        var huffsize = try allocator.alloc(u8, ht.num_codes + 1);
        defer allocator.free(huffsize);

        var p: usize = 0;
        for (0..16) |i| {
            const code_length = i + 1;
            const num_codes = ht.code_lengths[i];

            for (0..num_codes) |_| {
                huffsize[p] = @intCast(code_length);
                p += 1;
            }
        }
        huffsize[p] = 0; // Terminate with zero

        // Step 2: Generate huffcode table
        var huffcode = try allocator.alloc(u16, ht.num_codes);
        defer allocator.free(huffcode);

        var code: u16 = 0;
        var si: u8 = huffsize[0];
        p = 0;
        var k: usize = 0;

        while (huffsize[k] != 0) {
            while (huffsize[k] == si) {
                huffcode[k] = code;
                code += 1;
                k += 1;
            }
            code <<= 1;
            si += 1;
        }

        // Step 3: Assign codes and sizes to ht.codes
        for (0..ht.num_codes) |i| {
            ht.codes[i] = HuffTable.CodeEntry{
                .code = huffcode[i],
                .length = huffsize[i],
                .value = ht.values[i],
            };
        }

        // Cancel the defer once ht is successfully stored
        // errdefer {}

        // Store the HuffTable in the correct slot based on id
        tables[ht.id] = ht;
    }

    // Verify that we've read exactly the number of bytes specified
    if (bytes_read != table_length) {
        return error.InvalidTableLength;
    }
}

pub fn free(self: *HuffTable, allocator: std.mem.Allocator) void {
    print("freeing codes\n", .{});
    allocator.free(self.codes);
}
