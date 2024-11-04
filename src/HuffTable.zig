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

pub fn init(tables: *[4]?HuffTable, stream: *jpeg.Stream) !void {
    const table_length = try stream.readU16();

    var bytes_read: usize = 0;
    // - 2 to account for table length bytes
    while (bytes_read < table_length - 2) {
        const class_and_id = try stream.readByte();
        // Break up nibbles
        const table_class = (class_and_id & 0b1111_0000) >> 4;
        const id = class_and_id & 0b0000_1111;

        var ht = HuffTable{
            .table_class = table_class == 1,
            .id = id,
            .code_lengths = undefined,
            .values = undefined,
        };

        var total_values: usize = 0;
        for (0..16) |i| {
            ht.code_lengths[i] = try stream.readByte();
            total_values += ht.code_lengths[i];
        }

        // Read `total_values` bytes for the values associated with each Huffman code length
        for (0..total_values) |i| {
            ht.values[i] = try stream.readByte();
        }

        // 1 byte for `class_and_id`, 16 bytes for `code_lengths`, `total_values` bytes for `values`
        bytes_read += 1 + 16 + total_values;

        // Store the table in the correct slot (based on `id` and `table_class`)
        tables[ht.id] = ht;
    }
}
