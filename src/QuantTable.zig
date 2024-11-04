const jpeg = @import("jpeg.zig");

const QuantTable = @This();

// Quantization table destination identifier. Referenced by later decoding steps to load specific tables.
id: u8,
// Precision of table elements. False -> 8-bit, true -> 16-bit
precision: bool,
// quantization values for DCT coefficients
// u16 to fit both 8bit and 16bit values
values: [64]u16,

pub fn init(tables: *[4]?QuantTable, stream: *jpeg.Stream) !void {
    // [B.2.4.1] Quantization table-specification syntax
    // | DQT | Lq | Pq | Tq | Q0...Q63|
    // The Pq, Tq, and Q portions can repeat several times, each a different table.
    const table_length = try stream.readU16();

    // Read bytes as specified in the table length, using the precision to know when a new
    // table defintion is beginning.
    var bytes_read: usize = 0;
    // - 2 to account for table length bytes
    while (bytes_read < table_length - 2) {
        const precision_and_id = try stream.readByte();
        // Break up nibbles
        const precision = (precision_and_id & 0b1111_0000) >> 4;
        const id = precision_and_id & 0b0000_1111;

        var qt = QuantTable{
            .id = id,
            .precision = precision == 1,
            .values = undefined,
        };

        for (0..64) |i| {
            qt.values[i] = if (qt.precision) try stream.readU16() else try stream.readByte();
        }

        const qk_size: usize = if (qt.precision) 2 else 1;
        // 1 byte for precision_and_id, 64 * size of each Qk value
        bytes_read += 1 + (64 * qk_size);

        tables[qt.id] = qt;
    }
}
