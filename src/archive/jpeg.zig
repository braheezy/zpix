//! jpeg package provides a full JPEG decoder
const std = @import("std");
const QuantTable = @import("QuantTable.zig");
const HuffTable = @import("HuffTable.zig");

const oneMB = 1.049E+6;
const decodeLimit = oneMB;

const dbg = std.debug.print;

fn dbgln(comptime fmt: []const u8) void {
    std.debug.print("{s}\n", .{fmt});
}

pub const FileError = error{
    /// SOI marker missing or incorrect
    InvalidSOIMarker,
    /// EOI marker missing or incorrect
    InvalidEOIMarker,
    /// Unexpected end of data in stream
    UnexpectedEndOfData,
    /// Expected marker but failed to read it
    InvalidMarker,
    InvalidHuffmanCode,
    NotEnoughBits,
    BlockOverflow,
    EndOfEntropyData,
    /// General format issue
    InvalidJPEGFormat,
};

const Error = FileError || std.io.AnyReader.Error;

// [Table B.1] Marker code assignments
pub const Marker = enum(u16) {
    SOF0 = 0xFFC0,
    SOF1 = 0xFFC1,
    SOF2 = 0xFFC2,
    DHT = 0xFFC4,
    SOI = 0xFFD8,
    SOS = 0xFFDA,
    EOI = 0xFFD9,
    DQT = 0xFFDB,
    _,
};

const MCU = struct {
    // Array of coefficient blocks for each component
    // Each block is 8x8, represented as 64 coefficients
    // For the Y component (luminance)
    y_blocks: std.ArrayList([64]i16),
    // For the Cb component (chrominance)
    cb_blocks: std.ArrayList([64]i16),
    // For the Cr component (chrominance)
    cr_blocks: std.ArrayList([64]i16),
};

pub const Decoder = struct {
    allocator: std.mem.Allocator,

    // frame header fields
    precision: u16,
    dct_components: []DctComponent,
    num_lines: u16,
    samples_per_line: u16,

    mcus: std.ArrayList(MCU),
    decodeMcu: *const fn (*Decoder) Error!void,

    // Bitstream management
    bit_buffer: u32 = 0,
    bit_count: u8 = 0,

    // Tables
    quant_tables: [4]?QuantTable,
    huff_tables: [4]?HuffTable,

    // Scan header
    // Present at the start of a scan. This header specifies which component(s) are contained in the scan, specifies
    // the destinations from which the entropy tables to be used with each component are retrieved, and (for the
    // progressive DCT) which part of the DCT quantized coefficient data is contained in the scan. For lossless
    // processes the scan parameters specify the predictor and the point transform
    scan_components: []ScanComponent,
    // Start of spectral or predictor selection
    // In the DCT modes of operation, this parameter specifies the first DCT coefficient in each block in zig-zag order
    // which shall be coded in the scan. This parameter shall be set to zero for the sequential DCT processes. In the
    // lossless mode of operations this parameter is used to select the predictor.
    start_selection: u8,
    // End of spectral selection – Specifies the last DCT coefficient in each block in zig-zag order which shall be
    // coded in the scan. This parameter shall be set to 63 for the sequential DCT processes. In the lossless mode of
    // operations this parameter has no meaning. It shall be set to zero.
    end_selection: u8,
    // Successive approximation bit position high – This parameter specifies the point transform used in the
    // preceding scan (i.e. successive approximation bit position low in the preceding scan) for the band of
    // coefficients specified by Ss and Se. This parameter shall be set to zero for the first scan of each band of
    // coefficients. In the lossless mode of operations this parameter has no meaning. It shall be set to zero.
    successive_approx_high_bit: u8,
    // Successive approximation bit position low or point transform – In the DCT modes of operation this
    // parameter specifies the point transform, i.e. bit position low, used before coding the band of coefficients
    // specified by Ss and Se. This parameter shall be set to zero for the sequential DCT processes. In the lossless
    // mode of operations, this parameter specifies the point transform.
    successive_approx_low_bit: u8,

    const DctComponent = struct {
        // C: component identifier
        // Used in scan headers to identify the components in the scan.
        id: u8,
        // H and V combined: horizontal and vertical sampling factor
        // H will be in high nibble, V in low nibble.
        // Specifies relationship between component dimension and max image dimension.
        sampling_factors: u8,
        // Tq: quantization table destination factor
        // Specifies one of four of the quant tables to use when scans contain this component.
        quant_table_id: u8,
    };

    const ScanComponent = struct {
        // Scan component selector
        // selects which of the Nf image components specified in the frame parameters
        selector: u8,
        // DC entropy coding table destination selector. The DC table must already be installed to the decoder.
        dc_table_selector: u8,
        // AC entropy coding table destination selector. The AC table must already be installed to the decoder.
        // 0 for lossless.
        ac_table_selector: u8,

        // Component state variables
        // Previous DC coefficient for differential coding
        prev_dc: i16 = 0,
        // Current position in the block (0..63)
        block_pos: u8 = 0,
        // DCT coefficients for the current block
        coefficients: [64]i16 = undefined,
    };

    /// Processes JPEG markers in the stream and decodes the image data.
    /// Reads markers sequentially and handles them according to their type.
    pub fn processMarkers(self: *Decoder) !DecodedImage {
        while (true) {
            const marker = try self.stream.readMarker();
            std.log.debug("marker: {x}", .{marker});

            switch (marker) {
                // Start of image
                .SOI => continue,
                // Baseline DCT
                .SOF0 => {
                    try self.decodeDctHeader();
                    self.decodeMcu = Decoder.decodeMcuBaselineDct;
                },
                // Define quantization table(s)
                .DQT => try self.decodeDctTables(),
                // Define Huffman table(s)
                .DHT => try self.decodeDhtTables(),
                // Start of scan
                .SOS => {
                    try self.decodeScanHeader();
                    try self.decodeSegment();
                },
                // End of image
                .EOI => {
                    dbgln("done!");
                    dbg("mcus: {d}\n", .{self.mcus.items.len});
                    return DecodedImage{ .width = 1, .height = 1 };
                },
                else => try self.stream.readLengthAndSkip(),
            }
        }
    }

    fn decodeSegment(self: *Decoder) !void {
        // TODO: improve performance by reading more than 1 byte at a time
        var data_size: u32 = 0;

        while (self.stream.readByteWithMarkerCheck(&self.bit_buffer, &self.bit_count)) |_| {
            self.callDecodeMcu() catch |err| {
                if (err == FileError.EndOfEntropyData) {
                    dbg("read {d} bytes of entropy data\n", .{data_size});
                    return;
                } else {
                    return err;
                }
            };
            data_size += 1;
        } else |err| {
            dbg("Read {d} bytes of entropy data\n", .{data_size});
            return err;
        }
    }

    fn callDecodeMcu(self: *Decoder) Error!void {
        // as an amatuer zig programmer, why does self need to pass self to the function?
        return self.decodeMcu(self);
    }

    fn decodeMcuBaselineDct(self: *Decoder) Error!void {
        // This function processes the ECS data for a single MCU in a baseline sequential JPEG

        var mcu = MCU{
            .y_blocks = std.ArrayList([64]i16).init(self.allocator),
            .cb_blocks = std.ArrayList([64]i16).init(self.allocator),
            .cr_blocks = std.ArrayList([64]i16).init(self.allocator),
        };

        // Iterate over each scan component
        for (self.scan_components) |*component| {
            // Find the corresponding DCT component to get sampling factors
            const dct_component = self.dct_components[component.selector - 1];
            const h_sampling = dct_component.sampling_factors >> 4;
            const v_sampling = dct_component.sampling_factors & 0x0F;
            const blocks_per_mcu = h_sampling * v_sampling;

            for (0..blocks_per_mcu) |_| {
                // Decode DC coefficient
                var selected_dc_table = self.huff_tables[component.dc_table_selector] orelse return error.InvalidComponentSelector;
                const dc_diff = try self.decodeDc(&selected_dc_table);
                const dc = component.prev_dc + dc_diff;
                component.*.prev_dc = dc;

                // Initialize block with DC coefficient
                var block: [64]i16 = undefined;
                block[0] = dc;

                // Decode AC coefficients
                var k: usize = 1;
                while (k < 64) {
                    var selected_ac_table = self.huff_tables[component.ac_table_selector] orelse return error.InvalidComponentSelector;
                    const ac_coeff = try self.decodeAc(&selected_ac_table);

                    if (ac_coeff.run_length == 0 and ac_coeff.amplitude == 0) {
                        // End of Block (EOB)
                        break;
                    } else {
                        k += ac_coeff.run_length;
                        if (k >= 64) break;
                        block[k] = ac_coeff.amplitude; // Store the coefficient at the correct position
                        k += 1;
                    }
                }

                // Append the block to the appropriate component
                switch (component.selector) {
                    1 => try mcu.y_blocks.append(block),
                    2 => try mcu.cb_blocks.append(block),
                    3 => try mcu.cr_blocks.append(block),
                    else => return error.InvalidComponentSelector,
                }
            }
        }

        // Add the decoded MCU to the list
        try self.mcus.append(mcu);
    }

    fn decodeDc(self: *Decoder, huff_table: *HuffTable) !i16 {
        // Step 1: Decode the Huffman symbol to get 's'
        const s = try self.decodeHuffmanSymbol(huff_table);

        // Step 2: If 's' is zero, the DC difference is zero
        if (s == 0) {
            return 0;
        } else if (s > 16) {
            return error.InvalidDcSize;
        }

        // Step 3: Read 's' additional bits
        const diff = try self.receiveBits(s);

        return diff;
    }

    // Stub function for decodeHuffmanSymbol
    fn decodeHuffmanSymbol(self: *Decoder, huff_table: *HuffTable) !u8 {
        var code: u16 = 0;
        var code_length: u8 = 0;

        while (code_length < 16) {
            // Ensure there's at least one bit in the buffer
            if (self.bit_count == 0) {
                // Read the next byte, handling marker checks
                try self.stream.readByteWithMarkerCheck(&self.bit_buffer, &self.bit_count);
            }

            // Extract the next bit
            self.bit_count -= 1;
            const next_bit = (self.bit_buffer >> @intCast(self.bit_count)) & 1;
            const casted_next_bit: u16 = @intCast(next_bit);
            code = (code << 1) | casted_next_bit;
            code_length += 1;

            // Search for the code in the Huffman table
            for (huff_table.codes) |code_entry| {
                if (code_entry.length == code_length and code_entry.code == code) {
                    std.debug.print("Matched code: code={d}, length={d}, value={d}\n", .{ code, code_length, code_entry.value });
                    return code_entry.value;
                }
            }
        }

        return error.InvalidHuffmanCode;
    }

    const AcCoefficient = struct {
        run_length: u8,
        amplitude: i16,
    };

    fn decodeAc(self: *Decoder, huff_table: *HuffTable) !AcCoefficient {
        // Decode a Huffman symbol from the bitstream
        const symbol = try self.decodeHuffmanSymbol(huff_table);

        if (symbol == 0x00) {
            // End of Block (EOB)
            return AcCoefficient{ .run_length = 0, .amplitude = 0 };
        } else if (symbol == 0xF0) {
            // Zero Run Length of 16 zeros (ZRL)
            return AcCoefficient{ .run_length = 16, .amplitude = 0 };
        } else {
            const run = symbol >> 4;
            const size = symbol & 0x0F;

            if (size > 16) {
                return error.InvalidAcSize;
            }

            var amplitude: i16 = 0;
            if (size > 0) {
                amplitude = try self.receiveBits(size);
            }

            return AcCoefficient{ .run_length = run, .amplitude = amplitude };
        }
    }

    fn receiveBits(self: *Decoder, size: u8) !i16 {
        if (size == 0 or size > 16) {
            return error.InvalidBitSize;
        }

        var value: u32 = 0;
        const original_size = size;
        var size_remaining = size;

        while (size_remaining > 0) {
            // Ensure there are enough bits in the buffer
            if (self.bit_count < size_remaining) {
                // Read the next byte, handling marker checks
                try self.stream.readByteWithMarkerCheck(&self.bit_buffer, &self.bit_count);
            }

            // Calculate the number of bits to extract in this iteration
            const bits_to_extract = if (self.bit_count >= size_remaining) size_remaining else self.bit_count;

            // Shift the buffer to align the bits to extract
            const shift_amount = self.bit_count - bits_to_extract;
            const extracted_bits = (self.bit_buffer >> @intCast(shift_amount)) & ((@as(u16, 1) << @intCast(bits_to_extract)) - 1);

            // Append the extracted bits to the value
            value = (value << @intCast(bits_to_extract)) | extracted_bits;

            // Update the buffer and bit count
            self.bit_buffer &= (@as(u32, 1) << @intCast(shift_amount)) - 1;
            self.bit_count -= bits_to_extract;
            size_remaining -= bits_to_extract;
        }

        // Sign-extend the value if necessary
        if (value & (@as(u32, 1) << @intCast(original_size - 1)) != 0) {
            value = value - (@as(u32, 1) << @intCast(original_size));
        }

        return @intCast(value);
    }

    // Define the standard JPEG zig-zag order for an 8x8 block
    const ZIGZAG_ORDER: [64]usize = .{
        0,  1,  5,  6,  14, 15, 27, 28,
        2,  4,  7,  13, 16, 26, 29, 42,
        3,  8,  12, 17, 25, 30, 41, 43,
        9,  11, 18, 24, 31, 40, 44, 53,
        10, 19, 23, 32, 39, 45, 52, 54,
        20, 22, 33, 38, 46, 51, 55, 60,
        21, 34, 37, 47, 50, 56, 59, 61,
        35, 36, 48, 49, 57, 58, 62, 63,
    };

    // Function to map the current position in the zig-zag sequence to the block index
    fn zigzagDecode(position: usize) !usize {
        if (position >= ZIGZAG_ORDER.len) {
            return error.InvalidZigZagPosition;
        }

        return ZIGZAG_ORDER[position];
    }

    // fn decodeHuffmanSymbol(
    //     self: *Decoder,
    //     ht: HuffTable,
    //     bit_buffer: *u32,
    //     bit_count: *u8,
    // ) Error!u8 {
    //     var code: u16 = 0;
    //     var s: u8 = 1;
    //     while (s <= 16) : (s += 1) {
    //         const bit = try self.readBit(bit_buffer, bit_count);
    //         code = (code << 1) | bit;

    //         if (code >= ht.min_code[s] and code <= ht.max_code[s]) {
    //             const index = ht.val_ptr[s] + (code - ht.min_code[s]);
    //             return ht.values[index];
    //         }
    //         dbg("Invalid Huffman code: {x}, min_code: {x}, max_code: {x}\n", .{ code, ht.min_code[s], ht.max_code[s] });
    //     }

    //     return error.InvalidHuffmanCode;
    // }

    fn readBit(self: *Decoder, bit_buffer: *u32, bit_count: *u8) Error!u8 {
        return @intCast(try self.readBits(bit_buffer, bit_count, 1));
    }
    fn readBits(self: *Decoder, bit_buffer: *u32, bit_count: *u8, num_bits: u8) Error!u32 {
        // dbg("reading {d} bits from {d}\n", .{ num_bits, bit_buffer });
        if (num_bits == 0) {
            return 0;
        }
        if (num_bits > 16) {
            dbg("Invalid bit count: {d}\n", .{num_bits});
            return error.InvalidBitCount;
        }
        while (bit_count.* < num_bits) {
            try self.stream.readByteWithMarkerCheck(bit_buffer, bit_count);
        }

        // Calculate shift_amount before modifying bit_count.*
        const shift_amount: u8 = bit_count.* - num_bits;
        const mask_shift_amount: u5 = @intCast(num_bits);
        const buffer_shift_amount: u5 = @intCast(shift_amount);

        const mask = (@as(u32, 1) << mask_shift_amount) - 1;
        const bits = (bit_buffer.* >> buffer_shift_amount) & mask;

        bit_count.* -= num_bits;
        return bits;
    }

    fn extendSign(value: u32, num_bits: u8) i16 {
        if (num_bits == 0) return 0;

        // Cast num_bits - 1 to u5 for the shift amount
        const shift_amount_minus1: u5 = @intCast(num_bits - 1);
        // Cast 1 to u32 to make LHS a fixed-width integer
        const sign_bit: u32 = @as(u32, 1) << shift_amount_minus1;

        // Perform the initial cast ofa'value' to i32
        var extended_value: i32 = @intCast(value);

        if ((value & sign_bit) != 0) {
            // Negative number, perform sign extension
            const shift_amount: u5 = @intCast(num_bits);
            const value_to_subtract: i32 = @as(i32, @as(i32, 1) << shift_amount);
            extended_value = extended_value - value_to_subtract;
        }

        // Cast the result to i16
        return @intCast(extended_value);
    }

    // fn placeSymbol(
    //     self: *Decoder,
    //     symbol: u8,
    //     component: *ScanComponent,
    //     bit_buffer: *u32,
    //     bit_count: *u8,
    // ) !void {
    //     // If we are at the beginning of the block, process the DC coefficient
    //     if (component.block_pos == 0) {
    //         const num_bits = symbol;

    //         var dc_diff: i16 = 0;
    //         if (num_bits > 0) {
    //             const bits = try self.readBits(bit_buffer, bit_count, num_bits);
    //             // Sign extend the bits to get the correct value
    //             dc_diff = extendSign(bits, num_bits);
    //         }

    //         // Add the differential to the previous DC value
    //         const dc_coefficient = component.prev_dc + dc_diff;

    //         // Update the previous DC value
    //         component.prev_dc = dc_coefficient;

    //         // Store the DC coefficient at the first position in the block
    //         component.coefficients[0] = dc_coefficient;

    //         // Update the block position
    //         component.block_pos += 1;
    //     } else {
    //         // Process the AC coefficients
    //         var block_pos = component.block_pos;

    //         if (symbol == 0x00) {
    //             // EOB (End of Block): Fill the rest of the block with zeros
    //             while (block_pos < 64) : (block_pos += 1) {
    //                 component.coefficients[block_pos] = 0;
    //             }
    //             component.block_pos = block_pos;
    //         } else if (symbol == 0xF0) {
    //             // ZRL (Zero Run Length): Skip 16 zeros
    //             for (0..16) |_| {
    //                 if (block_pos >= 64) {
    //                     return FileError.BlockOverflow;
    //                 }
    //                 component.coefficients[block_pos] = 0;
    //                 block_pos += 1;
    //             }
    //             component.block_pos = block_pos;
    //         } else {
    //             // Decode the run-length and size of the coefficient
    //             const run_length = symbol >> 4;
    //             const size = symbol & 0x0F;

    //             // Skip over zeros for the run-length
    //             for (0..run_length) |_| {
    //                 if (block_pos >= 64) {
    //                     return FileError.BlockOverflow;
    //                 }
    //                 component.coefficients[block_pos] = 0;
    //                 block_pos += 1;
    //             }

    //             // Read the coefficient value
    //             var ac_value: i16 = 0;
    //             if (size > 0) {
    //                 const bits = try self.readBits(bit_buffer, bit_count, size);
    //                 ac_value = extendSign(bits, size);
    //             }

    //             // Place the coefficient
    //             if (block_pos >= 64) {
    //                 return FileError.BlockOverflow;
    //             }
    //             component.coefficients[block_pos] = ac_value;
    //             block_pos += 1;

    //             // Update the block position
    //             component.block_pos = block_pos;
    //         }
    //     }
    // }

    /// Parses the stream to find the SOF marker and ensuing frame header info
    fn decodeDctHeader(self: *Decoder) !void {
        // [B.2.2] Frame header syntax
        // | SOFn | Lf | P | Y | X | Nf | Component-specification parameters |
        const frame_length = try self.stream.readU16();
        self.precision = try self.stream.readByte();
        self.num_lines = try self.stream.readU16();
        self.samples_per_line = try self.stream.readU16();
        const num_components = try self.stream.readByte();

        if ((num_components * 3) + 8 != frame_length) {
            return FileError.InvalidJPEGFormat;
        }

        const components = try self.allocator.alloc(DctComponent, num_components);
        for (components) |*component| {
            component.id = try self.stream.readByte();
            component.sampling_factors = try self.stream.readByte();
            component.quant_table_id = try self.stream.readByte();
        }
        self.dct_components = components;
    }

    fn decodeDctTables(self: *Decoder) !void {
        try QuantTable.init(&self.quant_tables, self.stream);
    }

    fn decodeDhtTables(self: *Decoder) !void {
        try HuffTable.init(self.allocator, &self.huff_tables, self.stream);
    }

    fn decodeScanHeader(self: *Decoder) !void {
        // [B.2.3] Scan header syntax
        // | SOS | Ls | Ns | component-spec | Ss | Se | Ah | Af |

        _ = try self.stream.readU16();
        const num_components = try self.stream.readByte();

        self.scan_components = try self.allocator.alloc(ScanComponent, num_components);

        for (0..num_components) |i| {
            const component_selector = try self.stream.readByte();
            const dc_and_ac_table_selector = try self.stream.readByte();

            const dc_table_selector = (dc_and_ac_table_selector & 0b1111_0000) >> 4;
            const ac_table_selector = dc_and_ac_table_selector & 0b0000_1111;

            self.scan_components[i] = ScanComponent{
                .selector = component_selector,
                .dc_table_selector = dc_table_selector,
                .ac_table_selector = ac_table_selector,
            };
        }

        self.start_selection = try self.stream.readByte();
        self.end_selection = try self.stream.readByte();
        const successive_approx = try self.stream.readByte();
        self.successive_approx_high_bit = (successive_approx & 0b1111_0000) >> 4;
        self.successive_approx_low_bit = successive_approx & 0b0000_1111;
    }

    fn free(self: *Decoder) void {
        self.allocator.free(self.dct_components);
        self.allocator.free(self.scan_components);
        self.mcus.deinit();
        for (self.huff_tables, 0..) |table, i| {
            if (table) |_| {
                self.huff_tables[i].?.free(self.allocator);
            }
        }
    }
};

const zigzag_order = [_]u8{
    0,  1,  5,  6,  14, 15, 27, 28,
    2,  4,  7,  13, 16, 26, 29, 42,
    3,  8,  12, 17, 25, 30, 41, 43,
    9,  11, 18, 24, 31, 40, 44, 53,
    10, 19, 23, 32, 39, 45, 52, 54,
    20, 22, 33, 38, 46, 51, 55, 60,
    21, 34, 37, 47, 50, 56, 59, 61,
    35, 36, 48, 49, 57, 58, 62, 63,
};

fn zigzagReorder(input: [64]i16) [64]i16 {
    var output: [64]i16 = undefined;
    for (0..64) |i| {
        const zigzag_index = zigzag_order[i];
        output[zigzag_index] = input[i];
    }
    return output;
}

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
            return FileError.InvalidMarker;
        }
        return @enumFromInt(@as(u16, byte1) << 8 | @as(u16, byte2));
    }

    fn readByteWithMarkerCheck(self: *Stream, bit_buffer: *u32, bit_count: *u8) Error!void {
        const byte = try self.readByte();
        if (byte == 0xFF) {
            const next_byte = try self.readByte();
            if (next_byte != 0) {
                // Restore stream position for just consumed marker
                try self.inner.seekBy(-2);
                return error.EndOfEntropyData;
            } else {
                // Append a single 0xFF for the stuffed byte
                bit_buffer.* = (bit_buffer.* << 8) | 0xFF;
                bit_count.* += 8;
                return;
            }
        }
        bit_buffer.* = (bit_buffer.* << 8) | byte;
        bit_count.* += 8;
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
    try validateFile(jpeg_file);

    var stream = Stream{ .inner = jpeg_file.seekableStream() };
    var decoder = Decoder{
        .allocator = allocator,
        .stream = &stream,
        .precision = 0,
        .dct_components = undefined,
        .num_lines = 0,
        .samples_per_line = 0,
        .quant_tables = .{null} ** 4,
        .huff_tables = .{null} ** 4,
        .scan_components = undefined,
        .start_selection = 0,
        .end_selection = 0,
        .successive_approx_high_bit = 0,
        .successive_approx_low_bit = 0,
        .decodeMcu = undefined,
        .mcus = std.ArrayList(MCU).init(allocator),
    };
    defer decoder.free();

    const result = decoder.processMarkers();

    return result;
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
