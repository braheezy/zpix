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
    y_blocks: [][]i16,
    // For the Cb component (chrominance)
    cb_blocks: [][]i16,
    // For the Cr component (chrominance)
    cr_blocks: [][]i16,
};

pub const Decoder = struct {
    stream: *Stream,
    allocator: std.mem.Allocator,

    // frame header fields
    precision: u16,
    dct_components: []DctComponent,
    num_lines: u16,
    samples_per_line: u16,

    mcus: []MCU,
    decodeMcu: *const fn (*Decoder, *u32, *u8) Error!void,

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
                    return DecodedImage{ .width = 1, .height = 1 };
                },
                else => try self.stream.readLengthAndSkip(),
            }
        }
    }

    fn decodeSegment(self: *Decoder) !void {
        // TODO: improve performance by reading more than 1 byte at a time
        var data_size: u32 = 0;
        // Buffer to store bits read from the stream
        var bit_buffer: u32 = 0;
        // // Number of valid bits in the bit_buffer
        var bit_count: u8 = 0;

        while (self.stream.readByte()) |byte| {
            switch (byte) {
                0xFF => {
                    const next_byte = try self.stream.readByte();
                    if (next_byte != 0) {
                        // Restore stream position for just consumed marker and let processMarkers deal with it
                        try self.stream.inner.seekBy(-2);
                        dbg("need to parse {d} bytes of entropy data\n", .{data_size});
                        return;
                    }
                },
                else => {
                    bit_buffer = (bit_buffer << 8) | byte;
                    bit_count += 8;
                    try self.callDecodeMcu(&bit_buffer, &bit_count);
                    data_size += 1;
                },
            }
        } else |err| {
            dbg("Read {d} bytes of entropy data\n", .{data_size});
            return err;
        }
    }

    fn callDecodeMcu(self: *Decoder, bit_buffer: *u32, bit_count: *u8) Error!void {
        // as an amatuer zig programmer, why does self need to pass self to the function?
        return self.decodeMcu(self, bit_buffer, bit_count);
    }

    fn decodeMcuBaselineDct(self: *Decoder, bit_buffer: *u32, bit_count: *u8) Error!void {
        // This function processes the ECS data for a single MCU in a baseline sequential JPEG
        for (self.scan_components) |*component| {
            // Reset component state
            component.block_pos = 0;
            component.coefficients = [_]i16{0} ** 64;

            // Process DC coefficient
            const dc_huff_table = self.huff_tables[component.dc_table_selector] orelse {
                return FileError.InvalidHuffmanCode;
            };

            const dc_symbol = try self.decodeHuffmanSymbol(dc_huff_table, bit_buffer, bit_count);

            try self.placeSymbol(dc_symbol, component, bit_buffer, bit_count);

            // Process AC coefficients
            const ac_huff_table = self.huff_tables[component.ac_table_selector] orelse {
                return FileError.InvalidHuffmanCode;
            };

            while (component.block_pos < 64) {
                const ac_symbol = try self.decodeHuffmanSymbol(ac_huff_table, bit_buffer, bit_count);
                try self.placeSymbol(ac_symbol, component, bit_buffer, bit_count);

                // Break if EOB symbol was processed
                if (ac_symbol == 0x00) {
                    break;
                }
            }
        }
    }

    fn decodeHuffmanSymbol(
        self: *Decoder,
        table: HuffTable,
        bit_buffer: *u32,
        bit_count: *u8,
    ) Error!u8 {
        var code: u32 = 0;
        var length: u8 = 0;

        while (length < 16) {
            if (bit_count.* == 0) {
                const byte = try self.stream.readByte();
                bit_buffer.* = (bit_buffer.* << 8) | byte;
                bit_count.* += 8;
            }

            code = (code << 1) | ((bit_buffer.* >> @intCast(bit_count.* - 1)) & 1);
            bit_count.* -= 1;
            length += 1;

            if (code < (@as(u32, 1) << @intCast(length))) {
                var offset: usize = 0;
                for (0..length) |i| {
                    offset += table.code_lengths[i];
                }
                var base_code: u32 = 0;
                for (0..length) |i| {
                    base_code = (base_code << 1) + table.code_lengths[i];
                }
                if (code == 0) {
                    return table.values[offset];
                } else {
                    return table.values[offset + (code - base_code)];
                }
            }
        }

        return FileError.InvalidHuffmanCode;
    }

    fn readBits(self: *Decoder, bit_buffer: *u32, bit_count: *u8, num_bits: u8) Error!u32 {
        // dbg("reading {d} bits from {d}\n", .{ num_bits, bit_buffer });
        while (bit_count.* < num_bits) {
            const byte = try self.stream.readByte();
            bit_buffer.* = (bit_buffer.* << 8) | byte;
            bit_count.* += 8;
        }
        const shift_amount: u5 = @intCast(bit_count.* - num_bits);
        const mask = (@as(u32, 1) << @intCast(num_bits)) - 1;
        const bits = (bit_buffer.* >> shift_amount) & mask;
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

    fn placeSymbol(
        self: *Decoder,
        symbol: u8,
        component: *ScanComponent,
        bit_buffer: *u32,
        bit_count: *u8,
    ) !void {
        // If we are at the beginning of the block
        if (component.block_pos == 0) {
            // This is the DC coefficient
            const num_bits = symbol;

            var dc_diff: i16 = 0;
            if (num_bits > 0) {
                const bits = try self.readBits(bit_buffer, bit_count, num_bits);
                // Sign extend
                dc_diff = extendSign(bits, num_bits);
            }

            // Add to previous DC value
            const dc_coefficient = component.prev_dc + dc_diff;

            // Update previous DC value
            component.prev_dc = dc_coefficient;

            // Store the DC coefficient in the block
            component.coefficients[0] = dc_coefficient;

            // Update block position
            component.block_pos += 1;
        } else {
            // This is an AC coefficient
            var block_pos = component.block_pos;

            if (symbol == 0x00) {
                // EOB: Fill the rest of the block with zeros
                while (block_pos < 64) : (block_pos += 1) {
                    component.coefficients[block_pos] = 0;
                }
                component.block_pos = block_pos;
            } else if (symbol == 0xF0) {
                // ZRL: Skip 16 zeros
                for (0..16) |_| {
                    if (block_pos >= 64) {
                        return FileError.BlockOverflow;
                    }
                    component.coefficients[block_pos] = 0;
                    block_pos += 1;
                }
                component.block_pos = block_pos;
            } else {
                // The upper 4 bits represent the run-length of zeros
                const run_length = symbol >> 4;
                // The lower 4 bits represent the size in bits of the coefficient
                const size = symbol & 0x0F;

                // Skip zeros
                for (0..run_length) |_| {
                    if (block_pos >= 64) {
                        return FileError.BlockOverflow;
                    }
                    component.coefficients[block_pos] = 0;
                    block_pos += 1;
                }

                // Read the coefficient value
                var ac_value: i16 = 0;
                if (size > 0) {
                    _ = self.readBits(bit_buffer, bit_count, size) catch {
                        const bits = try self.readBits(bit_buffer, bit_count, size);
                        ac_value = extendSign(bits, size);
                    };

                    if (block_pos >= 64) {
                        return FileError.BlockOverflow;
                    }

                    // Store the coefficient
                    component.coefficients[block_pos] = ac_value;
                    block_pos += 1;
                    component.block_pos = block_pos;
                }
            }
        }
    }

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
        try HuffTable.init(&self.huff_tables, self.stream);
        // if (self.huff_tables[0]) |huff| {
        //     dbg("peek huff 1: {any}\n", .{huff.values[0..3]});
        // } else {
        //     dbgln("huff 1 still null");
        // }
        // if (self.huff_tables[1]) |huff| {
        //     dbg("peek huff 2: {any}\n", .{huff.values[0..3]});
        // } else {
        //     dbgln("huff 2 still null");
        // }
        // if (self.huff_tables[2]) |huff| {
        //     dbg("peek huff 3: {any}\n", .{huff.values[0..3]});
        // } else {
        //     dbgln("huff 3 still null");
        // }
        // if (self.huff_tables[3]) |huff| {
        //     dbg("peek huff 4: {any}\n", .{huff.values[0..3]});
        // } else {
        //     dbgln("huff 4 still null");
        // }
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

            // dbg("Component {d}: id={d}, dc_table={d}, ac_table={d}\n", .{
            //     i,
            //     component_selector,
            //     dc_table_selector,
            //     ac_table_selector,
            // });
        }

        self.start_selection = try self.stream.readByte();
        self.end_selection = try self.stream.readByte();
        const successive_approx = try self.stream.readByte();
        self.successive_approx_high_bit = (successive_approx & 0b1111_0000) >> 4;
        self.successive_approx_low_bit = successive_approx & 0b0000_1111;

        // dbg("Ss: {d}, Se: {d}, Ah: {d}, Al: {d}\n", .{
        //     self.start_selection,
        //     self.end_selection,
        //     self.successive_approx_high_bit,
        //     self.successive_approx_low_bit,
        // });
    }

    fn free(self: *Decoder) void {
        self.allocator.free(self.dct_components);
        self.allocator.free(self.scan_components);
    }
};

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
        .mcus = undefined,
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
