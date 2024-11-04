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
    /// General format issue
    InvalidJPEGFormat,
};

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

pub const Decoder = struct {
    stream: *Stream,
    allocator: std.mem.Allocator,

    // frame header fields
    precision: u16,
    dct_components: []DctComponent,
    num_lines: u16,
    samples_per_line: u16,

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
                    data_size += 1;
                },
            }
        } else |err| {
            return err;
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
