const std = @import("std");
const assert = std.debug.assert;

pub const FilterType = enum(u8) {
    none = 0,
    sub = 1,
    up = 2,
    average = 3,
    paeth = 4,
    // A synthetic filter used reverse-filter implementation.
    avg0 = 5,
};

// Function type for inverse filters
pub const InverseFilterFn = *const fn (
    []u8, // current row
    u32, // bytes per pixel
    u32, // bytes per line
    u32, // height
) anyerror!void;

// Function table for inverse filters
pub const InverseFilterTable = struct {
    filters: [5]InverseFilterFn, // One for each filter type

    pub fn init() InverseFilterTable {
        return .{
            .filters = [_]InverseFilterFn{
                inverseFilter, // none
                inverseFilter, // sub
                inverseFilter, // up
                inverseFilter, // average
                inverseFilter, // paeth
            },
        };
    }
};

fn inverseFilter(current_row: []u8, bytes_per_pixel: u32, bytes_per_line: u32, height: u32) !void {
    assert(bytes_per_pixel > 0);
    assert(bytes_per_line > 1);
    assert(height > 0);

    // Subtract one BYTE that is used to store the `filter` ID - it's always processed and not part of pixel data.
    const bpl = bytes_per_line - 1;
    var row_idx: usize = 0;
    var prev_row_idx: usize = 0;
    var remaining_rows: usize = height;

    // First row uses a special filter that doesn't access the previous row,
    // which is assumed to contain all zeros.
    var filter_type: FilterType = @enumFromInt(current_row[row_idx]);
    row_idx += 1;

    if (@intFromEnum(filter_type) >= 5) {
        filter_type = .none;
    }

    filter_type = @enumFromInt(simplifyFilterOfFirstRow(@intFromEnum(filter_type)));

    while (true) {
        switch (filter_type) {
            .sub => {
                var i: u32 = bpl - bytes_per_pixel;
                while (i > 0) : (i -= 1) {
                    const pos = row_idx + i - 1;
                    current_row[pos + bytes_per_pixel] = applySumFilter(current_row[pos + bytes_per_pixel], current_row[pos]);
                }
                row_idx += bpl;
            },

            .up => {
                var i: u32 = bpl;
                while (i > 0) : (i -= 1) {
                    const pos = row_idx + (i - 1);
                    current_row[pos] = applySumFilter(current_row[pos], current_row[prev_row_idx + (i - 1)]);
                }
                row_idx += bpl;
                prev_row_idx += bpl;
            },

            .average => {
                var i: usize = 0;
                while (i < bytes_per_pixel) : (i += 1) {
                    current_row[row_idx + i] = applySumFilter(current_row[row_idx + i], current_row[prev_row_idx + i] >> 1);
                }
                // Remaining bytes
                i = bytes_per_pixel;
                while (i < bpl) : (i += 1) {
                    current_row[row_idx + i] = applySumFilter(current_row[row_idx + i], applyAvgFilter(current_row[row_idx + i - bytes_per_pixel], current_row[prev_row_idx + i]));
                }
                row_idx += bpl;
                prev_row_idx += bpl;
            },

            .paeth => {
                // First bytes_per_pixel bytes
                var i: usize = 0;
                while (i < bytes_per_pixel) : (i += 1) {
                    current_row[row_idx + i] = applySumFilter(current_row[row_idx + i], current_row[prev_row_idx + i]);
                }
                // Remaining bytes
                i = bytes_per_pixel;
                while (i < bpl) : (i += 1) {
                    current_row[row_idx + i] = applySumFilter(current_row[row_idx + i], applyPaethFilter(current_row[row_idx + i - bytes_per_pixel], current_row[prev_row_idx + i - bytes_per_pixel], current_row[prev_row_idx + i]));
                }
                row_idx += bpl;
                prev_row_idx += bpl;
            },

            .avg0 => {
                var i: usize = bytes_per_pixel;
                while (i < bpl) : (i += 1) {
                    current_row[row_idx + i] = applySumFilter(current_row[row_idx + i], current_row[row_idx + i - bytes_per_pixel] >> 1);
                }
                row_idx += bpl;
            },
            .none => row_idx += bpl,
        }

        // Decrement row counter and break if we're done
        remaining_rows -= 1;
        if (remaining_rows == 0) {
            break;
        }

        // Get filter type for the next row
        filter_type = @enumFromInt(current_row[row_idx]);
        row_idx += 1;

        // Validate filter type
        if (@intFromEnum(filter_type) >= 5) {
            filter_type = .none;
        }
    }
}

// Performs PNG sum filter and casts to byte.
fn applySumFilter(a: u32, b: u32) u8 {
    return @intCast((a + b) & 0xFF);
}

// Performs PNG average filter.
fn applyAvgFilter(a: u8, b: u8) u8 {
    return (a + b) / 2;
}

// Performs PNG paeth filter.
fn applyPaethFilter(a: u32, b: u32, c: u32) u32 {
    const threshold = @as(i32, @intCast(c * 3)) - @as(i32, @intCast(a + b));

    const minAB = @min(a, b);
    const maxAB = @max(a, b);

    const t0 = if (@as(i32, @intCast(maxAB)) > threshold) c else minAB;
    const t1 = if (threshold > @as(i32, @intCast(minAB))) t0 else maxAB;

    return t1;
}

// Returns a simplified filter for the first row
fn simplifyFilterOfFirstRow(filter: u8) u8 {
    const replacement = (@as(u32, @intFromEnum(FilterType.none)) << 0) | // None  -> None
        (@as(u32, @intFromEnum(FilterType.sub)) << 4) | // Sub   -> Sub
        (@as(u32, @intFromEnum(FilterType.none)) << 8) | // Up    -> None
        (@as(u32, @intFromEnum(FilterType.avg0)) << 12) | // Avg   -> Avg0
        (@as(u32, @intFromEnum(FilterType.sub)) << 16); // Paeth -> Sub
    return @intCast((replacement >> (@as(u5, @intCast(filter)) * 4)) & 0xF);
}
