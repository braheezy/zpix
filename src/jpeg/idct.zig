pub const block_size = 64;

pub const Block = [block_size]i32;

pub fn emptyBlock() Block {
    // This is a block of all zeros
    return [_]i32{0} ** block_size;
}
