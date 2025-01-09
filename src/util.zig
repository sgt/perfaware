const std = @import("std");

/// Returns true if the first `n` bits of `value` match `prefix`.
pub fn startsWithBits(comptime T: type, comptime prefix: T, value: u8) bool {
    const bitSize = @bitSizeOf(T);
    if (bitSize > 8) {
        @compileError("cannot match more than 8 bits");
    }
    const truncated: T = @truncate(value >> (8 - bitSize));
    return truncated == prefix;
}

pub fn isBitSet(comptime n: u3, value: u8) bool {
    return (value & (@as(u8, 1) << n)) != 0;
}

test "match first bits" {
    const value = 0b1100_0111;
    try std.testing.expect(startsWithBits(u8, 0b1100_0111, value));
    try std.testing.expect(!startsWithBits(u8, 0b1101_0111, value));
    try std.testing.expect(startsWithBits(u3, 0b110, value));
    try std.testing.expect(!startsWithBits(u3, 0b111, value));
}

test "is bit set" {
    const value = 0b1100_0101;
    try std.testing.expect(isBitSet(0, value));
    try std.testing.expect(!isBitSet(1, value));
    try std.testing.expect(isBitSet(2, value));
}
