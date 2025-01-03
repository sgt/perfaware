const std = @import("std");

/// Returns true if the first `n` bits of `value` match `match`.
pub fn matchFirstBits(comptime T: type, value: u8, match: T) bool {
    const bitSize = @bitSizeOf(T);
    if (bitSize > 8) {
        @compileError("cannot match more than 8 bits");
    }
    const truncated: T = @truncate(value >> (8 - bitSize));
    return truncated == match;
}

pub fn readBytes(comptime n: usize, reader: anytype) ![n]u8 {
    var buf: [n]u8 = undefined;
    const amt_read = try reader.read(&buf);
    if (amt_read < n) {
        return error.UnexpectedEndOfData;
    }
    return buf;
}

pub fn readByte(reader: anytype) !u8 {
    const bytes = try readBytes(1, reader);
    return bytes[0];
}

pub fn isBitSet(value: u8, n: u3) bool {
    return (value & (@as(u8, 1) << n)) != 0;
}

test "match first bits" {
    const value = 0b1100_0111;
    try std.testing.expect(matchFirstBits(u8, value, 0b1100_0111));
    try std.testing.expect(!matchFirstBits(u8, value, 0b1101_0111));
    try std.testing.expect(matchFirstBits(u3, value, 0b110));
    try std.testing.expect(!matchFirstBits(u3, value, 0b111));
}

test "read bytes" {
    const data = "abcd";
    var reader = std.io.fixedBufferStream(data);
    const first_byte = try readByte(&reader);
    try std.testing.expectEqual('a', first_byte);
    const next_3_bytes = try readBytes(3, &reader);
    try std.testing.expectEqualStrings("bcd", &next_3_bytes);
    try std.testing.expectError(error.UnexpectedEndOfData, readByte(&reader));
}

test "is bit set" {
    const value = 0b1100_0101;
    try std.testing.expect(isBitSet(value, 0));
    try std.testing.expect(!isBitSet(value, 1));
    try std.testing.expect(isBitSet(value, 2));
}
