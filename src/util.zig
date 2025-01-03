const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const expectError = std.testing.expectError;

/// Returns true if the first `n` bits of `value` match `match`.
pub fn matchFirstBits(comptime T: type, value: u8, match: T) bool {
    const bitSize = @bitSizeOf(T);
    if (bitSize > 8) {
        @compileError("cannot match more than 8 bits");
    }
    const ones: u8 = (1 << 8) - 1;
    const mask = ones << (8 - bitSize);
    const compare = @as(u8, match) << (8 - bitSize);
    return (value & mask) == compare;
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

test "match first bits" {
    try expect(matchFirstBits(u8, 0b1100_0111, 0b1100_0111));
    try expect(!matchFirstBits(u8, 0b1100_0111, 0b1101_0111));
    try expect(matchFirstBits(u3, 0b1100_0111, 0b110));
    try expect(!matchFirstBits(u3, 0b1100_0111, 0b111));
}

test "read bytes" {
    const data = "abcd";
    var reader = std.io.fixedBufferStream(data);
    const first_byte = try readByte(&reader);
    try expectEqual('a', first_byte);
    const next_3_bytes = try readBytes(3, &reader);
    try expectEqualStrings("bcd", &next_3_bytes);
    try expectError(error.UnexpectedEndOfData, readByte(&reader));
}
