const std = @import("std");
const util = @import("util.zig");

const Op = enum {
    mov,
};

// zig fmt: off
const Register = enum { al, bl, cl, dl, ah, bh, ch, dh, ax, bx, cx, dx, sp, bp, si, di, };
// zig fmt: on

const OpArg = union(enum) {
    reg: Register,
    addr_one_reg: struct { reg: Register, d: i16 = 0 },
    addr_two_regs: struct { reg1: Register, reg2: Register, d: i16 = 0 },
    direct_addr: u16,
    immediate8: i8,
    immediate16: i16,

    pub fn format(self: OpArg, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .reg => |reg| {
                try writer.writeAll(@tagName(reg));
            },
            .addr_one_reg => |arg| {
                try writer.print("[{s}", .{@tagName(arg.reg)});
                if (arg.d > 0) {
                    try writer.print(" + {d}", .{arg.d});
                } else if (arg.d < 0) {
                    try writer.print(" - {d}", .{-arg.d});
                }
                try writer.writeAll("]");
            },
            .addr_two_regs => |arg| {
                try writer.print("[{s} + {s}", .{ @tagName(arg.reg1), @tagName(arg.reg2) });
                if (arg.d > 0) {
                    try writer.print(" + {d}", .{arg.d});
                } else if (arg.d < 0) {
                    try writer.print(" - {d}", .{-arg.d});
                }
                try writer.writeAll("]");
            },
            .direct_addr => |addr| {
                try writer.print("[{d}]", .{addr});
            },
            .immediate8 => |imm| {
                try writer.print("{d}", .{imm});
            },
            .immediate16 => |imm| {
                try writer.print("{d}", .{imm});
            },
        }
    }

    /// Decode register's 3-bit encoding.
    fn decodeRegister(w: bool, encoding: u3) OpArg {
        const reg: Register = switch (encoding) {
            0b000 => if (w) .ax else .al,
            0b001 => if (w) .cx else .cl,
            0b010 => if (w) .dx else .dl,
            0b011 => if (w) .bx else .bl,
            0b100 => if (w) .sp else .ah,
            0b101 => if (w) .bp else .ch,
            0b110 => if (w) .si else .dh,
            0b111 => if (w) .di else .bh,
        };
        return .{ .reg = reg };
    }

    /// Decode effective address when MOD is 0b00, 0b01 or 0b10.
    fn decodeAddr(encoding: u3, d: i16) OpArg {
        return switch (encoding) {
            0b000 => .{ .addr_two_regs = .{ .reg1 = .bx, .reg2 = .si, .d = d } },
            0b001 => .{ .addr_two_regs = .{ .reg1 = .bx, .reg2 = .di, .d = d } },
            0b010 => .{ .addr_two_regs = .{ .reg1 = .bp, .reg2 = .si, .d = d } },
            0b011 => .{ .addr_two_regs = .{ .reg1 = .bp, .reg2 = .di, .d = d } },
            0b100 => .{ .addr_one_reg = .{ .reg = .si, .d = d } },
            0b101 => .{ .addr_one_reg = .{ .reg = .di, .d = d } },
            0b110 => .{ .addr_one_reg = .{ .reg = .bp, .d = d } },
            0b111 => .{ .addr_one_reg = .{ .reg = .bx, .d = d } },
        };
    }

    fn decodeRegMem(mod: u2, w: bool, encoding: u3, reader: anytype) !OpArg {
        return switch (mod) {
            0b11 => decodeRegister(w, encoding),
            0b00 => if (encoding == 0b110) .{ .direct_addr = try reader.readInt(u16, .little) } else decodeAddr(encoding, 0),
            0b01 => decodeAddr(encoding, try reader.readInt(i8, .little)),
            0b10 => decodeAddr(encoding, try reader.readInt(i16, .little)),
        };
    }
};

const OpArgs = union(enum) {
    none: void,
    one: OpArg,
    two: struct { arg1: OpArg, arg2: OpArg },

    pub fn format(self: OpArgs, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .none => {},
            .one => |one| {
                return writer.print("{}", .{one});
            },
            .two => |two| {
                const modifier = switch (two.arg1) {
                    .addr_one_reg, .addr_two_regs => switch (two.arg2) {
                        .immediate8 => " byte",
                        .immediate16 => " word",
                        else => "",
                    },
                    else => "",
                };
                return writer.print("{},{s} {}", .{ two.arg1, modifier, two.arg2 });
            },
        }
    }
};

const Instr = struct {
    op: Op,
    op_args: OpArgs,

    pub fn format(self: Instr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return writer.print("{s} {}", .{ @tagName(self.op), self.op_args });
    }
};

/// MOV register/memory to/from register.
fn opMovRegFromRegMem(first_byte: u8, reader: anytype) !Instr {
    const d = util.isBitSet(1, first_byte);
    const w = util.isBitSet(0, first_byte);

    const second_byte = try reader.readByte();
    const mod: u2 = @truncate((second_byte & 0b1100_0000) >> 6);
    const reg: u3 = @truncate(second_byte >> 3);
    const regmem: u3 = @truncate(second_byte);

    const arg1 = OpArg.decodeRegister(w, reg);
    const arg2 = try OpArg.decodeRegMem(mod, w, regmem, reader);

    const op_args = if (d) .{ .two = .{ .arg1 = arg1, .arg2 = arg2 } } else .{ .two = .{ .arg1 = arg2, .arg2 = arg1 } };
    return .{ .op = .mov, .op_args = op_args };
}

/// MOV Immediate to register/memory.
fn opMovImmediateToRegMem(first_byte: u8, reader: anytype) !Instr {
    const w = util.isBitSet(0, first_byte);

    const second_byte = try reader.readByte();
    const mod: u2 = @truncate((second_byte & 0b1100_0000) >> 6);
    const zeroes: u3 = @truncate(second_byte >> 3);
    if (zeroes != 0) return error.UnknownOp;
    const regmem: u3 = @truncate(second_byte);

    const arg1 = try OpArg.decodeRegMem(mod, w, regmem, reader);
    const arg2 = if (w) OpArg{ .immediate16 = try reader.readInt(i16, .little) } else OpArg{ .immediate8 = try reader.readInt(i8, .little) };
    return .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = arg1, .arg2 = arg2 } } };
}

/// MOV Immediate to register.
fn opMovImmediateToReg(first_byte: u8, reader: anytype) !Instr {
    const w = util.isBitSet(3, first_byte);
    const reg: u3 = @truncate(first_byte);

    const arg1 = OpArg.decodeRegister(w, reg);
    const arg2 = if (w) OpArg{ .immediate16 = try reader.readInt(i16, .little) } else OpArg{ .immediate8 = try reader.readInt(i8, .little) };
    return .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = arg1, .arg2 = arg2 } } };
}

/// MOV Memory to accumulator.
fn opMovMemToAcc(first_byte: u8, reader: anytype) !Instr {
    const w = util.isBitSet(0, first_byte);
    const addr = if (w) try reader.readInt(u16, .little) else try reader.readInt(u8, .little);
    return .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .ax }, .arg2 = .{ .direct_addr = addr } } } };
}

/// MOV Accumulator to memory.
fn opMovAccToMem(first_byte: u8, reader: anytype) !Instr {
    const w = util.isBitSet(0, first_byte);
    const addr = if (w) try reader.readInt(u16, .little) else try reader.readInt(u8, .little);
    return .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .direct_addr = addr }, .arg2 = .{ .reg = .ax } } } };
}

/// MOV register/memory to/from segment register.
fn opMovRegMemToSegReg(_: anytype) !Instr {
    return error.NotImplemented;
}

/// MOV segment register to/from register/memory.
fn opMovSegRegToRegMem(_: anytype) !Instr {
    return error.NotImplemented;
}

fn decodeNextInstr(reader: anytype) !Instr {
    const first_byte = try reader.readByte();
    if (util.startsWithBits(u6, 0b1000_10, first_byte)) {
        return opMovRegFromRegMem(first_byte, reader);
    }
    if (util.startsWithBits(u7, 0b1100_011, first_byte)) {
        return opMovImmediateToRegMem(first_byte, reader);
    }
    if (util.startsWithBits(u4, 0b1011, first_byte)) {
        return opMovImmediateToReg(first_byte, reader);
    }
    if (util.startsWithBits(u7, 0b1010_000, first_byte)) {
        return opMovMemToAcc(first_byte, reader);
    }
    if (util.startsWithBits(u7, 0b1010_001, first_byte)) {
        return opMovAccToMem(first_byte, reader);
    }
    if (first_byte == 0b1000_1110) {
        return opMovRegMemToSegReg(reader);
    }
    if (first_byte == 0b1000_1100) {
        return opMovSegRegToRegMem(reader);
    }
    return error.UnknownOp;
}

fn decode(reader: anytype, allocator: std.mem.Allocator) ![]Instr {
    var result = std.ArrayList(Instr).init(allocator);
    defer result.deinit();

    while (true) {
        const instr = decodeNextInstr(reader) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        try result.append(instr);
    }

    return result.toOwnedSlice();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = args.skip();

    const file_path = args.next();
    const file = if (file_path) |value| try std.fs.cwd().openFile(value, .{ .mode = .read_only }) else std.io.getStdIn();
    defer file.close();

    const instructions = try decode(file.reader(), allocator);
    defer allocator.free(instructions);

    const stdout = std.io.getStdOut().writer();
    if (file_path) |value| {
        try stdout.print("; {s} disassembly:\n", .{value});
    }
    try stdout.print("bits 16\n\n", .{});

    for (instructions) |instr| {
        try stdout.print("{s}\n", .{instr});
    }
}

// ************ TESTS ************

test "format instruction" {
    const allocator = std.testing.allocator;
    const instr = Instr{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .ax }, .arg2 = .{ .reg = .bx } } } };
    const str = try std.fmt.allocPrint(allocator, "{}", .{instr});
    defer allocator.free(str);
    try std.testing.expectEqualStrings("mov ax, bx", str);
}

fn testDecoder(comptime data: []const u8, comptime expected: []const Instr) !void {
    const allocator = std.testing.allocator;
    var stream = std.io.fixedBufferStream(data);
    const reader = stream.reader();
    const instrs = try decode(reader, allocator);
    defer allocator.free(instrs);
    try std.testing.expectEqualDeep(expected, instrs);
}

test "decode mov reg to reg" {
    const data = "\x89\xd9\x88\xe5";
    const expected = .{
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .cx }, .arg2 = .{ .reg = .bx } } } },
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .ch }, .arg2 = .{ .reg = .ah } } } },
    };
    try testDecoder(data, &expected);
}

test "decode mov immediate to reg" {
    const data = "\xb1\x0c\xb5\xf4\xb9\x0c\x00\xb9\xf4\xff";
    const expected = .{
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .cl }, .arg2 = .{ .immediate8 = 12 } } } },
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .ch }, .arg2 = .{ .immediate8 = -12 } } } },
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .cx }, .arg2 = .{ .immediate16 = 12 } } } },
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .cx }, .arg2 = .{ .immediate16 = -12 } } } },
    };
    try testDecoder(data, &expected);
}

test "decode mov negative displacement" {
    const data = "\x8b\x41\xdb";
    const expected = .{
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .ax }, .arg2 = .{ .addr_two_regs = .{ .reg1 = .bx, .reg2 = .di, .d = -37 } } } } },
    };
    try testDecoder(data, &expected);
}

test "decode mov immediate to memory" {
    const data = "\xc6\x03\x07\xc7\x85\x85\x03\x5b\x01";
    const expected = .{
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .addr_two_regs = .{ .reg1 = .bp, .reg2 = .di } }, .arg2 = .{ .immediate8 = 7 } } } },
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .addr_one_reg = .{ .reg = .di, .d = 901 } }, .arg2 = .{ .immediate16 = 347 } } } },
    };
    try testDecoder(data, &expected);
}

test "decode mov memory to register" {
    const data = "\x8b\x2e\x05\x00" ++ "\x8b\x1e\x82\x0d";
    const expected = .{
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .bp }, .arg2 = .{ .direct_addr = 0x5 } } } },
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .bx }, .arg2 = .{ .direct_addr = 0xd82 } } } },
    };
    try testDecoder(data, &expected);
}

test "decode mov memory to acc" {
    const data = "\xa1\xfb\x09" ++ "\xa1\x10\x00";
    const expected = .{
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .ax }, .arg2 = .{ .direct_addr = 0x9fb } } } },
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .reg = .ax }, .arg2 = .{ .direct_addr = 0x10 } } } },
    };
    try testDecoder(data, &expected);
}

test "decode mov acc to memory" {
    const data = "\xa3\xfa\x09" ++ "\xa3\x0f\x00";
    const expected = .{
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .direct_addr = 0x9fa }, .arg2 = .{ .reg = .ax } } } },
        .{ .op = .mov, .op_args = .{ .two = .{ .arg1 = .{ .direct_addr = 0xf }, .arg2 = .{ .reg = .ax } } } },
    };
    try testDecoder(data, &expected);
}

test {
    std.testing.refAllDecls(@This());
}
