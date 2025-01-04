const std = @import("std");
const util = @import("util.zig");

const Op = enum {
    mov,
};

const Register = enum {
    al,
    bl,
    cl,
    dl,
    ah,
    bh,
    ch,
    dh,
    ax,
    bx,
    cx,
    dx,
    sp,
    bp,
    si,
    di,
};

const OpArg = union(enum) {
    reg: Register,
    addrOneReg: Register,
    addrOneRegD8: struct { reg: Register, d8: u8 },
    addrOneRegD16: struct { reg: Register, d16: u16 },
    addrTwoRegs: struct { reg1: Register, reg2: Register },
    addrTwoRegsD8: struct { reg1: Register, reg2: Register, d8: u8 },
    addrTwoRegsD16: struct { reg1: Register, reg2: Register, d16: u16 },
    // TODO: direct address variant

    pub fn format(self: OpArg, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .reg => |reg| {
                try writer.writeAll(@tagName(reg));
            },
            else => unreachable,
        }
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
                return writer.print("{}, {}", .{ two.arg1, two.arg2 });
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
    const d = util.isBitSet(first_byte, 1);
    const w = util.isBitSet(first_byte, 0);

    const second_byte = try reader.readByte();

    const mod = (second_byte & 0b1100_0000) >> 6;
    if (mod != 0b11) {
        return error.UnsupportedOpVariant;
    }

    const reg: u3 = @truncate(second_byte >> 3);
    const rm: u3 = @truncate(second_byte);

    const param1 = OpArg{ .reg = decodeRegister(w, reg) };
    const param2 = OpArg{ .reg = decodeRegister(w, rm) };

    const op_args = if (d) OpArgs{ .two = .{ .arg1 = param1, .arg2 = param2 } } else OpArgs{ .two = .{ .arg1 = param2, .arg2 = param1 } };
    return .{ .op = .mov, .op_args = op_args };
}

/// MOV Immediate to register/memory.
fn opMovImmediateToRegMem(_: u8, _: anytype) !Instr {
    return error.NotImplemented;
}

/// MOV Immediate to register.
fn opMovImmediateToReg(_: u8, _: anytype) !Instr {
    return error.NotImplemented;
}

/// MOV Memory to accumulator.
fn opMovMemToAcc(_: u8, _: anytype) !Instr {
    return error.NotImplemented;
}

/// MOV Accumulator to memory.
fn opMovAccToMem(_: u8, _: anytype) !Instr {
    return error.NotImplemented;
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
    if (util.startsWithBits(first_byte, u6, 0b1000_10)) {
        return opMovRegFromRegMem(first_byte, reader);
    } else if (util.startsWithBits(first_byte, u7, 0b1100_011)) {
        return opMovImmediateToRegMem(first_byte, reader);
    } else if (util.startsWithBits(first_byte, u4, 0b1011)) {
        return opMovImmediateToReg(first_byte, reader);
    } else if (util.startsWithBits(first_byte, u7, 0b1010_000)) {
        return opMovMemToAcc(first_byte, reader);
    } else if (util.startsWithBits(first_byte, u7, 0b1010_001)) {
        return opMovAccToMem(first_byte, reader);
    } else if (first_byte == 0b1000_1110) {
        return opMovRegMemToSegReg(reader);
    } else if (first_byte == 0b1000_1100) {
        return opMovSegRegToRegMem(reader);
    } else {
        return error.UnknownOp;
    }
}

fn decodeRegister(w: bool, encoding: u3) Register {
    return switch (encoding) {
        0b000 => if (w) .ax else .al,
        0b001 => if (w) .cx else .cl,
        0b010 => if (w) .dx else .dl,
        0b011 => if (w) .bx else .bl,
        0b100 => if (w) .sp else .ah,
        0b101 => if (w) .bp else .ch,
        0b110 => if (w) .si else .dh,
        0b111 => if (w) .di else .bh,
    };
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
    const allocator = std.heap.page_allocator;
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
        try stdout.print("{}\n", .{instr});
    }
}

test "format instruction" {
    const allocator = std.testing.allocator;
    const instr = Instr{ .op = .mov, .op_args = OpArgs{ .two = .{ .arg1 = .{ .reg = .ax }, .arg2 = .{ .reg = .bx } } } };
    const str = try std.fmt.allocPrint(allocator, "{}", .{instr});
    defer allocator.free(str);
    try std.testing.expectEqualStrings("mov ax, bx", str);
}

test "decode mov" {
    const data = "\x89\xd9\x88\xe5";
    const allocator = std.testing.allocator;
    var stream = std.io.fixedBufferStream(data);
    const reader = stream.reader();
    const instrs = try decode(reader, allocator);
    defer allocator.free(instrs);
    try std.testing.expectEqualDeep(&[_]Instr{
        Instr{ .op = .mov, .op_args = OpArgs{ .two = .{ .arg1 = .{ .reg = .cx }, .arg2 = .{ .reg = .bx } } } },
        Instr{ .op = .mov, .op_args = OpArgs{ .two = .{ .arg1 = .{ .reg = .ch }, .arg2 = .{ .reg = .ah } } } },
    }, instrs);
}

test {
    _ = util;
}
