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
    register: Register,

    pub fn format(self: OpArg, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .register => |reg| {
                try writer.writeAll(@tagName(reg));
            },
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

fn determineOp(byte: u8) !Op {
    const first_6_bits = byte & 0b1111_1100;
    return switch (first_6_bits) {
        0b1000_1000 => .mov,
        else => error.UnknownOp,
    };
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
        // read next byte or bail on eof
        const first_byte = reader.readByte() catch break;

        const op = try determineOp(first_byte);
        switch (op) {
            .mov => {
                const d = util.isBitSet(first_byte, 1);
                const w = util.isBitSet(first_byte, 0);

                const second_byte = try reader.readByte();

                const mod = (second_byte & 0b1100_0000) >> 6;
                if (mod != 0b11) {
                    return error.UnsupportedOpVariant;
                }

                const reg: u3 = @truncate(second_byte >> 3);
                const rm: u3 = @truncate(second_byte);

                const param1 = OpArg{ .register = decodeRegister(w, reg) };
                const param2 = OpArg{ .register = decodeRegister(w, rm) };

                const op_args = if (d) OpArgs{ .two = .{ .arg1 = param1, .arg2 = param2 } } else OpArgs{ .two = .{ .arg1 = param2, .arg2 = param1 } };
                const instr = Instr{ .op = op, .op_args = op_args };
                try result.append(instr);
            },
        }
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
    const instr = Instr{ .op = .mov, .op_args = OpArgs{ .two = .{ .arg1 = .{ .register = .ax }, .arg2 = .{ .register = .bx } } } };
    const str = try std.fmt.allocPrint(allocator, "{}", .{instr});
    defer allocator.free(str);
    try std.testing.expectEqualStrings("mov ax, bx", str);
}

test "decode mov" {
    const data = "\x89\xd9\x88\xe5";
    const allocator = std.testing.allocator;
    var stream = std.io.fixedBufferStream(data);
    const reader = stream.reader();
    const instrs = try decode(&reader, allocator);
    defer allocator.free(instrs);
    try std.testing.expectEqualDeep(&[_]Instr{
        Instr{ .op = .mov, .op_args = OpArgs{ .two = .{ .arg1 = .{ .register = .cx }, .arg2 = .{ .register = .bx } } } },
        Instr{ .op = .mov, .op_args = OpArgs{ .two = .{ .arg1 = .{ .register = .ch }, .arg2 = .{ .register = .ah } } } },
    }, instrs);
}

test {
    _ = util;
}
