const std = @import("std");
const expectEqualDeep = std.testing.expectEqualDeep;
const expectEqualStrings = std.testing.expectEqualStrings;
const BitReader = std.io.BitReader;

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

fn decodeRegister(w: bool, enc_3_bits: u8) Register {
    if (!w) {
        return switch (enc_3_bits) {
            0b000 => .al,
            0b001 => .cl,
            0b010 => .dl,
            0b011 => .bl,
            0b100 => .ah,
            0b101 => .ch,
            0b110 => .dh,
            0b111 => .bh,
            else => unreachable,
        };
    } else {
        return switch (enc_3_bits) {
            0b000 => .ax,
            0b001 => .cx,
            0b010 => .dx,
            0b011 => .bx,
            0b100 => .sp,
            0b101 => .bp,
            0b110 => .si,
            0b111 => .di,
            else => unreachable,
        };
    }
}

fn decode(file: std.fs.File, allocator: std.mem.Allocator) ![]Instr {
    var result = std.ArrayList(Instr).init(allocator);
    defer result.deinit();

    var reader = file.reader();

    while (true) {
        const first_byte = reader.readByte() catch |err| {
            if (err == error.EndOfStream) {
                break;
            } else {
                return err;
            }
        };
        const op = try determineOp(first_byte);
        switch (op) {
            .mov => {
                const d = first_byte & 0b10 == 0b10;
                const w = first_byte & 0b1 == 0b1;

                const second_byte = try reader.readByte();

                const mod = (second_byte & 0b1100_0000) >> 6;
                if (mod != 0b11) {
                    return error.UnsupportedOpVariant;
                }

                const reg: u8 = (second_byte & 0b0011_1000) >> 3;
                const rm: u8 = second_byte & 0b0000_0111;

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

    const instructions = try decode(file, allocator);
    defer allocator.free(instructions);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("bits 16\n\n", .{});

    for (instructions) |instr| {
        try stdout.print("{}\n", .{instr});
    }
}

test "format" {
    const allocator = std.testing.allocator;
    const instr = Instr{ .op = .mov, .op_args = OpArgs{ .two = .{ .arg1 = .{ .register = .ax }, .arg2 = .{ .register = .bx } } } };
    const str = try std.fmt.allocPrint(allocator, "{}", .{instr});
    defer allocator.free(str);
    try expectEqualStrings(str, "mov ax, bx");
}
