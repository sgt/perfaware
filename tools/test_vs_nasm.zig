const std = @import("std");

const data_dir_path = "data";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const ally = arena.allocator();

    const output_dir_path = try std.fs.path.join(ally, &.{ data_dir_path, "out" });

    // make output dir if doesn't exist
    std.fs.cwd().makeDir(output_dir_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var data_dir = try std.fs.cwd().openDir(data_dir_path, .{ .iterate = true });
    defer data_dir.close();

    const binary_filenames = blk: {
        var list = std.ArrayList([]const u8).init(ally);

        var iterator = data_dir.iterate();
        while (try iterator.next()) |entry| {
            if (entry.kind == .file and !std.mem.endsWith(u8, entry.name, ".asm")) {
                try list.append(try ally.dupe(u8, entry.name));
            }
        }

        break :blk try list.toOwnedSlice();
    };

    for (binary_filenames) |binary| {
        std.debug.print("{s}\n", .{binary});
    }

    // const result = try std.process.Child.run(.{
    //     .allocator = allocator,
    //     .argv = &.{"ls"},
    // });
    // std.debug.print("{s}\n", .{result.stdout});
}
