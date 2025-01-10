const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimise = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "sim8086",
        .root_source_file = b.path("src/sim8086.zig"),
        .target = target,
        .optimize = optimise,
    });

    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/sim8086.zig"),
        .target = target,
        .optimize = optimise,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    // const exe_release = b.addExecutable(.{
    //     .name = "sim8086-release",
    //     .root_source_file = b.path("src/sim8086.zig"),
    //     .target = target,
    //     .optimize = .ReleaseFast,
    // });

    // b.installArtifact(exe_release);
    // const run_release_cmd = b.addRunArtifact(exe_release);
    // run_release_cmd.step.dependOn(b.getInstallStep());

    // if (b.args) |args| {
    //     run_release_cmd.addArgs(args);
    // }

    // const run_release_step = b.step("run-release", "Run the app (release build)");
    // run_release_step.dependOn(&run_release_cmd.step);

    const test_vs_nasm = b.addExecutable(.{
        .name = "test_vs_nasm",
        .root_source_file = b.path("tools/test_vs_nasm.zig"),
        .target = target,
        .optimize = optimise,
    });

    b.installArtifact(test_vs_nasm);
    const run_test_vs_nasm_cmd = b.addRunArtifact(test_vs_nasm);
    run_test_vs_nasm_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_test_vs_nasm_cmd.addArgs(args);
    }

    const run_test_vs_nasm_step = b.step("test-vs-nasm", "Run test_vs_nasm");
    run_test_vs_nasm_step.dependOn(&run_test_vs_nasm_cmd.step);

    const tools_unit_tests = b.addTest(.{
        .root_source_file = b.path("tools/test_vs_nasm.zig"),
        .target = target,
        .optimize = optimise,
    });

    const run_tools_unit_tests = b.addRunArtifact(tools_unit_tests);
    test_step.dependOn(&run_tools_unit_tests.step);
}
