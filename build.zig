const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const util_mod = b.createModule(.{
        .source_file = Build.FileSource.relative("src/util.zig"),
    });
    const eval_mod = b.addModule("comath", .{
        .source_file = Build.FileSource.relative("src/main.zig"),
        .dependencies = &.{
            .{ .name = "util", .module = util_mod },
        },
    });
    _ = eval_mod;

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const main_tests = b.addTest(.{
        .root_source_file = Build.FileSource.relative("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_main_tests = b.addRunArtifact(main_tests);
    main_tests.addModule("util", util_mod);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_main_tests.step);
}
