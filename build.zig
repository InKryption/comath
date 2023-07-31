const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const util_mod = b.createModule(.{
        .source_file = Build.FileSource.relative("src/util.zig"),
    });
    const eval = b.addModule("eval", .{
        .source_file = Build.FileSource.relative("src/main.zig"),
        .dependencies = &.{
            .{ .name = "util", .module = util_mod },
        },
    });
    const contexts = b.addModule("contexts", .{
        .source_file = Build.FileSource.relative("src/contexts.zig"),
        .dependencies = &.{
            .{ .name = "util", .module = util_mod },
            .{ .name = "eval", .module = eval },
        },
    });
    _ = contexts;

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const main_tests = b.addTest(.{
        .root_source_file = Build.FileSource.relative("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_main_tests = b.addRunArtifact(main_tests);
    main_tests.addModule("util", util_mod);

    const contexts_tests = b.addTest(.{
        .root_source_file = Build.FileSource.relative("src/contexts.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_contexts_tests = b.addRunArtifact(contexts_tests);
    contexts_tests.addModule("eval", eval);
    contexts_tests.addModule("util", util_mod);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_main_tests.step);
    test_step.dependOn(&run_contexts_tests.step);
}
