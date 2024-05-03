const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run all tests");
    const unit_test_step = b.step("unit-test", "Run unit tests");
    test_step.dependOn(unit_test_step);

    const comath_mod = b.addModule("comath", .{
        .root_source_file = b.path("src/main.zig"),
    });
    const util_mod = b.createModule(.{
        .root_source_file = b.path("src/util.zig"),
    });
    comath_mod.addImport("util", util_mod);

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    unit_tests.root_module.addImport("util", util_mod);
    const run_unit_tests = b.addRunArtifact(unit_tests);
    unit_test_step.dependOn(&run_unit_tests.step);
}
