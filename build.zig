const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run all tests");
    const unit_test_step = b.step("unit-test", "Run unit tests");
    const integration_test_step = b.step("integration-test", "Run integration tests");
    test_step.dependOn(unit_test_step);
    test_step.dependOn(integration_test_step);

    const comath_mod = b.addModule("comath", .{ .source_file = Build.FileSource.relative("src/main.zig") });
    const util_mod = b.createModule(.{ .source_file = Build.FileSource.relative("src/util.zig") });
    comath_mod.dependencies.putNoClobber("util", util_mod) catch |err| @panic(@errorName(err));

    const unit_tests = b.addTest(.{
        .root_source_file = Build.FileSource.relative("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    unit_tests.addModule("util", util_mod);
    const run_unit_tests = b.addRunArtifact(unit_tests);
    unit_test_step.dependOn(&run_unit_tests.step);

    const integration_tests = b.addTest(.{
        .root_source_file = Build.FileSource.relative("tests/integration.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_integration_tests = b.addRunArtifact(integration_tests);
    integration_tests.addModule("comath", comath_mod);
    integration_test_step.dependOn(&run_integration_tests.step);
}
