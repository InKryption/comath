const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    // some options are for local development only;
    // they're only enabled if this is the root `build.zig`.
    const is_root = b.pkg_hash.len == 0;

    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const run_steps = if (is_root) !(b.option(bool, "no-run", "Don't run any of the executables implied by the specified steps.") orelse false) else false;

    const install_step = b.getInstallStep();
    const unit_test_step = b.step("unit-test", "Run unit tests.");

    {
        const test_step = b.step("test", "Run all tests.");
        test_step.dependOn(unit_test_step);
    }

    const comath_mod = b.addModule("comath", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const util_mod = b.createModule(.{
        .root_source_file = b.path("src/util.zig"),
    });
    comath_mod.addImport("util", util_mod);

    const unit_test_exe = b.addTest(.{
        .name = "unit-test",
        .root_module = comath_mod,
    });

    const unit_test_install = b.addInstallArtifact(unit_test_exe, .{});
    install_step.dependOn(&unit_test_install.step);
    install_step.dependOn(&unit_test_install.step);

    const unit_test_run = b.addRunArtifact(unit_test_exe);
    if (run_steps) unit_test_step.dependOn(&unit_test_run.step);
}
