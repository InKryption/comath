const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    // some options are for local development only;
    // they're only enabled if this is the root `build.zig`.
    const is_root = b.pkg_hash.len == 0;

    const maybe_target = if (is_root) b.standardTargetOptions(.{}) else null;
    const maybe_optimize = if (is_root) b.standardOptimizeOption(.{}) else null;

    const install_step = b.getInstallStep();
    const unit_test_step = b.step("unit-test", "Run unit tests.");

    {
        const test_step = b.step("test", "Run all tests.");
        test_step.dependOn(unit_test_step);
    }

    {
        const check_step = b.step("check", "Check step.");
        check_step.dependOn(install_step);
    }

    const comath_mod = b.addModule("comath", .{
        .root_source_file = b.path("src/comath.zig"),
        // only applicable for local development, ie as the root of the unit test step.
        // as a dependency, these should simply be inherited by whatever imports comath.
        .target = maybe_target,
        .optimize = maybe_optimize,
    });
    const util_mod = b.createModule(.{
        .root_source_file = b.path("src/util.zig"),
    });
    comath_mod.addImport("util", util_mod);

    if (!is_root) return;
    const bin_install = !(b.option(bool, "no-bin", "Don't install any of the artifact implied by the specified steps.") orelse false);
    const bin_run = !(b.option(bool, "no-run", "Don't run any of the executables implied by the specified steps.") orelse false);

    const unit_test_exe = b.addTest(.{
        .name = "unit-test",
        .root_module = comath_mod,
    });
    unit_test_step.dependOn(&unit_test_exe.step);

    if (bin_install) {
        const unit_test_install = b.addInstallArtifact(unit_test_exe, .{});
        install_step.dependOn(&unit_test_install.step);
    }

    if (bin_run) {
        const unit_test_run = b.addRunArtifact(unit_test_exe);
        unit_test_step.dependOn(&unit_test_run.step);
    }
}
