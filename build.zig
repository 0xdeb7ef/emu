const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("emu", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // CHIP-8 test ROMs
    const chip8_test_suite = b.dependency("chip8_test_suite", .{});
    const chip8_tests_dir = chip8_test_suite.path("bin");
    var chip8_tests = try std.fs.openDirAbsolute(chip8_tests_dir.getPath(b), .{ .iterate = true });
    defer chip8_tests.close();

    var iter = chip8_tests.iterate();
    while (try iter.next()) |file| {
        if (std.mem.endsWith(u8, file.name, ".ch8")) {
            lib_unit_tests.root_module.addAnonymousImport(
                file.name,
                .{
                    .root_source_file = chip8_tests_dir.path(b, file.name),
                },
            );
        }
    }

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
