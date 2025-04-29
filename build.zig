const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create the main zpix module
    const zpix_mod = b.addModule("zpix", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Create submodules that can be imported directly
    const color_mod = b.addModule("color", .{
        .root_source_file = b.path("src/color/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const image_mod = b.addModule("image", .{
        .root_source_file = b.path("src/image/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const jpeg_mod = b.addModule("jpeg", .{
        .root_source_file = b.path("src/jpeg/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const png_mod = b.addModule("png", .{
        .root_source_file = b.path("src/png/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add dependencies between modules
    image_mod.addImport("color", color_mod);
    jpeg_mod.addImport("image", image_mod);
    png_mod.addImport("image", image_mod);
    png_mod.addImport("color", color_mod);

    // Add submodules to main zpix module
    zpix_mod.addImport("color", color_mod);
    zpix_mod.addImport("image", image_mod);
    zpix_mod.addImport("jpeg", jpeg_mod);
    zpix_mod.addImport("png", png_mod);

    // Tests
    const jpeg_tests = b.addTest(.{ .root_module = jpeg_mod });
    const png_tests = b.addTest(.{ .root_module = png_mod });

    const run_jpeg_tests = b.addRunArtifact(jpeg_tests);
    const run_png_tests = b.addRunArtifact(png_tests);

    const test_step_jpeg = b.step("test-jpeg", "Run unit tests");
    test_step_jpeg.dependOn(&run_jpeg_tests.step);

    const test_step_png = b.step("test-png", "Run unit tests");
    test_step_png.dependOn(&run_png_tests.step);

    // Documentation
    const docs_lib = b.addStaticLibrary(.{
        .name = "zpix",
        .root_module = zpix_mod,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = docs_lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Copy documentation artifacts to prefix path");
    docs_step.dependOn(&install_docs.step);

    const serve_step = b.step("serve", "Serve documentation");
    var a3: [3][]const u8 = .{ "zig", "run", "serveDocs.zig" };
    const serve_run = b.addSystemCommand(&a3);
    serve_step.dependOn(&serve_run.step);

    // Example builds
    const exe = try buildExample(b, optimize, target);
    exe.root_module.addImport("zpix", zpix_mod);

    const sng_exe = b.addExecutable(.{
        .name = "sng",
        .root_module = b.createModule(.{
            .root_source_file = b.path("example/sng.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    sng_exe.root_module.addImport("zpix", zpix_mod);
    b.installArtifact(sng_exe);
}

fn buildExample(
    b: *std.Build,
    optimize: std.builtin.OptimizeMode,
    target: std.Build.ResolvedTarget,
) !*std.Build.Step.Compile {
    const exe = b.addExecutable(.{
        .name = "zpixview",
        .root_module = b.createModule(.{
            .root_source_file = b.path("example/zpixview.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Dependencies
    if (b.lazyDependency("SDL", .{
        .optimize = optimize,
        .target = target,
    })) |sdl_dep| {
        const sdl_artifact = sdl_dep.artifact("SDL2");
        for (sdl_artifact.root_module.include_dirs.items) |include_dir| {
            try exe.root_module.include_dirs.append(b.allocator, include_dir);
        }
        exe.linkLibrary(sdl_artifact);
    }

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the example");
    run_step.dependOn(&run_cmd.step);

    return exe;
}
