# zpix Agent Guide

## Project Overview

zpix is a pure Zig image decoding library that ports Go's standard library color/image functionality. The project focuses on providing robust image decoding capabilities with support for multiple formats:

- JPEG (Baseline and Progressive)
  - Gray, YCbCr, RGBA, and YCMK color formats
- PNG
  - Gray (1, 2, 4, 8, and 16 bit)
  - Gray + alpha (8 and 16 bit)
  - Truecolor (8 and 16 bit)
  - Truecolor + alpha (8 and 16 bit)
  - Paletted (1, 2, 4, and 8 bit)
  - Interlaced support
- QOI (in development)

## Zig Version

The project requires Zig version 0.14.0 or higher. All code must be compatible with this version.

Here is commonly used syntax that you should check to make sure is correct:

- `@intCast(int: anytype) anytype`: Converts an integer to another integer while keeping the same numerical value. The return type is the inferred result type.
- `@as(comptime T: type, expression) T`: Performs Type Coercion.

## Project Structure

The codebase is organized into several key modules:

```
src/
  ├── color/       # Color models and conversions
  ├── image/       # Core image types and operations
  ├── jpeg/        # JPEG decoder implementation
  ├── png/         # PNG decoder implementation
  ├── qoi/         # QOI encoding and decoding support
  └── root.zig     # Main module exports
```

## Key APIs and Patterns

### Color Models

The color module (`src/color/color.zig`) provides various color types and conversions:

- Basic types: Gray, Gray16, RGB, RGBA, NRGBA, etc.
- All color types implement conversion to alpha-premultiplied RGBA
- Color models support conversion between different formats

### Image Types

The image module (`src/image/image.zig`) defines the core image types:

- `Image` union type supporting multiple formats
- Each format has its own struct (RGBAImage, GrayImage, etc.)
- Common operations: bounds checking, pixel access, subimage creation

### Memory Management

- Use arena allocators for temporary allocations during decoding
- Always free allocated memory using the same allocator
- Image data ownership transfers to the caller after decoding

### Error Handling

- Use explicit error unions for failure cases
- Provide descriptive error messages through std.log
- Handle dimension and buffer overflow checks carefully

## Coding Style

### General Guidelines

1. Use descriptive variable names that reflect Go equivalents where applicable
2. Follow Zig's error handling patterns with explicit error unions
3. Implement thorough bounds checking for all pixel operations
4. Use comptime features where appropriate for performance
5. Document public APIs with clear usage examples

### Memory Management

1. Always use explicit allocator arguments
2. Clean up resources in reverse order of allocation
3. Use arena allocators for temporary decode operations
4. Transfer ownership of final image data to caller

### Error Handling

1. Define specific error sets for each module
2. Use descriptive error names (e.g., `error.InvalidDimension`)
3. Log errors with context using `std.log`
4. Handle all error cases explicitly

## Testing

- Test files are alongside implementation files
- Use descriptive test names that explain the test case
- Test edge cases and error conditions
- Include visual test cases with known good outputs

## Documentation

- Document public APIs with usage examples
- Include parameter descriptions and error conditions
- Reference Go standard library equivalents where applicable
- Keep implementation notes for complex algorithms

## Common Tasks

### Adding a New Image Format

1. Create a new module directory under `src/`
2. Implement the decoder following existing patterns
3. Add format-specific error types
4. Integrate with the main Image type
5. Add tests with sample images

### Modifying Color Support

1. Add new color type to `color.zig`
2. Implement conversion to RGBA
3. Update relevant image types
4. Add tests for color conversion
5. Update documentation

### Performance Optimization

1. Profile the specific operation
2. Consider comptime optimizations
3. Optimize memory allocation patterns
4. Maintain readability and safety
5. Document performance implications

## Dependencies

The project aims to minimize external dependencies. Current dependencies:

- Standard library only for core functionality
- SDL for example viewer (optional)
