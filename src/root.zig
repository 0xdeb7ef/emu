const std = @import("std");

pub const chip8 = @import("chip8.zig");

test {
    std.testing.refAllDecls(@This());
}
