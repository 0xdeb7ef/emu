const std = @import("std");

pub const CPU = struct {
    memory: [4 * 1024]u8 = undefined,
    register: [16]u8 = [_]u8{0} ** 16,

    I: u16 = 0,
    PC: u16 = PC_START,
    SP: u8 = 0,
    stack: [STACK_SIZE]u16 = [_]u16{0} ** STACK_SIZE,

    delay: u8 = 0,
    sound: u8 = 0,

    keyboard: [16]bool = [_]bool{false} ** 16,
    display: [HEIGHT][WIDTH]u1 = undefined,

    // config
    old_shr: bool = false,

    pub const PC_START = 0x200;
    const STACK_SIZE = 16;

    const WIDTH = 64;
    const HEIGHT = 32;

    const FONT_START = 0x000;
    const FONT = [_]u8{
        0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
        0x20, 0x60, 0x20, 0x20, 0x70, // 1
        0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
        0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
        0x90, 0x90, 0xF0, 0x10, 0x10, // 4
        0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
        0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
        0xF0, 0x10, 0x20, 0x40, 0x40, // 7
        0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
        0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
        0xF0, 0x90, 0xF0, 0x90, 0x90, // A
        0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
        0xF0, 0x80, 0x80, 0x80, 0xF0, // C
        0xE0, 0x90, 0x90, 0x90, 0xE0, // D
        0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
        0xF0, 0x80, 0xF0, 0x80, 0x80, // F
    };

    pub fn init() CPU {
        var c = CPU{};
        c.load_mem(FONT_START, &FONT);
        return c;
    }

    pub fn load_mem(self: *CPU, address: u16, mem: []const u8) void {
        @memcpy(self.memory[address .. address + mem.len], mem);
    }

    fn stack_push(self: *CPU) void {
        if (self.SP >= self.stack.len)
            @panic("stack overflow");

        self.stack[self.SP] = self.PC;
        self.SP += 1;
    }

    fn stack_pop(self: *CPU) void {
        if (self.SP <= 0)
            @panic("stack underflow");

        self.PC = self.stack[self.SP];
        self.SP -= 1;
    }

    pub fn fetch(self: *CPU) u16 {
        const i: u16 = @as(u16, self.memory[self.PC]) << 8 | self.memory[self.PC + 1];
        self.PC += 2;
        return i;
    }

    pub fn decode(inst: u16) Instruction {
        return switch (inst & 0xF000) {
            0x0000 => switch (inst & 0x0FFF) {
                0x0E0 => Instruction{ .CLS = {} },
                0x0EE => Instruction{ .RET = {} },
                else => Instruction{ .SYS = {} },
            },
            0x1000 => Instruction{ .JP = @bitCast(inst) },
            0x2000 => Instruction{ .CALL = @bitCast(inst) },
            0x3000 => Instruction{ .SE_Vx = @bitCast(inst) },
            0x4000 => Instruction{ .SNE_Vx = @bitCast(inst) },
            0x5000 => switch (inst & 0x000F) {
                0x0 => Instruction{ .SE_Vx_Vy = @bitCast(inst) },
                else => Instruction{ .invalid = inst },
            },
            0x6000 => Instruction{ .LD_Vx = @bitCast(inst) },
            0x7000 => Instruction{ .ADD_Vx = @bitCast(inst) },
            0x8000 => switch (inst & 0x000F) {
                0x0 => Instruction{ .LD_Vx_Vy = @bitCast(inst) },
                0x1 => Instruction{ .OR = @bitCast(inst) },
                0x2 => Instruction{ .AND = @bitCast(inst) },
                0x3 => Instruction{ .XOR = @bitCast(inst) },
                0x4 => Instruction{ .ADD_Vx_Vy = @bitCast(inst) },
                0x5 => Instruction{ .SUB = @bitCast(inst) },
                0x6 => Instruction{ .SHR = @bitCast(inst) },
                0x7 => Instruction{ .SUBN = @bitCast(inst) },
                0xE => Instruction{ .SHL = @bitCast(inst) },
                else => Instruction{ .invalid = inst },
            },
            0x9000 => switch (inst & 0x000F) {
                0x0 => Instruction{ .SNE_Vx_Vy = @bitCast(inst) },
                else => Instruction{ .invalid = inst },
            },
            0xA000 => Instruction{ .LD_I = @bitCast(inst) },
            0xB000 => Instruction{ .JP_V0 = @bitCast(inst) },
            0xC000 => Instruction{ .RND = @bitCast(inst) },
            0xD000 => Instruction{ .DRW = @bitCast(inst) },
            0xE000 => switch (inst & 0x00FF) {
                0x9E => Instruction{ .SKP = @bitCast(inst) },
                0xA1 => Instruction{ .SKNP = @bitCast(inst) },
                else => Instruction{ .invalid = inst },
            },
            0xF000 => switch (inst & 0x00FF) {
                0x07 => Instruction{ .LD_DT = @bitCast(inst) },
                0x0A => Instruction{ .LD_K = @bitCast(inst) },
                0x15 => Instruction{ .LD_tDT = @bitCast(inst) },
                0x18 => Instruction{ .LD_tST = @bitCast(inst) },
                0x1E => Instruction{ .ADD_I = @bitCast(inst) },
                0x29 => Instruction{ .LD_F = @bitCast(inst) },
                0x33 => Instruction{ .LD_B = @bitCast(inst) },
                0x55 => Instruction{ .LD_wI = @bitCast(inst) },
                0x65 => Instruction{ .LD_rI = @bitCast(inst) },
                else => Instruction{ .invalid = inst },
            },
            else => Instruction{ .invalid = inst },
        };
    }

    // using comptime, we avoid having to write out the switch statements
    pub fn execute(self: *CPU, inst: Instruction) !void {
        switch (inst) {
            .invalid => |i| {
                _ = i; // autofix
                return error.Invalid;
                // @panic("invalid instruction");
            },
            .SYS => {
                // SYS is a noop, would be interesting to see if this can be extended
                // to allow ROMs to call certain routines, maybe add networking support?
            },
            inline else => |i, tag| {
                if (!@hasDecl(@This(), @tagName(tag))) {
                    // @panic("instruction not implemented");
                    return error.Unimplemented;
                }

                const f = @field(@This(), @tagName(tag));

                if (@TypeOf(i) == void) {
                    @call(.auto, f, .{self});
                } else {
                    @call(.auto, f, .{ self, inst });
                }
            },
        }
    }

    // 00E0 - CLS
    /// Clear the display.
    fn CLS(self: *CPU) void {
        for (&self.display) |*d| {
            @memset(d, 0);
        }
    }

    // 00EE - RET
    /// Return from a subroutine
    fn RET(self: *CPU) void {
        self.stack_pop();
    }

    // 1nnn - JP addr
    /// Jump to location nnn
    fn JP(self: *CPU, inst: Instruction) void {
        self.PC = inst.JP.addr;
    }

    // 2nnn - CALL addr
    /// Call subroutine at nnn
    fn CALL(self: *CPU, inst: Instruction) void {
        self.stack_push();
        self.PC = inst.CALL.addr;
    }

    // 3xkk - SE Vx, byte
    /// Skip next instruction if Vx = kk
    fn SE_Vx(self: *CPU, inst: Instruction) void {
        if (self.register[inst.SE_Vx.Vx] == inst.SE_Vx.byte)
            self.PC += 2;
    }

    // 4xkk - SNE Vx, byte
    /// Skip next instruction if Vx != kk
    fn SNE_Vx(self: *CPU, inst: Instruction) void {
        if (self.register[inst.SNE_Vx.Vx] != inst.SNE_Vx.byte)
            self.PC += 2;
    }

    // 5xy0 - SE Vx, Vy
    /// Skip next instruction if Vx = Vy
    fn SE_Vx_Vy(self: *CPU, inst: Instruction) void {
        if (self.register[inst.SE_Vx_Vy.Vx] == self.register[inst.SE_Vx_Vy.Vy])
            self.PC += 2;
    }

    // 6xkk - LD Vx, byte
    /// Set Vx = kk
    fn LD_Vx(self: *CPU, inst: Instruction) void {
        self.register[inst.LD_Vx.Vx] = inst.LD_Vx.byte;
    }

    // 7xkk - ADD Vx, byte
    /// Set Vx = Vx + kk
    fn ADD_Vx(self: *CPU, inst: Instruction) void {
        self.register[inst.ADD_Vx.Vx] +%= inst.ADD_Vx.byte;
    }

    // Annn - LD I, addr
    /// Set I = nnn
    fn LD_I(self: *CPU, inst: Instruction) void {
        self.I = inst.LD_I.addr;
    }

    // Dxyn - DRW Vx, Vy, nibble
    /// Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision
    fn DRW(self: *CPU, inst: Instruction) void {
        const col = self.register[inst.DRW.Vx] % WIDTH;
        const row = self.register[inst.DRW.Vy] % HEIGHT;
        self.register[0xF] = 0;

        for (0..inst.DRW.nibble) |r| {
            if (row + r >= HEIGHT) break;
            const sprite = self.memory[self.I + r];

            for (0..8) |c| {
                if (col + c >= WIDTH) break;
                const s = (sprite >> @truncate(7 - c)) & 1;

                if (s == 1 and self.display[row + r][col + c] == 1) {
                    self.register[0xF] = 1;
                    self.display[row + r][col + c] = 0;
                } else if (s == 1 and self.display[row + r][col + c] == 0) {
                    self.display[row + r][col + c] = 1;
                }
            }
        }
    }
};

const Address: type = packed struct(u16) { addr: u12, _: u4 };
const RegByte: type = packed struct(u16) { byte: u8, Vx: u4, _: u4 };
const RegReg: type = packed struct(u16) { _pad0: u4, Vy: u4, Vx: u4, _pad1: u4 };
const RegRegNib: type = packed struct(u16) { nibble: u4, Vy: u4, Vx: u4, _: u4 };
const Reg: type = packed struct(u16) { _pad0: u8, Vx: u4, _pad1: u4 };

const Instruction = union(enum) {
    invalid: u16,
    SYS,
    CLS,
    RET,
    JP: Address,
    CALL: Address,
    SE_Vx: RegByte,
    SNE_Vx: RegByte,
    SE_Vx_Vy: RegReg,
    LD_Vx: RegByte,
    ADD_Vx: RegByte,
    LD_Vx_Vy: RegReg,
    OR: RegReg,
    AND: RegReg,
    XOR: RegReg,
    ADD_Vx_Vy: RegReg,
    SUB: RegReg,
    SHR: RegReg,
    SUBN: RegReg,
    SHL: RegReg,
    SNE_Vx_Vy: RegReg,
    LD_I: Address,
    JP_V0: Address,
    RND: RegByte,
    DRW: RegRegNib,
    SKP: Reg,
    SKNP: Reg,
    LD_DT: Reg,
    LD_K: Reg,
    LD_tDT: Reg,
    LD_tST: Reg,
    ADD_I: Reg,
    LD_F: Reg,
    LD_B: Reg,
    LD_wI: Reg,
    LD_rI: Reg,
};

test "cpu fetch" {
    var c = CPU{ .PC = 0 };
    c.memory[0] = 0xBE;
    c.memory[1] = 0xEF;

    try std.testing.expectEqual(0xBEEF, c.fetch());
}

test "ibm.ch8" {
    const rom = @embedFile("ibm.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        std.debug.print("{}\n", .{ii});

        try c.execute(ii);

        if (prev == i)
            run = false;
        prev = i;
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }
}

test "1-chip8-logo.ch8" {
    const rom = @embedFile("1-chip8-logo.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        std.debug.print("{}\n", .{ii});

        try c.execute(ii);

        if (prev == i)
            run = false;
        prev = i;
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }
}

test "2-ibm-logo.ch8" {
    const rom = @embedFile("2-ibm-logo.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        std.debug.print("{}\n", .{ii});

        try c.execute(ii);

        if (prev == i)
            run = false;
        prev = i;
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }
}

test "3-corax+.ch8" {
    const rom = @embedFile("3-corax+.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        std.debug.print("{}\n", .{ii});

        try c.execute(ii);

        if (prev == i)
            run = false;
        prev = i;
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }
}

test "4-flags.ch8" {
    const rom = @embedFile("4-flags.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        std.debug.print("{}\n", .{ii});

        try c.execute(ii);

        if (prev == i)
            run = false;
        prev = i;
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }
}

test "5-quirks.ch8" {
    const rom = @embedFile("5-quirks.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        std.debug.print("{}\n", .{ii});

        try c.execute(ii);

        if (prev == i)
            run = false;
        prev = i;
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }
}

test "6-keypad.ch8" {
    const rom = @embedFile("6-keypad.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        std.debug.print("{}\n", .{ii});

        try c.execute(ii);

        if (prev == i)
            run = false;
        prev = i;
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }
}

test "7-beep.ch8" {
    const rom = @embedFile("7-beep.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        std.debug.print("{}\n", .{ii});

        try c.execute(ii);

        if (prev == i)
            run = false;
        prev = i;
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }
}

test "8-scrolling.ch8" {
    const rom = @embedFile("8-scrolling.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        std.debug.print("{}\n", .{ii});

        try c.execute(ii);

        if (prev == i)
            run = false;
        prev = i;
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }
}
