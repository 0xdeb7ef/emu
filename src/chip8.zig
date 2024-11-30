const std = @import("std");
const random = std.Random.DefaultPrng;

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

    /// The original COSMAC VIP set the Vx register to Vy before shifting.
    /// This option allows you to control that behaviour.
    old_shift: bool = false,

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

        self.SP -= 1;
        self.PC = self.stack[self.SP];
    }

    fn get_key(self: *CPU) union(enum) { key: u4, no_key: void } {
        for (self.keyboard, 0..) |k, i| {
            if (k) return .{ .key = @truncate(i) };
        }

        return .no_key;
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
            .invalid => {
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
    /// Clear the display
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
        const addr = inst.JP.addr;

        self.PC = addr;
    }

    // 2nnn - CALL addr
    /// Call subroutine at nnn
    fn CALL(self: *CPU, inst: Instruction) void {
        const addr = inst.CALL.addr;

        self.stack_push();
        self.PC = addr;
    }

    // 3xkk - SE Vx, byte
    /// Skip next instruction if Vx = kk
    fn SE_Vx(self: *CPU, inst: Instruction) void {
        const Vx = inst.SE_Vx.Vx;
        const byte = inst.SE_Vx.byte;

        if (self.register[Vx] == byte)
            self.PC += 2;
    }

    // 4xkk - SNE Vx, byte
    /// Skip next instruction if Vx != kk
    fn SNE_Vx(self: *CPU, inst: Instruction) void {
        const Vx = inst.SNE_Vx.Vx;
        const byte = inst.SNE_Vx.byte;

        if (self.register[Vx] != byte)
            self.PC += 2;
    }

    // 5xy0 - SE Vx, Vy
    /// Skip next instruction if Vx = Vy
    fn SE_Vx_Vy(self: *CPU, inst: Instruction) void {
        const Vx = inst.SE_Vx_Vy.Vx;
        const Vy = inst.SE_Vx_Vy.Vy;

        if (self.register[Vx] == self.register[Vy])
            self.PC += 2;
    }

    // 6xkk - LD Vx, byte
    /// Set Vx = kk
    fn LD_Vx(self: *CPU, inst: Instruction) void {
        const Vx = inst.LD_Vx.Vx;
        const byte = inst.LD_Vx.byte;

        self.register[Vx] = byte;
    }

    // 7xkk - ADD Vx, byte
    /// Set Vx = Vx + kk
    fn ADD_Vx(self: *CPU, inst: Instruction) void {
        const Vx = inst.ADD_Vx.Vx;
        const byte = inst.ADD_Vx.byte;

        self.register[Vx] +%= byte;
    }

    // 8xy0 - LD Vx, Vy
    /// Set Vx = Vy
    fn LD_Vx_Vy(self: *CPU, inst: Instruction) void {
        const Vx = inst.LD_Vx_Vy.Vx;
        const Vy = inst.LD_Vx_Vy.Vy;

        self.register[Vx] = self.register[Vy];
    }

    // 8xy1 - OR Vx, Vy
    /// Set Vx = Vx OR Vy
    fn OR(self: *CPU, inst: Instruction) void {
        const Vx = inst.OR.Vx;
        const Vy = inst.OR.Vy;

        self.register[Vx] |= self.register[Vy];
    }

    // 8xy2 - AND Vx, Vy
    /// Set Vx = Vx AND Vy
    fn AND(self: *CPU, inst: Instruction) void {
        const Vx = inst.AND.Vx;
        const Vy = inst.AND.Vy;

        self.register[Vx] &= self.register[Vy];
    }

    // 8xy3 - XOR Vx, Vy
    /// Set Vx = Vx XOR Vy
    fn XOR(self: *CPU, inst: Instruction) void {
        const Vx = inst.XOR.Vx;
        const Vy = inst.XOR.Vy;

        self.register[Vx] ^= self.register[Vy];
    }

    // 8xy4 - ADD Vx, Vy
    /// Set Vx = Vx + Vy, set VF = carry
    fn ADD_Vx_Vy(self: *CPU, inst: Instruction) void {
        const Vx = inst.ADD_Vx_Vy.Vx;
        const Vy = inst.ADD_Vx_Vy.Vy;

        const a = @addWithOverflow(self.register[Vx], self.register[Vy]);

        // Vx has to be set BEFORE VF!
        self.register[Vx] = a.@"0";
        self.register[0xF] = a.@"1";
    }

    // 8xy5 - SUB Vx, Vy
    /// Set Vx = Vx - Vy, set VF = NOT borrow
    fn SUB(self: *CPU, inst: Instruction) void {
        const Vx = inst.SUB.Vx;
        const Vy = inst.SUB.Vy;

        const o: u8 = if (self.register[Vx] >= self.register[Vy]) 1 else 0;

        self.register[Vx] -%= self.register[Vy];
        self.register[0xF] = o;
    }

    // 8xy6 - SHR Vx {, Vy}
    /// Set Vx = Vx SHR 1
    fn SHR(self: *CPU, inst: Instruction) void {
        const Vx = inst.SHR.Vx;
        const Vy = inst.SHR.Vy;

        if (self.old_shift)
            self.register[Vx] = self.register[Vy];

        const f = self.register[Vx] & 1;
        self.register[Vx] >>= 1;
        self.register[0xF] = f;
    }

    // 8xy7 - SUBN Vx, Vy
    /// Set Vx = Vy - Vx, set VF = NOT borrow
    fn SUBN(self: *CPU, inst: Instruction) void {
        const Vx = inst.SUBN.Vx;
        const Vy = inst.SUBN.Vy;

        const o: u8 = if (self.register[Vy] >= self.register[Vx]) 1 else 0;
        self.register[Vx] = self.register[Vy] -% self.register[Vx];
        self.register[0xF] = o;
    }

    // 8xyE - SHL Vx {, Vy}
    /// Set Vx = Vx SHL 1
    fn SHL(self: *CPU, inst: Instruction) void {
        const Vx = inst.SHL.Vx;
        const Vy = inst.SHL.Vy;

        if (self.old_shift)
            self.register[Vx] = self.register[Vy];

        const f = (self.register[Vx] & (1 << 7)) >> 7;
        self.register[Vx] <<= 1;
        self.register[0xF] = f;
    }

    // 9xy0 - SNE Vx, Vy
    /// Skip next instruction if Vx != Vy
    fn SNE_Vx_Vy(self: *CPU, inst: Instruction) void {
        const Vx = inst.SNE_Vx_Vy.Vx;
        const Vy = inst.SNE_Vx_Vy.Vy;

        if (self.register[Vx] != self.register[Vy])
            self.PC += 2;
    }

    // Annn - LD I, addr
    /// Set I = nnn
    fn LD_I(self: *CPU, inst: Instruction) void {
        const addr = inst.LD_I.addr;

        self.I = addr;
    }

    // Bnnn - JP V0, addr
    /// Jump to location nnn + V0
    fn JP_V0(self: *CPU, inst: Instruction) void {
        const addr = inst.JP_V0.addr;

        self.PC = addr + self.register[0x0];
    }

    // Cxkk - RND Vx, byte
    /// Set Vx = random byte AND kk
    fn RND(self: *CPU, inst: Instruction) void {
        const Vx = inst.RND.Vx;
        const byte = inst.RND.byte;

        var rand = random.init(0);
        const r = std.Random.int(rand.random(), u8);
        self.register[Vx] = r & byte;
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

    // Ex9E - SKP Vx
    /// Skip next instruction if key with the value of Vx is pressed
    fn SKP(self: *CPU, inst: Instruction) void {
        const Vx = inst.SKP.Vx;

        if (self.keyboard[Vx]) {
            self.PC += 2;
        }
    }

    // ExA1 - SKNP Vx
    /// Skip next instruction if key with the value of Vx is not pressed
    fn SKNP(self: *CPU, inst: Instruction) void {
        const Vx = inst.SKNP.Vx;

        if (!self.keyboard[Vx]) {
            self.PC += 2;
        }
    }

    // Fx07 - LD Vx, DT
    /// Set Vx = delay timer value
    fn LD_DT(self: *CPU, inst: Instruction) void {
        const Vx = inst.LD_DT.Vx;

        self.register[Vx] = self.delay;
    }

    // Fx0A - LD Vx, K
    /// Wait for a key press, store the value of the key in Vx
    fn LD_K(self: *CPU, inst: Instruction) void {
        const Vx = inst.LD_K.Vx;

        const key = self.get_key();
        switch (key) {
            .key => |k| {
                self.register[Vx] = k;
            },
            .no_key => {
                self.PC -= 2;
            },
        }
    }

    // Fx15 - LD DT, Vx
    /// Set delay timer = Vx
    fn LD_tDT(self: *CPU, inst: Instruction) void {
        const Vx = inst.LD_tDT.Vx;

        self.delay = self.register[Vx];
    }

    // Fx18 - LD ST, Vx
    // Set sound timer = Vx
    fn LD_tST(self: *CPU, inst: Instruction) void {
        const Vx = inst.LD_tST.Vx;

        self.sound = self.register[Vx];
    }

    // Fx1E - ADD I, Vx
    /// Set I = I + Vx
    fn ADD_I(self: *CPU, inst: Instruction) void {
        const Vx = inst.ADD_I.Vx;

        self.I += self.register[Vx];
    }

    // Fx29 - LD F, Vx
    /// Set I = location of sprite for digit Vx
    fn LD_F(self: *CPU, inst: Instruction) void {
        const Vx = inst.LD_F.Vx;

        self.I = CPU.FONT_START + Vx;
    }

    // Fx33 - LD B, Vx
    /// Store BCD representation of Vx in memory locations I, I+1, and I+2
    fn LD_B(self: *CPU, inst: Instruction) void {
        const v = self.register[inst.LD_B.Vx];

        self.memory[self.I] = v / 100;
        self.memory[self.I + 1] = (v / 10) % 10;
        self.memory[self.I + 2] = v % 10;
    }

    // Fx55 - LD [I], Vx
    /// Store registers V0 through Vx in memory starting at location I
    fn LD_wI(self: *CPU, inst: Instruction) void {
        const Vx = inst.LD_wI.Vx;

        for (0..Vx + 1) |i| {
            self.memory[self.I + i] = self.register[i];
        }
    }

    // Fx65 - LD Vx, [I]
    /// Read registers V0 through Vx from memory starting at location I
    fn LD_rI(self: *CPU, inst: Instruction) void {
        const Vx = inst.LD_rI.Vx;

        for (0..Vx + 1) |i| {
            self.register[i] = self.memory[self.I + i];
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
    if (false) return error.SkipZigTest;
    var c = CPU{ .PC = 0 };
    c.memory[0] = 0xBE;
    c.memory[1] = 0xEF;

    try std.testing.expectEqual(0xBEEF, c.fetch());
}

test "1-chip8-logo.ch8" {
    if (false) return error.SkipZigTest;
    std.debug.print("1-chip-logo.ch8:\n", .{});

    const rom = @embedFile("1-chip8-logo.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        // std.debug.print("{}\n", .{ii});

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
    if (false) return error.SkipZigTest;
    std.debug.print("2-ibm-logo.ch8:\n", .{});

    const rom = @embedFile("2-ibm-logo.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        // std.debug.print("{}\n", .{ii});

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
    if (false) return error.SkipZigTest;
    std.debug.print("3-corax+.ch8:\n", .{});

    const rom = @embedFile("3-corax+.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        // std.debug.print("{}\n", .{ii});

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

    // yes, we are checking the pixels to see if we passed the tests
    var i: usize = 3;
    while (i <= 18) : (i += 5) {
        var j: usize = 11;
        while (j < 64) : (j += 16) {
            try std.testing.expectEqual(1, c.display[i][j]);
        }
    }

    i = 23;
    while (i <= 28) : (i += 5) {
        var j: usize = 11;
        while (j <= 43) : (j += 16) {
            try std.testing.expectEqual(1, c.display[i][j]);
        }
    }
}

test "4-flags.ch8" {
    if (false) return error.SkipZigTest;
    std.debug.print("4-flags.ch8:\n", .{});

    const rom = @embedFile("4-flags.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    for (0..1000) |_| {
        const i = c.fetch();
        const ii = CPU.decode(i);

        // std.debug.print("{}\n", .{ii});

        try c.execute(ii);
    }

    for (c.display) |col| {
        for (col) |d| {
            std.debug.print("{s}", .{if (d == 1) "\u{2588}" else " "});
        }
        std.debug.print("\n", .{});
    }

    // yes, we are, once again, checking the display pixels
    try std.testing.expect(checkPixelFlags(c, 2, 27, 3));
    try std.testing.expect(checkPixelFlags(c, 2, 49, 3));

    try std.testing.expect(checkPixelFlags(c, 7, 5, 3));
    try std.testing.expect(checkPixelFlags(c, 7, 27, 4));
    try std.testing.expect(checkPixelFlags(c, 7, 49, 4));

    try std.testing.expect(checkPixelFlags(c, 12, 5, 3));
    try std.testing.expect(checkPixelFlags(c, 12, 27, 4));
    try std.testing.expect(checkPixelFlags(c, 12, 49, 3));

    try std.testing.expect(checkPixelFlags(c, 18, 27, 4));
    try std.testing.expect(checkPixelFlags(c, 18, 49, 4));

    try std.testing.expect(checkPixelFlags(c, 23, 5, 3));
    try std.testing.expect(checkPixelFlags(c, 23, 27, 4));
    try std.testing.expect(checkPixelFlags(c, 23, 49, 3));

    try std.testing.expect(checkPixelFlags(c, 29, 31, 2));
}

fn checkPixelFlags(cpu: CPU, row: usize, col: usize, n: usize) bool {
    var i: usize = 0;
    while (i < n) : (i += 1) {
        if (cpu.display[row][col + (i * 4)] == 0)
            return false;
    }

    return true;
}

test "5-quirks.ch8" {
    // TODO - Add timer and other stuff to make this test work
    if (true) return error.SkipZigTest;
    std.debug.print("5-quirks.ch8:\n", .{});

    const rom = @embedFile("5-quirks.ch8");
    var c = CPU.init();

    // auto-select CHIP-8 mode
    c.memory[0x1FF] = 1;

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        // std.debug.print("{}\n", .{ii});

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
    // TODO - Add keyboard support
    if (true) return error.SkipZigTest;
    std.debug.print("6-keypad.ch8:\n", .{});

    const rom = @embedFile("6-keypad.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        // std.debug.print("{}\n", .{ii});

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
    // TODO - Add beep
    if (true) return error.SkipZigTest;
    std.debug.print("7-beep.ch8:\n", .{});

    const rom = @embedFile("7-beep.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        // std.debug.print("{}\n", .{ii});

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
    // not supported
    if (true) return error.SkipZigTest;
    std.debug.print("8-scrolling.ch8:\n", .{});

    const rom = @embedFile("8-scrolling.ch8");
    var c = CPU.init();

    c.load_mem(CPU.PC_START, rom);

    var prev: u16 = undefined;
    var run = true;
    while (run) {
        const i = c.fetch();
        const ii = CPU.decode(i);

        // std.debug.print("{}\n", .{ii});

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
