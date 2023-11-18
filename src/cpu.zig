const std = @import("std");

const Config = @import("config.zig");
const Ppu = @import("ppu.zig").Ppu;
const Apu = @import("apu.zig").Apu;
const mapper = @import("mapper.zig");
const Mapper = mapper.Mapper;

pub const ram_size = 1 << 16;

const PS = packed struct {
    carry: bool,
    zero: bool,
    interrupt: bool,
    decimal: bool,
    b_flag: bool,
    b_flag2: bool,
    overflow: bool,
    negative: bool
};

const AddressingMode = enum {
    zero_page,
    zero_page_x,
    zero_page_y,
    absolute,
    absolute_x,
    absolute_y,
    immediate,
    accumulator,
    indirect_x,
    indirect_y,
};

const Op = struct {
    name: [3]u8,
    code: u8,
    bytes: u8,
    cycles: u8,
    mode: ?AddressingMode,
};

pub const Cpu = struct {
    n_steps: usize = 0,
    cycle: usize = 0,
    ram: *[ram_size]u8,

    ppu: *Ppu,
    apu: *Apu,
    mapper: *Mapper,

    // Registers
    // Default values https://www.nesdev.org/wiki/CPU_power_up_state

    // Program Counter (PC)
    pc: u16 = 0x34,
    // Stack Pointer (S)
    sp: u8 = 0xfd,
    // Accumulator (A)
    acc: u8 = 0,
    // Index X (X)
    ix: u8 = 0,
    // Index Y (X)
    iy: u8 = 0,
    // Processor Status (P)
    ps: PS = @bitCast(PS, @as(u8, 0x24)),

    nmi: bool = false,

    page_crossed: bool = false,

    log_instructions: bool,

    read_input: bool = false,
    input_register: u8 = 0,

    const Self = @This();

    fn readMem(self: *Self, address: u16) u8 {
        const addr = if (address >= 0x2008 and address < 0x4000)
            0x2000 + (address % 8)
        else
            address;
        return switch (addr) {
            0x2002 => self.ppu.regStatus(),
            0x2005 => @panic("PPUSCROLL read!"),
            0x2007 => self.ppu.readVram(),
            0x4014 => @panic("OAM read!"),
            0x4015 => @panic("APU STATUS read!"),
            0x4016, 0x4017 => blk: {
                const bit = self.input_register & 1;
                self.input_register >>= 1;
                break :blk bit;
            },
            0x8000...0xBFFF => self.mapper.prgRom0()[addr - 0x8000],
            0xC000...0xFFFF => self.mapper.prgRom1()[addr - 0xC000],
            else => self.ram[addr]
        };
    }

    pub fn writeInput(self: *Self, input: u8) void {
        std.debug.assert(self.read_input);
        self.input_register = input;
    }

    // PPU registers: 0x2000 to 0x2007, mirrored up to 0x3FFF
    pub fn writeMem(self: *Self, address: u16, byte: u8) void {
        // Register mirroring
        const addr = if (address >= 0x2008 and address < 0x4000)
            0x2000 + (address % 8)
        else
            address;
        switch (addr) {
            0x2000 => self.ppu.writeRegCtrl(byte),
            0x2001 => self.ppu.writeRegMask(byte),
            0x2003 => self.ppu.writeOamAddr(byte),
            0x2004 => self.ppu.writeOam(byte),
            0x2005 => self.ppu.writeRegScroll(byte),
            0x2006 => self.ppu.writeVramAddr(byte),
            0x2007 => self.ppu.writeVram(byte),
            0x4000...0x4003 => self.apu.writePulse(addr, byte),
            0x4004...0x4007 => self.apu.writePulse(addr, byte),
            0x4008...0x400B => self.apu.writeTriangle(addr, byte),
            0x400C...0x400F => self.apu.writeNoise(addr, byte),
            0x4015 => self.apu.writeStatusRegister(byte),
            0x4014 => {
                const old_cycle = self.cycle;
                var oam_addr = @as(u16, byte) << 8;
                var i: usize = 0;
                while (i < 256) : (i += 1) {
                    self.ppu.oam[i] = self.ram[oam_addr];
                    oam_addr += 1;
                }
                self.cycle += (513 + @as(usize, if (old_cycle % 2 == 0) 0 else 1));
            },
            0x4016 => switch (byte & 1) {
                0 => self.read_input = false,
                1 => self.read_input = true,
                else => unreachable,
            },
            0x4017 => self.apu.writeFrameCounter(byte),
            0x8000...0xFFFF => self.mapper.write(addr, byte),
            else => self.ram[addr] = byte,
        }
    }

    pub fn getStack(self: *Self) []u8 {
        return self.ram[0x0100..0x01ff + 1];
    }

    fn pushStack(self: *Self, byte: u8) void {
        var stack = self.getStack();
        stack[self.sp] = byte;
        self.sp -= 1;
    }

    fn popStack(self: *Self) u8 {
        self.sp += 1;
        var byte = self.getStack()[self.sp];
        return byte;
    }

    fn handleNmi(self: *Self) void {
        std.debug.assert(self.nmi);
        const addr = self.absoluteIndexed(0xFFFA);
        const bytes = @bitCast([2]u8, self.pc);
        self.pushStack(@bitCast(u8, bytes[1]));
        self.pushStack(@bitCast(u8, bytes[0]));
        self.pushStack(@bitCast(u8, self.ps));
        self.pc = addr;
        self.ps.b_flag = true;
        self.nmi = false;
    }

    pub fn step(self: *Self) u16 {
        const start_cycle = self.cycle;

        if (self.nmi) {
            self.handleNmi();
            return 7;
        }
        const opcode = self.readMem(self.pc);
        const op = ops[opcode];
        if (op != null) {
            doOp(self, op.?);
        } else {
            const msg = std.fmt.allocPrint(std.heap.c_allocator, "Unhandled opcode 0x{x:0>2}", .{opcode}) catch unreachable;
            @panic(msg);
        }
        self.n_steps += 1;
        self.page_crossed = false;

        return @intCast(u16, self.cycle - start_cycle);
    }

    fn opMem(self: *Self, op: Op) u16 {
        return switch(op.mode.?) {
            .indirect_x => blk: {
                const inst = self.readMem(self.pc + 1);
                const a = self.readMem(inst +% self.ix);
                const b = self.readMem(inst +% self.ix +% 1);
                break :blk @as(u16, b) << 8 ^ a;
            },
            .indirect_y => blk: {
                const inst = self.readMem(self.pc + 1);
                const a = self.readMem(inst);

                var next_inst: u8 = undefined;
                const overflow = @addWithOverflow(u8, inst, 1, &next_inst);
                const b = self.readMem(next_inst);

                const base = (@as(u16, b) << 8 ^ a);
                var address: u16 = undefined;
                const overflow2 = @addWithOverflow(u16, base, self.iy, &address);

                if (overflow or overflow2)
                    self.cycle += 1;
                break :blk address;
            },
            .absolute => self.absolute(),
            .absolute_x => blk: {
                const lsb = self.readMem(self.pc + 1);
                var msb = self.readMem(self.pc + 2);

                var part1: u8 = undefined;
                const overflow = @addWithOverflow(u8, lsb, self.ix, &part1);
                if (overflow) {
                    msb +%= 1;
                    self.page_crossed = true;
                }
                const address = ((@as(u16, msb) << 8) ^ part1);
                break :blk address;
            },
            .absolute_y => blk: {
                const lsb = self.readMem(self.pc + 1);
                var msb = self.readMem(self.pc + 2);

                var part1: u8 = undefined;
                const overflow = @addWithOverflow(u8, lsb, self.iy, &part1);
                if (overflow) {
                    msb +%= 1;
                    self.page_crossed = true;
                }
                const address = ((@as(u16, msb) << 8) ^ part1);
                break :blk address;
            },
            .immediate => self.pc + 1,
            .zero_page => self.readMem(self.pc + 1),
            .zero_page_x => self.readMem(self.pc + 1) +% self.ix,
            .zero_page_y => self.readMem(self.pc + 1) +% self.iy,
            .accumulator => unreachable
        };
    }

    fn updateFlags(self: *Self, val: u8, comptime flags: anytype) void {
        inline for (flags) |flag| {
            comptime switch (flag) {
                .z => self.ps.zero = val == 0,
                .c => self.ps.zero = val == 0,
                .o => self.ps.zero = val == 0,
                .n => self.ps.negative = val >> 7 & 1 == 1,
                else => unreachable
            };
        }
    }

    inline fn absolute(self: *Cpu) u16 {
        return self.absoluteIndexed(self.pc + 1);
    }

    pub inline fn absoluteIndexed(self: *Cpu, start: u16) u16 {
        const msb = @as(u16, self.readMem(start + 1)) << 8;
        const lsb = self.readMem(start);
        return msb | lsb;
    }

    fn relative(self: *Cpu) u16 {
        const offset = self.readMem(self.pc + 1);
        // offset is a signed integer, convert to unsigned if negative
        return if (offset >> 7 & 1 == 1)
            self.pc - (~offset + 1)
        else
            self.pc + offset;
    }
};

const ops = blk: {
    var all_ops: [0xff]?Op = undefined;
    std.mem.set(?Op, &all_ops, null);
    var tmp = [_]Op {
        .{ .name = "ADC".*, .code = 0x61, .bytes = 2, .cycles = 6, .mode = .indirect_x },
        .{ .name = "ADC".*, .code = 0x65, .bytes = 2, .cycles = 3, .mode = .zero_page},
        .{ .name = "ADC".*, .code = 0x69, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "ADC".*, .code = 0x6d, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "ADC".*, .code = 0x71, .bytes = 2, .cycles = 5, .mode = .indirect_y },
        .{ .name = "ADC".*, .code = 0x75, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "ADC".*, .code = 0x79, .bytes = 3, .cycles = 4, .mode = .absolute_y },
        .{ .name = "ADC".*, .code = 0x7d, .bytes = 3, .cycles = 4, .mode = .absolute_x },
        .{ .name = "AND".*, .code = 0x21, .bytes = 2, .cycles = 6, .mode = .indirect_x },
        .{ .name = "AND".*, .code = 0x21, .bytes = 2, .cycles = 6, .mode = .indirect_x },
        .{ .name = "AND".*, .code = 0x25, .bytes = 2, .cycles = 3, .mode = .zero_page},
        .{ .name = "AND".*, .code = 0x29, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "AND".*, .code = 0x29, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "AND".*, .code = 0x2d, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "AND".*, .code = 0x31, .bytes = 2, .cycles = 5, .mode = .indirect_y },
        .{ .name = "AND".*, .code = 0x35, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "AND".*, .code = 0x39, .bytes = 3, .cycles = 4, .mode = .absolute_y },
        .{ .name = "AND".*, .code = 0x3d, .bytes = 3, .cycles = 4, .mode = .absolute_x },
        .{ .name = "ASL".*, .code = 0x06, .bytes = 2, .cycles = 5, .mode = .zero_page },
        .{ .name = "ASL".*, .code = 0x0a, .bytes = 1, .cycles = 2, .mode = .accumulator },
        .{ .name = "ASL".*, .code = 0x0e, .bytes = 3, .cycles = 6, .mode = .absolute },
        .{ .name = "ASL".*, .code = 0x16, .bytes = 2, .cycles = 6, .mode = .zero_page_x },
        .{ .name = "ASL".*, .code = 0x1e, .bytes = 3, .cycles = 7, .mode = .absolute_x },
        .{ .name = "BCC".*, .code = 0x90, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "BCS".*, .code = 0xb0, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "BEQ".*, .code = 0xf0, .bytes = 2, .cycles = 2, .mode = null },
        .{ .name = "BIT".*, .code = 0x24, .bytes = 2, .cycles = 3, .mode = .zero_page },
        .{ .name = "BIT".*, .code = 0x2c, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "BMI".*, .code = 0x30, .bytes = 2, .cycles = 2, .mode = .immediate},
        .{ .name = "BNE".*, .code = 0xd0, .bytes = 2, .cycles = 2, .mode = null },
        .{ .name = "BPL".*, .code = 0x10, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "BRK".*, .code = 0x00, .bytes = 1, .cycles = 7, .mode = null },
        .{ .name = "BVC".*, .code = 0x50, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "BVS".*, .code = 0x70, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "CLC".*, .code = 0x18, .bytes = 1, .cycles = 2, .mode = null},
        .{ .name = "CLD".*, .code = 0xd8, .bytes = 1, .cycles = 2, .mode = null},
        .{ .name = "CLV".*, .code = 0xb8, .bytes = 1, .cycles = 2, .mode = null},
        .{ .name = "CMP".*, .code = 0xc1, .bytes = 2, .cycles = 6, .mode = .indirect_x },
        .{ .name = "CMP".*, .code = 0xc5, .bytes = 2, .cycles = 3, .mode = .zero_page},
        .{ .name = "CMP".*, .code = 0xc9, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "CMP".*, .code = 0xcd, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "CMP".*, .code = 0xd1, .bytes = 2, .cycles = 5, .mode = .indirect_y },
        .{ .name = "CMP".*, .code = 0xd5, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "CMP".*, .code = 0xd9, .bytes = 3, .cycles = 4, .mode = .absolute_y },
        .{ .name = "CMP".*, .code = 0xdd, .bytes = 3, .cycles = 4, .mode = .absolute_x },
        .{ .name = "CPX".*, .code = 0xe0, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "CPX".*, .code = 0xe4, .bytes = 2, .cycles = 3, .mode = .zero_page },
        .{ .name = "CPX".*, .code = 0xec, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "CPY".*, .code = 0xc0, .bytes = 2, .cycles = 2, .mode = .immediate},
        .{ .name = "CPY".*, .code = 0xc4, .bytes = 2, .cycles = 3, .mode = .zero_page },
        .{ .name = "CPY".*, .code = 0xcc, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "DEC".*, .code = 0xc6, .bytes = 2, .cycles = 5, .mode = .zero_page },
        .{ .name = "DEC".*, .code = 0xce, .bytes = 3, .cycles = 6, .mode = .absolute },
        .{ .name = "DEC".*, .code = 0xd6, .bytes = 2, .cycles = 6, .mode = .zero_page_x },
        .{ .name = "DEC".*, .code = 0xde, .bytes = 3, .cycles = 7, .mode = .absolute_x },
        .{ .name = "DEX".*, .code = 0xca, .bytes = 1, .cycles = 2, .mode = null },
        .{ .name = "DEY".*, .code = 0x88, .bytes = 1, .cycles = 2, .mode = null },
        .{ .name = "EOR".*, .code = 0x41, .bytes = 2, .cycles = 6, .mode = .indirect_x },
        .{ .name = "EOR".*, .code = 0x45, .bytes = 2, .cycles = 3, .mode = .zero_page},
        .{ .name = "EOR".*, .code = 0x49, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "EOR".*, .code = 0x4d, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "EOR".*, .code = 0x51, .bytes = 2, .cycles = 5, .mode = .indirect_y },
        .{ .name = "EOR".*, .code = 0x55, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "EOR".*, .code = 0x59, .bytes = 3, .cycles = 4, .mode = .absolute_y },
        .{ .name = "EOR".*, .code = 0x5d, .bytes = 3, .cycles = 4, .mode = .absolute_x },
        .{ .name = "INC".*, .code = 0xe6, .bytes = 2, .cycles = 5, .mode = .zero_page },
        .{ .name = "INC".*, .code = 0xee, .bytes = 3, .cycles = 6, .mode = .absolute },
        .{ .name = "INC".*, .code = 0xf6, .bytes = 2, .cycles = 6, .mode = .zero_page_x },
        .{ .name = "INC".*, .code = 0xfe, .bytes = 3, .cycles = 7, .mode = .absolute_x },
        .{ .name = "INX".*, .code = 0xe8, .bytes = 1, .cycles = 2, .mode = null },
        .{ .name = "INY".*, .code = 0xc8, .bytes = 1, .cycles = 2, .mode = null },
        .{ .name = "JMP".*, .code = 0x4c, .bytes = 3, .cycles = 3, .mode = null },
        .{ .name = "JMP".*, .code = 0x6c, .bytes = 3, .cycles = 5, .mode = null },
        .{ .name = "JSR".*, .code = 0x20, .bytes = 3, .cycles = 6, .mode = .absolute },
        .{ .name = "LDA".*, .code = 0xa1, .bytes = 2, .cycles = 6, .mode = .indirect_x },
        .{ .name = "LDA".*, .code = 0xa5, .bytes = 2, .cycles = 3, .mode = .zero_page},
        .{ .name = "LDA".*, .code = 0xa9, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "LDA".*, .code = 0xad, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "LDA".*, .code = 0xb1, .bytes = 2, .cycles = 5, .mode = .indirect_y },
        .{ .name = "LDA".*, .code = 0xb5, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "LDA".*, .code = 0xb9, .bytes = 3, .cycles = 4, .mode = .absolute_y },
        .{ .name = "LDA".*, .code = 0xbd, .bytes = 3, .cycles = 4, .mode = .absolute_x },
        .{ .name = "LDX".*, .code = 0xa2, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "LDX".*, .code = 0xa6, .bytes = 2, .cycles = 3, .mode = .zero_page },
        .{ .name = "LDX".*, .code = 0xae, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "LDX".*, .code = 0xb6, .bytes = 2, .cycles = 4, .mode = .zero_page_y },
        .{ .name = "LDX".*, .code = 0xbe, .bytes = 3, .cycles = 4, .mode = .absolute_y },
        .{ .name = "LDY".*, .code = 0xa0, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "LDY".*, .code = 0xa4, .bytes = 2, .cycles = 3, .mode = .zero_page },
        .{ .name = "LDY".*, .code = 0xac, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "LDY".*, .code = 0xb4, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "LDY".*, .code = 0xbc, .bytes = 3, .cycles = 4, .mode = .absolute_x },
        .{ .name = "LSR".*, .code = 0x46, .bytes = 2, .cycles = 5, .mode = .zero_page },
        .{ .name = "LSR".*, .code = 0x4a, .bytes = 1, .cycles = 2, .mode = .accumulator },
        .{ .name = "LSR".*, .code = 0x4e, .bytes = 3, .cycles = 6, .mode = .absolute },
        .{ .name = "LSR".*, .code = 0x56, .bytes = 2, .cycles = 6, .mode = .zero_page_x },
        .{ .name = "LSR".*, .code = 0x5e, .bytes = 3, .cycles = 7, .mode = .absolute_x },
        .{ .name = "NOP".*, .code = 0xea, .bytes = 1, .cycles = 2, .mode = null},
        .{ .name = "ORA".*, .code = 0x01, .bytes = 2, .cycles = 6, .mode = .indirect_x },
        .{ .name = "ORA".*, .code = 0x05, .bytes = 2, .cycles = 3, .mode = .zero_page},
        .{ .name = "ORA".*, .code = 0x09, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "ORA".*, .code = 0x0d, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "ORA".*, .code = 0x11, .bytes = 2, .cycles = 5, .mode = .indirect_y },
        .{ .name = "ORA".*, .code = 0x15, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "ORA".*, .code = 0x19, .bytes = 3, .cycles = 4, .mode = .absolute_y },
        .{ .name = "ORA".*, .code = 0x1d, .bytes = 3, .cycles = 4, .mode = .absolute_x },
        .{ .name = "PHA".*, .code = 0x48, .bytes = 1, .cycles = 3, .mode = null},
        .{ .name = "PHP".*, .code = 0x08, .bytes = 1, .cycles = 3, .mode = null},
        .{ .name = "PLA".*, .code = 0x68, .bytes = 1, .cycles = 4, .mode = null},
        .{ .name = "PLP".*, .code = 0x28, .bytes = 1, .cycles = 4, .mode = null},
        .{ .name = "ROL".*, .code = 0x26, .bytes = 2, .cycles = 5, .mode = .zero_page },
        .{ .name = "ROL".*, .code = 0x2a, .bytes = 1, .cycles = 2, .mode = .accumulator },
        .{ .name = "ROL".*, .code = 0x2e, .bytes = 3, .cycles = 6, .mode = .absolute },
        .{ .name = "ROL".*, .code = 0x36, .bytes = 2, .cycles = 6, .mode = .zero_page_x },
        .{ .name = "ROL".*, .code = 0x3e, .bytes = 3, .cycles = 7, .mode = .absolute_x },
        .{ .name = "ROR".*, .code = 0x66, .bytes = 2, .cycles = 5, .mode = .zero_page },
        .{ .name = "ROR".*, .code = 0x6a, .bytes = 1, .cycles = 2, .mode = .accumulator },
        .{ .name = "ROR".*, .code = 0x6e, .bytes = 3, .cycles = 6, .mode = .absolute },
        .{ .name = "ROR".*, .code = 0x76, .bytes = 2, .cycles = 6, .mode = .zero_page_x },
        .{ .name = "ROR".*, .code = 0x7e, .bytes = 3, .cycles = 7, .mode = .absolute_x },
        .{ .name = "RTI".*, .code = 0x40, .bytes = 1, .cycles = 6, .mode = null},
        .{ .name = "RTS".*, .code = 0x60, .bytes = 1, .cycles = 6, .mode = null},
        .{ .name = "SBC".*, .code = 0xe1, .bytes = 2, .cycles = 6, .mode = .indirect_x },
        .{ .name = "SBC".*, .code = 0xe5, .bytes = 2, .cycles = 3, .mode = .zero_page},
        .{ .name = "SBC".*, .code = 0xe9, .bytes = 2, .cycles = 2, .mode = .immediate },
        .{ .name = "SBC".*, .code = 0xed, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "SBC".*, .code = 0xf1, .bytes = 2, .cycles = 5, .mode = .indirect_y },
        .{ .name = "SBC".*, .code = 0xf5, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "SBC".*, .code = 0xf9, .bytes = 3, .cycles = 4, .mode = .absolute_y },
        .{ .name = "SBC".*, .code = 0xfd, .bytes = 3, .cycles = 4, .mode = .absolute_x },
        .{ .name = "SEC".*, .code = 0x38, .bytes = 1, .cycles = 2, .mode = null},
        .{ .name = "SED".*, .code = 0xf8, .bytes = 1, .cycles = 2, .mode = null},
        .{ .name = "SEI".*, .code = 0x78, .bytes = 1, .cycles = 2, .mode = null},
        .{ .name = "STA".*, .code = 0x81, .bytes = 2, .cycles = 6, .mode = .indirect_x },
        .{ .name = "STA".*, .code = 0x85, .bytes = 2, .cycles = 3, .mode = .zero_page },
        .{ .name = "STA".*, .code = 0x8d, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "STA".*, .code = 0x91, .bytes = 2, .cycles = 6, .mode = .indirect_y },
        .{ .name = "STA".*, .code = 0x95, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "STA".*, .code = 0x99, .bytes = 3, .cycles = 5, .mode = .absolute_y },
        .{ .name = "STA".*, .code = 0x9d, .bytes = 3, .cycles = 5, .mode = .absolute_x },
        .{ .name = "STX".*, .code = 0x86, .bytes = 2, .cycles = 3, .mode = .zero_page },
        .{ .name = "STX".*, .code = 0x8e, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "STX".*, .code = 0x96, .bytes = 2, .cycles = 4, .mode = .zero_page_y },
        .{ .name = "STY".*, .code = 0x84, .bytes = 2, .cycles = 3, .mode = .zero_page },
        .{ .name = "STY".*, .code = 0x8c, .bytes = 3, .cycles = 4, .mode = .absolute },
        .{ .name = "STY".*, .code = 0x94, .bytes = 2, .cycles = 4, .mode = .zero_page_x },
        .{ .name = "TAX".*, .code = 0xaa, .bytes = 1, .cycles = 2, .mode = .accumulator },
        .{ .name = "TAY".*, .code = 0xa8, .bytes = 1, .cycles = 2, .mode = .accumulator },
        .{ .name = "TSX".*, .code = 0xba, .bytes = 1, .cycles = 2, .mode = null },
        .{ .name = "TXA".*, .code = 0x8a, .bytes = 1, .cycles = 2, .mode = .accumulator },
        .{ .name = "TXS".*, .code = 0x9a, .bytes = 1, .cycles = 2, .mode = null },
        .{ .name = "TYA".*, .code = 0x98, .bytes = 1, .cycles = 2, .mode = .accumulator },
    };
    for (tmp) |op| {
        all_ops[op.code] = op;
    }
    break :blk all_ops;
};

fn printOp(cpu: *Cpu, op: Op, w: anytype) void {
    if (op.mode == null) {
        w.print("{s}", .{op.name}) catch unreachable;
        return;
    }
    switch (op.mode.?) {
        .indirect_x => {
            const inst = cpu.readMem(cpu.pc + 1);
            w.print("{s} (${X:0>2}, X)", .{op.name, inst}) catch unreachable;
        },
        .absolute  => {
            const msb = (@as(u16, cpu.readMem(cpu.pc + 2)) << 8);
            const lsb = cpu.readMem(cpu.pc + 1);
            const address = msb | lsb;
            w.print("{s} ${X:0>4}", .{op.name, address}) catch unreachable;
        },
        .immediate => {
            const byte = cpu.readMem(cpu.pc + 1);
            w.print("{s} #${X:0>2}", .{op.name, byte}) catch unreachable;
        },
        .zero_page => {
            const address = cpu.readMem(cpu.pc + 1);
            const val = cpu.ram[address];
            w.print("{s} ${X:0>2} = {X:0>2}", .{op.name, address, val}) catch unreachable;
        },
        else => w.print("{s}", .{op.name}) catch unreachable,
    }
}

fn doOp(cpu: *Cpu, op: Op) void {
    var buf = std.mem.zeroes([100]u8);
    var fba = std.io.fixedBufferStream(&buf);
    var w = fba.writer();

    const old_pc = cpu.pc;
    const old_ps = cpu.ps;
    const old_sp = cpu.sp;
    const old_acc = cpu.acc;
    const old_ix = cpu.ix;
    const old_iy = cpu.iy;
    const old_cycle = cpu.cycle;

    var inc_pc = true;
    printOp(cpu, op, w);
    switch (op.code) {
        0x00 => {
            const pc_bytes = @bitCast([2]u8, cpu.pc);
            cpu.pushStack(pc_bytes[0]);
            cpu.pushStack(pc_bytes[1]);
            cpu.pushStack(@bitCast(u8, cpu.ps));
            cpu.pc = cpu.absoluteIndexed(0xFFFE);
            cpu.ps.b_flag = true;
        },
        0x4c => {
            cpu.pc = cpu.absolute();
            inc_pc = false;
        },
        0x6c => {
            const abs = cpu.absolute();
            // Special handling for page boundary due to hardware bug in the original NES CPU
            if (abs & 0xFF == 0xFF) {
                const msb_address = (abs >> 8) << 8;
                const address = (@as(u16, cpu.readMem(msb_address)) << 8) ^ cpu.readMem(abs);
                cpu.pc = address;
                inc_pc = false;
            } else {
                const address = (@as(u16, cpu.readMem(abs + 1)) << 8) ^ cpu.readMem(abs);
                cpu.pc = address;
                inc_pc = false;
            }
        },
        0xa2, 0xa6, 0xb6, 0xae, 0xbe => {
            cpu.ix = cpu.readMem(cpu.opMem(op));
            cpu.updateFlags(cpu.ix, .{.z, .n});
            if (cpu.page_crossed) cpu.cycle += 1;
        },
        0xa0, 0xa4, 0xb4, 0xac, 0xbc => {
            cpu.iy = cpu.readMem(cpu.opMem(op));
            cpu.updateFlags(cpu.iy, .{.z, .n});
            if (cpu.page_crossed) cpu.cycle += 1;
        },
        0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91 => {
            cpu.writeMem(cpu.opMem(op), cpu.acc);
        },
        0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11 => {
            cpu.acc |= cpu.readMem(cpu.opMem(op));
            cpu.updateFlags(cpu.acc, .{.z, .n});
        },
        0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31 => {
            cpu.acc &= cpu.readMem(cpu.opMem(op));
            cpu.updateFlags(cpu.acc, .{.z, .n});
        },
        0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51 => {
            cpu.acc ^= cpu.readMem(cpu.opMem(op));
            cpu.updateFlags(cpu.acc, .{.z, .n});
        },
        0x86, 0x96, 0x8e  => {
            cpu.writeMem(cpu.opMem(op), cpu.ix);
        },
        0x84, 0x94, 0x8c => {
            cpu.writeMem(cpu.opMem(op), cpu.iy);
        },
        0x20 => {
            // const bytes = cpu.ram[cpu.pc..][0..3];
            const bytes: [3]u8 = .{
                cpu.readMem(cpu.pc),
                cpu.readMem(cpu.pc + 1),
                cpu.readMem(cpu.pc + 2),
            };
            const pc = @bitCast([2]u8, cpu.pc + 2);
            cpu.pushStack(pc[1]);
            cpu.pushStack(pc[0]);
            cpu.pc = @bitCast(u16, bytes[1..].*);
            inc_pc = false;
        },
        0xea, => {
            // NOP
        },
        0x38 => {
            cpu.ps.carry = true;
        },
        0x18 => {
            cpu.ps.carry = false;
        },
        0xb8 => {
            cpu.ps.overflow = false;
        },
        0xb0 => {
            if (cpu.ps.carry) {
                cpu.pc = cpu.relative();
                cpu.cycle += 1;
            }
        },
        0x90 => {
            if (!cpu.ps.carry) {
                cpu.pc = cpu.relative();
                cpu.cycle += 1;
            }
        },
        0xf0 => {
            if (cpu.ps.zero) {
                cpu.pc = cpu.relative();
                cpu.cycle += 1;
            }
        },
        0x70 => {
            if (cpu.ps.overflow) {
                cpu.pc = cpu.relative();
                cpu.cycle += 1;
            }
        },
        0x30 => {
            if (cpu.ps.negative) {
                cpu.pc = cpu.relative();
                cpu.cycle += 1;
            }
        },
        0x50 => {
            if (!cpu.ps.overflow) {
                cpu.pc = cpu.relative();
                cpu.cycle += 1;
            }
        },
        0x10 => {
            if (!cpu.ps.negative) {
                cpu.pc = cpu.relative();
                cpu.cycle += 1;
            }
        },
        0xd0 => {
            if (!cpu.ps.zero) {
                cpu.pc = cpu.relative();
                cpu.cycle += 1;
            }
        },
        0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1 => {
            cpu.acc = cpu.readMem(cpu.opMem(op));
            cpu.updateFlags(cpu.acc, .{.z, .n});
            if (cpu.page_crossed) cpu.cycle += 1;
        },
        0x24, 0x2c => {
            const value = cpu.readMem(cpu.opMem(op));
            const mask = cpu.acc;
            const result = value & mask;
            cpu.ps.zero = result == 0;
            cpu.ps.overflow = (value >> 6) & 1 == 1;
            cpu.ps.negative = (value >> 7) & 1 == 1;
        },
        0x60 => {
            const a = cpu.popStack();
            const b = cpu.popStack();
            var pc = ((@as(u16, b) << 8) ^ a) + 1;
            cpu.pc = pc;
            inc_pc = false;
        },
        0x40 => {
            cpu.ps = @bitCast(PS, cpu.popStack());
            cpu.ps.b_flag2 = true; // TODO: is this correct?
            const a = cpu.popStack();
            const b = cpu.popStack();
            var pc = ((@as(u16, b) << 8) ^ a);
            cpu.pc = pc;
            inc_pc = false;
        },
        0x78 => {
            cpu.ps.interrupt = true;
        },
        0xf8 => {
            cpu.ps.decimal = true;
        },
        0xd8 => {
            cpu.ps.decimal = false;
        },
        0x08 => {
            var ps = cpu.ps;
            // TODO: might not be correct, see https://www.nesdev.org/wiki/Status_flags#The_B_flag
            ps.b_flag = true;
            ps.b_flag2 = true;
            cpu.pushStack(@bitCast(u8, ps));
        },
        0x28 => {
            var ps = @bitCast(PS, cpu.popStack());
            ps.b_flag = false;
            ps.b_flag2 = true;
            cpu.ps = ps;
        },
        0x68 => {
            cpu.acc = cpu.popStack();
            cpu.updateFlags(cpu.acc, .{.z, .n});
        },
        // TODO: extract common fn (only reg differs) >>>
        0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1 => {
            var result: u8 = undefined;
            const overflowed = @subWithOverflow(u8, cpu.acc, cpu.readMem(cpu.opMem(op)), &result);
            cpu.ps.carry = result >= 0 and !overflowed;
            cpu.updateFlags(result, .{.z, .n});
        },
        0xe0, 0xe4, 0xec => {
            var result: u8 = undefined;
            const overflowed = @subWithOverflow(u8, cpu.ix, cpu.readMem(cpu.opMem(op)), &result);
            cpu.ps.carry = result >= 0 and !overflowed;
            cpu.updateFlags(result, .{.z, .n});
        },
        0xc0, 0xc4, 0xcc => {
            var result: u8 = undefined;
            const overflowed = @subWithOverflow(u8, cpu.iy, cpu.readMem(cpu.opMem(op)), &result);
            cpu.ps.carry = result >= 0 and !overflowed;
            cpu.updateFlags(result, .{.z, .n});
        },
        // <<<
        0x48 => {
            cpu.pushStack(@bitCast(u8, cpu.acc));
        },
        0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71 => {
            const value = cpu.readMem(cpu.opMem(op));
            const old = cpu.acc;
            const tmp: u16 = cpu.acc;
            const new = tmp + value + @boolToInt(cpu.ps.carry);
            cpu.acc = @truncate(u8, new);
            cpu.ps.carry = (new >> 8) & 1 == 1;
            cpu.ps.zero = cpu.acc == 0;
            cpu.ps.negative = cpu.acc >> 7 & 1 == 1;
            // https://forums.nesdev.org/viewtopic.php?p=60520#p60520
            cpu.ps.overflow = ((old ^ cpu.acc) & (value ^ cpu.acc) & 0x80) != 0;
        },

        // TODO: duplicate of ADC, extract common fn
        // https://stackoverflow.com/a/29224684
        0xe9, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1 => {
            const value = ~cpu.readMem(cpu.opMem(op));
            const old = cpu.acc;
            const tmp: u16 = cpu.acc;
            const new = tmp + value + @boolToInt(cpu.ps.carry);
            cpu.acc = @truncate(u8, new);
            cpu.ps.carry = (new >> 8) & 1 == 1;
            cpu.ps.zero = cpu.acc == 0;
            cpu.ps.negative = cpu.acc >> 7 & 1 == 1;
            // https://forums.nesdev.org/viewtopic.php?p=60520#p60520
            cpu.ps.overflow = ((old ^ cpu.acc) & (value ^ cpu.acc) & 0x80) != 0;
        },
        0xe6, 0xf6, 0xee, 0xfe => {
            const addr = cpu.opMem(op);
            const value = cpu.readMem(addr);
            const new_value = value +% 1;
            cpu.writeMem(addr, new_value);
            cpu.updateFlags(new_value, .{.z, .n});
        },
        0xe8 => {
            cpu.ix +%= 1;
            cpu.updateFlags(cpu.ix, .{.z, .n});
        },
        0xc8 => {
            cpu.iy +%= 1;
            cpu.updateFlags(cpu.iy, .{.z, .n});
        },
        0xc6, 0xd6, 0xce, 0xde => {
            const addr = cpu.opMem(op);
            const value = cpu.readMem(addr);
            const new_value = value -% 1;
            cpu.writeMem(addr, new_value);
            cpu.updateFlags(new_value, .{.z, .n});
        },
        0xca => {
            cpu.ix -%= 1;
            cpu.updateFlags(cpu.ix, .{.z, .n});
        },
        0x88 => {
            cpu.iy -%= 1;
            cpu.updateFlags(cpu.iy, .{.z, .n});
        },
        0xaa => {
            cpu.ix = cpu.acc;
            cpu.updateFlags(cpu.ix, .{.z, .n});
        },
        0xa8 => {
            cpu.iy = cpu.acc;
            cpu.updateFlags(cpu.iy, .{.z, .n});
        },
        0xba => {
            cpu.ix = cpu.sp;
            cpu.updateFlags(cpu.ix, .{.z, .n});
        },
        0x8a => {
            cpu.acc = cpu.ix;
            cpu.updateFlags(cpu.acc, .{.z, .n});
        },
        0x9a => {
            cpu.sp = cpu.ix;
        },
        0x98 => {
            cpu.acc = cpu.iy;
            cpu.updateFlags(cpu.acc, .{.z, .n});
        },
        0x4a, 0x46, 0x56, 0x4e, 0x5e => {
            if (op.mode.? == .accumulator) {
                const old = cpu.acc;
                const new_value = old >> 1;
                cpu.acc = new_value;
                cpu.ps.carry = old & 1 == 1;
                cpu.updateFlags(new_value, .{.z, .n});
            } else {
                const addr = cpu.opMem(op);
                const old = cpu.readMem(addr);
                const new_value = old >> 1;
                cpu.writeMem(addr, new_value);
                cpu.ps.carry = old & 1 == 1;
                cpu.updateFlags(new_value, .{.z, .n});
            }
        },
        0x0a, 0x06, 0x16, 0x0e, 0x1e => {
            if (op.mode.? == .accumulator) {
                const old = cpu.acc;
                const new_value = old << 1;
                cpu.acc = new_value;
                cpu.ps.carry = old >> 7 & 1 == 1;
                cpu.updateFlags(new_value, .{.z, .n});
            } else {
                const addr = cpu.opMem(op);
                const old = cpu.readMem(addr);
                const new_value = old << 1;
                cpu.writeMem(addr, new_value);
                cpu.ps.carry = old >> 7 & 1 == 1;
                cpu.updateFlags(new_value, .{.z, .n});
            }
        },
        0x6a, 0x66, 0x76, 0x6e, 0x7e => {
            if (op.mode.? == .accumulator) {
                const old = cpu.acc;
                var new_value = old >> 1;
                new_value ^= @as(u8, (if (cpu.ps.carry) 1 else 0)) << 7;
                cpu.acc = new_value;
                cpu.ps.carry = old & 1 == 1;
                cpu.updateFlags(new_value, .{.z, .n});
            } else {
                const addr = cpu.opMem(op);
                const old = cpu.readMem(addr);
                var new_value = old >> 1;
                new_value ^= @as(u8, (if (cpu.ps.carry) 1 else 0)) << 7;
                cpu.writeMem(addr, new_value);
                cpu.ps.carry = old & 1 == 1;
                cpu.updateFlags(new_value, .{.z, .n});
            }
        },
        0x2a, 0x26, 0x36, 0x2e, 0x3e => {
            if (op.mode.? == .accumulator) {
                const old = cpu.acc;
                var new_value = old << 1;
                new_value ^= @as(u8, (if (cpu.ps.carry) 1 else 0));
                cpu.acc = new_value;
                cpu.ps.carry = old >> 7 & 1 == 1;
                cpu.updateFlags(new_value, .{.z, .n});
            } else {
                const addr = cpu.opMem(op);
                const old = cpu.readMem(addr);
                var new_value = old << 1;
                new_value ^= @as(u8, (if (cpu.ps.carry) 1 else 0));
                cpu.writeMem(addr, new_value);
                cpu.ps.carry = old >> 7 & 1 == 1;
                cpu.updateFlags(new_value, .{.z, .n});
            }
        },
        else => unreachable,
    }
    cpu.cycle += op.cycles;
    if (inc_pc)
        cpu.pc += op.bytes;

    if (cpu.log_instructions) {
        var cbuf = std.mem.zeroes([20]u8);
        var args = std.mem.zeroes([20]u8);
        var i: u16 = 0;
        while (i < op.bytes) : (i += 1) {
            args[i] = cpu.readMem(old_pc + i);
        }
        std.log.info("{d: <4} {X}  {s}  {s: <15} A: {X:0>2} X: {X:0>2} Y: {X:0>2} P: {X:0>2} SP: {X:0>2} PPU:  ({d},{d}) CYC: {d}", .{
            cpu.n_steps + 1,
            old_pc,
            printBytes(&cbuf, args[0..op.bytes]) catch unreachable,
            fba.getWritten(),
            old_acc,
            old_ix,
            old_iy,
            @bitCast(u8, old_ps),
            old_sp,
            cpu.ppu.scanline,
            cpu.ppu.cycle,
            old_cycle,
        });
    }
}

fn printBytes(buf: []u8, bytes: []const u8) ![]const u8 {
    return switch (bytes.len) {
        1 => try std.fmt.bufPrint(buf, "{X:0>2}      ", .{bytes[0]}),
        2 => try std.fmt.bufPrint(buf, "{X:0>2} {X:0>2}   ", .{bytes[0], bytes[1]}),
        3 => try std.fmt.bufPrint(buf, "{X:0>2} {X:0>2} {X:0>2}", .{bytes[0], bytes[1], bytes[2]}),
        else => unreachable,
    };
}
