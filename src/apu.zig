const std = @import("std");

const Resampler = @import("Resampler.zig");

pub const Apu = struct {
    frame_counter: usize = 0,
    total_cycles: usize = 0,

    resampler: *Resampler,

    pulse1: Pulse = .{ .sweep = .{ .change_modifier = 1 }},
    pulse2: Pulse = .{ .sweep = .{ .change_modifier = 0 }},
    triangle: Triangle = .{},
    noise: Noise = .{},
    dmc: Dmc = .{},

    status: Status = .{},

    const Self = @This();

    const lengths = [32]u8{
        10, 254, 20, 2,  40, 4,  80, 6,  160, 8,  60, 10, 14, 12, 26, 14,
        12, 16,  24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30
    };

    const PulseReg1 = packed struct {
        volume: u4,
        constant_volume: bool,
        length_counter_halt: bool,
        duty_cycle: u2,
    };

    const PulseSweepReg = packed struct {
        shift_count: u3 = 0,
        negate: bool = false,
        period: u3 = 0,
        enabled: bool = false,
    };

    pub fn writePulse(self: *Self, addr: u16, byte: u8) void {
        var p = switch (addr) {
            0x4000...0x4003 => &self.pulse1,
            0x4004...0x4007 => &self.pulse2,
            else => unreachable,
        };
        switch (addr) {
            0x4000, 0x4004 => {
                const reg = @bitCast(PulseReg1, byte);
                p.volume = reg.volume;
                p.env.constant_volume = reg.constant_volume;
                p.env.timer_reload = reg.volume;
                p.duty_cycle = reg.duty_cycle;
            },
            0x4001, 0x4005 => {
                const reg = @bitCast(PulseSweepReg, byte);
                p.sweep.shift_count = reg.shift_count;
                p.sweep.negate = reg.negate;
                p.sweep.enabled = reg.enabled;
                p.sweep.timer_reload = reg.period;
                p.sweep.timer_reload += 1;
                p.sweep.restart = true;
            },
            0x4002, 0x4006 => {
                p.timer_reload = (p.timer_reload & 0b111_0000_0000) ^ byte;
            },
            0x4003, 0x4007 => {
                p.length_counter = lengths[byte >> 3];
                p.timer_reload = ((p.timer_reload & 0b000_1111_1111) ^ @as(u11, byte) << 8) + 1;
                p.env.restart = true;
                p.wave_index = 0;
            },
            else => unreachable,
        }
    }

    const TriangleReg1 = packed struct {
        linear_reload: u7,
        control_flag: bool,
    };

    pub fn writeTriangle(self: *Self, addr: u16, byte: u8) void {
        var tri = &self.triangle;
        switch (addr) {
            0x4008 => {
                const reg = @bitCast(TriangleReg1, byte);
                self.triangle.linear_reload = reg.linear_reload;
                self.triangle.control = reg.control_flag;
            },
            0x400A => {
                tri.timer_reload = (tri.timer_reload & 0b111_0000_0000) ^ byte;
            },
            0x400B => {
                tri.length_counter = lengths[byte >> 3];
                tri.timer_reload = ((tri.timer_reload & 0b000_1111_1111) ^ @as(u11, byte) << 8) + 1;
                self.triangle.linear_restart = true;
            },
            else => unreachable,
        }
    }

    const NoiseReg1 = packed struct {
        volume: u4,
        constant_volume: bool,
        length_counter_halt: bool,
        _unused: u2,
    };

    const NoiseReg2 = packed struct {
        timer_index: u4,
        _unused: u3,
        tone_mode: bool,
    };

    pub fn writeNoise(self: *Self, addr: u16, byte: u8) void {
        var n = &self.noise;
        switch (addr) {
            0x400C => {
                const reg = @bitCast(NoiseReg1, byte);
                std.debug.assert(!reg.length_counter_halt);
                n.env.constant_volume = reg.constant_volume;
                n.env.timer_reload = reg.volume;
                n.volume = reg.volume;
            },
            0x400E => {
                const reg = @bitCast(NoiseReg2, byte);
                n.timer_reload = noise_periods[reg.timer_index];
                n.tone_mode = reg.tone_mode;
            },
            0x400F => {
                n.length_counter = lengths[byte >> 3];
                n.env.restart = true;
            },
            else => unreachable,
        }
    }

    pub fn writeDmc(self: *Self, addr: u16, byte: u8) void {
        switch (addr) {
            0x4011 => self.dmc.load_counter = byte,
            else => {},
        }
    }

    const Status = packed struct {
        pulse1_enabled: bool = false,
        pulse2_enabled: bool = false,
        triangle_enabled: bool = false,
        noise_enabled: bool = false,
        dmc_enabled: bool = false,
        _unused: u3 = undefined,
    };

    pub fn writeStatusRegister(self: *Self, byte: u8) void {
        self.status = @bitCast(Status, byte);
        if (!self.status.pulse1_enabled)   self.pulse1.length_counter = 0;
        if (!self.status.pulse2_enabled)   self.pulse2.length_counter = 0;
        if (!self.status.triangle_enabled) self.triangle.length_counter = 0;
        if (!self.status.noise_enabled)    self.noise.length_counter = 0;
    }

    const FrameCounterReg = packed struct {
        _unused: u6,
        irq_inhibit: bool,
        mode: u1,
    };

    // TODO: write mode, IRQ inhibit
    pub fn writeFrameCounter(self: *Self, byte: u8) void {
        _ = byte;
        // const reg = @bitCast(FrameCounterReg, byte);
        // std.debug.assert(reg.irq_inhibit);
        // std.debug.assert(reg.mode == 0);
        self.frame_counter = 0;
    }

    pub fn step(self: *Self) void {
        var tri = &self.triangle;
        var p1 = &self.pulse1;
        var p2 = &self.pulse2;
        var n = &self.noise;
        var dmc = &self.dmc;

        const tri_active = self.status.triangle_enabled and
            self.triangle.length_counter > 0 and
            self.triangle.linear_current > 0 and
            self.triangle.timer_reload > 2;
        const pulse1_active = self.status.pulse1_enabled and
            p1.length_counter > 0 and
            p1.timer_current > 8 and
            p1.timer_reload < 0x7FF;
        const pulse2_active = self.status.pulse2_enabled and
            p2.length_counter > 0 and
            p2.timer_current > 8 and
            p2.timer_reload < 0x7FF;
        const noise_active = self.status.noise_enabled and
            n.length_counter > 0;
        const dmc_active = self.status.dmc_enabled;

        if (tri_active) {
            if (tri.timer_current == 0) {
                tri.wave_index += 1;
                if (tri.wave_index >= 32) {
                    tri.wave_index = 0;
                }
                tri.timer_current = tri.timer_reload;
            } else {
                tri.timer_current -= 1;
            }
        }

        if (self.frame_counter % 2 == 0) {
            if (p1.timer_current == 0) {
                p1.wave_index += 1;
                if (p1.wave_index >= pulse_waves[0].len) {
                    p1.wave_index = 0;
                }
                p1.timer_current = p1.timer_reload;
            } else {
                p1.timer_current -= 1;
            }

            if (p2.timer_current == 0) {
                p2.wave_index += 1;
                if (p2.wave_index >= pulse_waves[0].len) {
                    p2.wave_index = 0;
                }
                p2.timer_current = p2.timer_reload;
            } else {
                p2.timer_current -= 1;
            }

            if (n.timer_current == 0) {
                n.step();
                n.timer_current = n.timer_reload;
            } else {
                n.timer_current -= 1;
            }
        }

        const is_half_frame = self.frame_counter % 7456 == 0;
        const is_quarter_frame = self.frame_counter % 3728 == 0;
        if (is_half_frame) {
            if (p1.length_counter > 0) p1.length_counter -= 1;
            if (p2.length_counter > 0) p2.length_counter -= 1;
            if (tri.length_counter > 0) tri.length_counter -= 1;
            if (n.length_counter > 0) n.length_counter -= 1;

            p1.sweep.step(p1);
            p2.sweep.step(p2);
        } else if (is_quarter_frame) {
            p1.env.step();
            p2.env.step();
            n.env.step();

            if (self.status.triangle_enabled and self.triangle.length_counter > 0) {
                if (self.triangle.linear_restart) {
                    self.triangle.linear_current = self.triangle.linear_reload;
                } else if (self.triangle.linear_current > 0) {
                    self.triangle.linear_current -= 1;
                }
            }
            if (!self.triangle.control) {
                self.triangle.linear_restart = false;
            }
        }

        const sample = Sample{
            .pulse1 = if (pulse1_active) p1.out() else 0,
            .pulse2 = if (pulse2_active) p2.out() else 0,
            .triangle = triangles[tri.wave_index],
            .noise = if (noise_active) n.out() else 0,
            .dmc = if (dmc_active) dmc.load_counter else 0,
        };
        self.resampler.push(sample.mixLookup());

        self.total_cycles += 1;
        self.frame_counter += 1;
        if (self.frame_counter == 29830)
            self.frame_counter = 0;
    }
};

pub const Pulse = struct {
    volume: u8 = 0,
    length_counter: u8 = 0,
    duty_cycle: u2 = 0,

    env: Envelope = .{},
    sweep: Sweep,

    wave_index: u8 = 0,
    timer_reload: u11 = 0,
    timer_current: u11 = 0,

    const Self = @This();

    pub fn out(self: *Self) u8 {
        const volume = if (self.env.constant_volume) self.volume else self.env.decay_level;
        return pulse_waves[self.duty_cycle][self.wave_index] * volume;
    }
};

const pulse_waves = [4][8]u8{
    [_]u8{ 0, 1, 0, 0, 0, 0, 0, 0, },
    [_]u8{ 0, 1, 1, 0, 0, 0, 0, 0, },
    [_]u8{ 0, 1, 1, 1, 1, 0, 0, 0, },
    [_]u8{ 1, 0, 0, 1, 1, 1, 1, 1, },
};

const Envelope = struct {
    constant_volume: bool = false,
    restart: bool = false,
    timer_reload: u8 = 0,
    timer_current: u8 = 0,
    decay_level: u8 = 0,

    pub fn step(self: *Envelope) void {
        if (self.restart) {
            self.restart = false;
            self.decay_level = 15;
            self.timer_current = self.timer_reload;
        } else if (self.timer_current > 0) {
            self.timer_current -= 1;
        }
        if (self.timer_current == 0) {
            self.timer_current = self.timer_reload;
            if (self.decay_level > 0) {
                self.decay_level -= 1;
            }
        }
    }
};

const Sweep = struct {
    change_modifier: u8,

    shift_count: u3 = 0,
    negate: bool = false,
    period: u3 = 0,
    enabled: bool = false,

    timer_current: u8 = 0,
    timer_reload: u8 = 0,
    restart: bool = false,

    const Self = @This();

    fn step(self: *Self, pulse: *Pulse) void {
        if (self.timer_current == 0 and self.enabled and self.shift_count > 0) {
            const change = pulse.timer_reload >> self.shift_count;
            pulse.timer_reload = if (self.negate) (pulse.timer_reload -| change -| self.change_modifier) else pulse.timer_reload +| change;
        }

        if (self.timer_current == 0 or self.restart) {
            self.timer_current = self.timer_reload;
            self.restart = false;
        } else {
            self.timer_current -= 1;
        }
    }
};


pub const Triangle = struct {
    length_counter: u8 = 0,
    linear_current: u7 = 0,
    linear_reload: u7 = 0,
    linear_restart: bool = false,
    control: bool = false, // length counter halt / linear counter control

    wave_index: u8 = 0,

    timer_reload: u11 = 0,
    timer_write: u1 = 0,
    timer_current: u11 = 0,
};

const triangles = [_]u8{
    15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15
};

const noise_periods = [_]u16{
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068
};

pub const Noise = struct {
    volume: u4 = 0,
    reg: u15 = 1,
    tone_mode: bool = false,

    timer_reload: u16 = 0,
    timer_current: u16 = 0,

    length_counter: u16 = 0,

    env: Envelope = .{},

    const Self = @This();

    fn step(self: *Self) void {
        const shift: u4 = if (self.tone_mode) 6 else 1;
        const feedback = ((self.reg >> shift) & 1) ^ (self.reg & 1);
        self.reg = (self.reg >> 1) | (feedback << 14);
    }

    pub fn out(self: *Self) u8 {
        if (self.reg & 1 == 1) return 0;
        return if (self.env.constant_volume) self.volume else self.env.decay_level;
    }
};

const Dmc = struct {
    load_counter: u8 = 0,
};

pub const Sample = struct {
    triangle: u8 = 0,
    pulse1: u8 = 0,
    pulse2: u8 = 0,
    noise: u8 = 0,
    dmc: u8 = 0,

    const Self = @This();

    const pulse_table = blk: {
        var table: [31]f32 = undefined;
        for (table) |*s, i| {
            s.* = 95.52 / (8128.0 / @intToFloat(f32, i) + 100);
        }
        break :blk table;
    };

    const tnd_table = blk: {
        var table: [203]f32 = undefined;
        for (table) |*s, i| {
            s.* = 163.67 / (24329.0 / @intToFloat(f32, i) + 100);
        }
        break :blk table;
    };

    // For debugging
    fn mixLinear(self: Self) f32 {
        const pulse_out = 0.00752 * (@intToFloat(f32, self.pulse1) + @intToFloat(f32, self.pulse2));
        const tnd_out = 0.00851 * @intToFloat(f32, self.triangle) + 0.00494 * @intToFloat(f32, self.noise) + 0.00335 * @intToFloat(f32, self.dmc);
        return pulse_out + tnd_out;
    }

    // https://www.nesdev.org/wiki/APU_Mixer#Lookup_Table
    fn mixLookup(self: Self) f32 {
        const pulse_out = pulse_table[self.pulse1 + self.pulse2];
        const tnd_out = tnd_table[3 * self.triangle + 2 * self.noise + self.dmc];
        return pulse_out + tnd_out;

    }
};
