const std = @import("std");
const mapper = @import("mapper.zig");
const Cpu = @import("cpu.zig").Cpu;

const Mapper = mapper.Mapper;

const vram_size = 1 << 14;

const Sprite = packed struct {
    y: u8,
    tile_index: u8,
    attrs: SpriteAttrs,
    x: u8,
};

const SpriteAttrs = packed struct {
    palette: u2,
    _unused: u3,
    priority: u1,
    flip_horizontal: bool,
    flip_vertical: bool,
};

pub const Oam = struct {
    data: [256]u8 align(4),

    pub const len = 64;

    const Self = @This();

    pub fn get(self: *Self, index: u8) *Sprite {
        // @compileLog(@typeInfo(Oam).Struct.fields);
        // @compileLog(@alignOf(Sprite));
        // @compileLog(@alignOf(Oam));
        std.debug.assert(index < 64);
        return @ptrCast(@alignCast(self.data[index * 4 ..][0..4]));
    }
};

// https://www.nesdev.org/wiki/PPU_registers

const SpriteSize = enum(u1) {
    small, // 8x8
    large, // 8x16
};

const RegCtrl = packed struct {
    base_nametable_address: u2,
    vram_address_increment: u1,
    sprite_pattern_table_address: u1,
    background_pattern_table_address: u1,
    sprite_size: SpriteSize,
    ppu_select: u1,
    generate_nmi: bool,
};

const RegMask = packed struct {
    greyscale: bool,
    show_background_left: bool,
    show_sprite_left: bool,
    show_background: bool,
    show_sprites: bool,
    emphasize_red: bool,
    emphasize_green: bool,
    emphasize_blue: bool,
};

const RegStatus = packed struct {
    open_bus: u5,
    sprite_overflow: bool,
    sprite_zero_hit: bool,
    vblank: bool,
};

pub const TileRow = struct {
    row: u16,

    const Self = @This();

    pub fn getCol(self: Self, col: u16) u2 {
        std.debug.assert(col < 8);
        return @as(u2, @truncate(self.row >> @as(u4, @intCast((7 - col) * 2))));
    }
};

pub const PatternTable = struct {
    data: [0x1000]u8,

    const Self = @This();

    pub fn getTileRow(self: *Self, tile: u16, row: u16) TileRow {
        std.debug.assert(tile < 256);
        std.debug.assert(row < 8);
        const pattern_index = tile * 16 + row;
        const low = self.data[pattern_index];
        const high = self.data[pattern_index + 8];
        return TileRow{ .row = interleave0(low) ^ (interleave0(high) << 1) };
    }

    fn interleave0(_b: u8) u16 {
        var b: u16 = _b;
        b = (b ^ (b << 4)) & 0x0f0f;
        b = (b ^ (b << 2)) & 0x3333;
        b = (b ^ (b << 1)) & 0x5555;
        return b;
    }
};

pub const AttributeTable = struct {
    data: [64]u8,

    const Self = @This();

    // 1 tile row = 8 rows
    // 1 tile col = 8 cols
    pub fn get(self: *Self, tile_row: u16, tile_col: u16) u8 {
        const attr_index = (tile_row / 4) * 8 + tile_col / 4;
        const attr = self.data[attr_index];
        const rowq = (tile_row / 2) % 2;
        const colq = (tile_col / 2) % 2;
        const shift = @as(u3, @intCast((rowq << 1) ^ colq));
        return (attr >> (shift * 2)) & 0b11;
    }

    pub fn shiftAttr(attr: u8, tile_row: u16, tile_col: u16) u8 {
        const rowq = (tile_row / 2) % 2;
        const colq = (tile_col / 2) % 2;
        const shift = @as(u3, @intCast((rowq << 1) ^ colq));
        return (attr >> (shift * 2)) & 0b11;
    }
};

const Register = enum {
    scroll,
    addr,
};

const AddressRegister = packed struct {
    x_coarse: u5,
    y_coarse: u5,
    h_nametable: u1,
    v_nametable: u1,
    y_fine: u3,
};

const VramRegister = struct {
    // https://www.nesdev.org/wiki/PPU_scrolling#PPU_internal_registers
    // yyy NN YYYYY XXXXX
    // ||| || ||||| +++++-- coarse X scroll
    // ||| || +++++-------- coarse Y scroll
    // ||| ++-------------- nametable select
    // +++----------------- fine Y scroll
    reg: AddressRegister = @as(AddressRegister, @bitCast(@as(u15, 0))),
    tmp: AddressRegister = @as(AddressRegister, @bitCast(@as(u15, 0))),
    x_fine: u3 = 0,
    state: u1 = 0,

    const Self = @This();

    fn write(self: *Self, register: Register, v: u8) void {
        switch (register) {
            .scroll => switch (self.state) {
                0 => {
                    self.x_fine = @as(u3, @truncate(v));
                    self.tmp.x_coarse = @as(u5, @truncate(v >> 3));
                    self.state = 1;
                },
                1 => {
                    self.tmp.y_fine = @as(u3, @truncate(v));
                    self.tmp.y_coarse = @as(u5, @truncate(v >> 3));
                    self.state = 0;
                },
            },
            .addr => switch (self.state) {
                0 => {
                    const S = packed struct { lo: u8, hi: u6, bit: u1 };
                    var p = @as(*S, @ptrCast(&self.tmp));
                    p.hi = @as(u6, @truncate(v));
                    p.bit = 0;
                    self.state = 1;
                },
                1 => {
                    var p = @as(*[2]u8, @ptrCast(&self.tmp));
                    p[0] = v;
                    self.reg = self.tmp;
                    self.state = 0;
                },
            },
        }
    }

    fn regInt(self: *Self) u15 {
        return @as(u15, @bitCast(self.reg));
    }

    fn addr(self: *Self) u14 {
        return @as(u14, @truncate(self.regInt()));
    }

    // https://www.nesdev.org/wiki/PPU_scrolling#Tile_and_attribute_fetching
    fn getTileAddress(self: *Self) u16 {
        const t = 0x2000 | (self.regInt() & 0x0FFF);
        std.debug.assert(t >= 0x2000 and t < 0x3000);
        return t;
    }

    // https://www.nesdev.org/wiki/PPU_scrolling#Tile_and_attribute_fetching
    fn getAttributeAddress(self: *Self) u16 {
        const v = self.regInt();
        return 0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07);
    }

    fn incrementX(self: *Self) void {
        if (self.reg.x_coarse == 31) {
            self.reg.x_coarse = 0;
            self.reg.h_nametable ^= 1;
        } else {
            self.reg.x_coarse += 1;
        }
    }

    fn incrementY(self: *Self) void {
        if (self.reg.y_fine < 7) {
            self.reg.y_fine += 1;
        } else {
            self.reg.y_fine = 0;
            if (self.reg.y_coarse == 29) {
                self.reg.y_coarse = 0;
                self.reg.v_nametable ^= 1;
            } else if (self.reg.y_coarse == 31) {
                self.reg.y_coarse = 0;
            } else {
                self.reg.y_coarse += 1;
            }
        }
    }
};

pub const Ppu = struct {
    vram: [vram_size]u8 = std.mem.zeroes([vram_size]u8),
    palette: [32]u8 = std.mem.zeroes([32]u8),
    oam: [256]u8 align(4) = std.mem.zeroes([256]u8),
    pixels: [256]u8 = std.mem.zeroes([256]u8),
    frame: [240][256]u8 = std.mem.zeroes([240][256]u8),

    scanline: u16 = 0,
    cycle: u16 = 0,
    total_cycle: usize = 0,

    reg_ctrl: RegCtrl = @as(RegCtrl, @bitCast(@as(u8, 0))),
    reg_mask: RegMask = @as(RegMask, @bitCast(@as(u8, 0))),
    reg_status: RegStatus = @as(RegStatus, @bitCast(@as(u8, 0b1010_0000))),

    cpu: *Cpu,

    mirroring: mapper.Mirroring = undefined,
    logging: bool = false,

    vram_register: VramRegister = VramRegister{},

    oam_addr: u8 = 0,

    data_buffer: u8 = 0,

    const Self = @This();

    inline fn prevCycle(self: *Self) u16 {
        return self.cycle - 1;
    }

    pub fn patternTableLeft(self: *Self) *PatternTable {
        return @as(*PatternTable, @ptrCast(self.vram[0x0000..0x1000]));
    }

    pub fn patternTableRight(self: *Self) *PatternTable {
        return @as(*PatternTable, @ptrCast(self.vram[0x1000..0x2000]));
    }

    pub fn getBackgroundPatternTable(self: *Self) *PatternTable {
        return switch (self.reg_ctrl.background_pattern_table_address) {
            0 => self.patternTableLeft(),
            1 => self.patternTableRight(),
        };
    }

    pub fn getSpritePatternTable(self: *Self) *PatternTable {
        return switch (self.reg_ctrl.sprite_pattern_table_address) {
            0 => self.patternTableLeft(),
            1 => self.patternTableRight(),
        };
    }

    pub fn regStatus(self: *Self) u8 {
        const old = @as(u8, @bitCast(self.reg_status));
        self.vram_register.state = 0;
        self.reg_status.vblank = false;
        return old;
    }

    pub fn writeRegMask(self: *Self, byte: u8) void {
        self.reg_mask = @as(RegMask, @bitCast(byte));
    }

    pub fn writeRegCtrl(self: *Self, byte: u8) void {
        const old = self.reg_ctrl;
        self.reg_ctrl = @as(RegCtrl, @bitCast(byte));
        self.vram_register.tmp.h_nametable = @as(u1, @truncate(byte));
        self.vram_register.tmp.v_nametable = @as(u1, @truncate(byte >> 1));
        if (!old.generate_nmi and self.reg_ctrl.generate_nmi and self.reg_status.vblank)
            self.cpu.nmi = true;
    }

    pub fn writeOamAddr(self: *Self, byte: u8) void {
        self.oam_addr = byte;
    }

    pub fn writeOam(self: *Self, byte: u8) void {
        self.oam[self.oam_addr] = byte;
        self.oam_addr +%= 1;
    }

    pub fn writeVramAddr(self: *Self, byte: u8) void {
        self.vram_register.write(.addr, byte);
    }

    pub fn writeRegScroll(self: *Self, byte: u8) void {
        self.vram_register.write(.scroll, byte);
    }

    fn spriteSize(self: *const Self) u8 {
        return switch (self.reg_ctrl.sprite_size) {
            .small => 8,
            .large => 16,
        };
    }

    const palette_mirror_pairs = [_][2]u16{
        .{ 0x3F00, 0x3F10 },
        .{ 0x3F04, 0x3F14 },
        .{ 0x3F08, 0x3F18 },
        .{ 0x3F0C, 0x3F1C },
    };

    pub fn writeVram(self: *Self, byte: u8) void {
        defer self.incrementAddr();

        const vram_addr = self.vram_register.addr();

        // Palette mirroring
        if (vram_addr >= 0x3F00 and vram_addr <= 0x3F1C) {
            for (palette_mirror_pairs) |pair| {
                if (vram_addr == pair[0] or vram_addr == pair[1]) {
                    self.vram[pair[0]] = byte;
                    self.vram[pair[1]] = byte;
                    return;
                }
            }
        }

        // Nametable mirroring
        if (vram_addr >= 0x2000 and vram_addr < 0x3000) {
            switch (self.mirroring) {
                .horizontal => {
                    if (vram_addr >= 0x2000 and vram_addr < 0x2400) {
                        self.vram[vram_addr + 0x400] = byte;
                    } else if (vram_addr >= 0x2400 and vram_addr < 0x2800) {
                        self.vram[vram_addr - 0x400] = byte;
                    } else if (vram_addr >= 0x2800 and vram_addr < 0x2C00) {
                        self.vram[vram_addr + 0x400] = byte;
                    } else if (vram_addr >= 0x2C00 and vram_addr < 0x3000) {
                        self.vram[vram_addr - 0x400] = byte;
                    }
                },
                .vertical => {
                    if (vram_addr >= 0x2000 and vram_addr < 0x2400) {
                        self.vram[vram_addr + 0x800] = byte;
                    } else if (vram_addr >= 0x2400 and vram_addr < 0x2800) {
                        self.vram[vram_addr + 0x800] = byte;
                    } else if (vram_addr >= 0x2800 and vram_addr < 0x2C00) {
                        self.vram[vram_addr - 0x800] = byte;
                    } else if (vram_addr >= 0x2C00 and vram_addr < 0x3000) {
                        self.vram[vram_addr - 0x800] = byte;
                    }
                },
            }
        }

        // Normal write
        self.vram[vram_addr] = byte;
    }

    pub fn readVram(self: *Self) u8 {
        // This is supposed to be "bad" if it happens, but the games that trigger it seem to work fine ¯\_(ツ)_/¯
        // const ok = self.reg_status.vblank or !self.reg_mask.show_background or !self.reg_mask.show_sprites;
        // if (!ok) @panic("2007 read during rendering!");

        const old = self.data_buffer;
        const addr = self.vram_register.addr();
        self.data_buffer = self.vram[addr];
        self.incrementAddr();
        // Unbuffered palette reads: https://www.nesdev.org/wiki/PPU_registers#The_PPUDATA_read_buffer_(post-fetch)
        return if (addr >= 0x3F00 and addr <= 0x3FFF) self.data_buffer else old;
    }

    fn incrementAddr(self: *Self) void {
        const increment: u8 = switch (self.reg_ctrl.vram_address_increment) {
            0 => 1,
            1 => 32,
        };
        self.vram_register.reg = @as(AddressRegister, @bitCast(@as(u15, @bitCast(self.vram_register.reg)) + increment));
    }

    pub fn mapRom(self: *Self, the_mapper: *const Mapper) void {
        if (the_mapper.id == .nrom) {
            @memcpy(self.vram[0..the_mapper.chrRom0().len], the_mapper.chrRom0());
            self.mirroring = the_mapper.header().flags6.mirroring;
        }
    }

    pub fn getNametable(self: *Self, index: u16) []const u8 {
        const addr = 0x2000 + (index - 1) * 0x400;
        return self.vram[addr..][0..0x400];
    }

    // 1-indexed!
    pub fn getAttributeTable(self: *Self, index: u16) *AttributeTable {
        std.debug.assert(index > 0 and index < 5);
        const addr = 0x2000 + index * 0x400 - 64;
        return @as(*AttributeTable, @ptrCast(self.vram[addr..][0..64]));
    }

    pub fn getOam(self: *Self) *Oam {
        return @ptrCast(@alignCast(&self.oam));
    }

    pub fn getPalette(self: *Self, index: u8) *[4]u8 {
        const palette = self.vram[0x3F00..][0..16];
        return switch (index) {
            0 => palette[0..4],
            1 => palette[4..8],
            2 => palette[8..12],
            3 => palette[12..16],
            else => unreachable,
        };
    }

    pub fn getSpritePalette(self: *Self, index: u8) *[4]u8 {
        const palette = self.vram[0x3F00..][16..32];
        return switch (index) {
            0 => palette[0..4],
            1 => palette[4..8],
            2 => palette[8..12],
            3 => palette[12..16],
            else => unreachable,
        };
    }

    const TileCache = struct {
        row: TileRow,
        bg_palette: ?*[4]u8, // null = cache is invalidated, needs refresh
    };

    var tile_cache: TileCache = .{
        .row = .{ .row = 0 },
        .bg_palette = null,
    };

    fn fetchPixel(self: *Self) u8 {
        const prev_cycle = self.prevCycle();
        const row: u16 = self.vram_register.reg.y_fine;
        const col = prev_cycle + self.vram_register.x_fine;

        // Load new tile
        if (col % 8 == 0 or tile_cache.bg_palette == null) {
            const nt_addr = self.vram_register.getTileAddress();
            const nt: u16 = self.vram[nt_addr];
            const pt = self.getBackgroundPatternTable();

            const attr_addr = self.vram_register.getAttributeAddress();
            const attr = self.vram[attr_addr];
            if (self.logging)
                std.log.info("scanline {d} cycle {d} -> nt addr {X:0>4} attr addr {X:0>4} nt {X:0>4} attr {X:0>4} x fine {d}", .{ self.scanline, self.cycle, nt_addr, attr_addr, nt, attr, self.vram_register.x_fine });
            const bg_palette_index = AttributeTable.shiftAttr(attr, self.vram_register.reg.y_coarse, self.vram_register.reg.x_coarse);
            const bg_palette = self.getPalette(bg_palette_index);
            tile_cache.row = pt.getTileRow(nt, row);
            tile_cache.bg_palette = bg_palette;
        }

        const bg_color_index = tile_cache.row.getCol(col % 8);
        const bg_opaque = bg_color_index != 0;
        const bg_pixel = if (bg_opaque) tile_cache.bg_palette.?[bg_color_index] else self.vram[0x3f00];

        const draw_background = self.reg_mask.show_background and bg_opaque;
        const bg = if (draw_background)
            bg_pixel
        else
            self.vram[0x3F00];

        if (!self.reg_mask.show_sprites)
            return bg;

        const active_sprites = oam_cache.active_sprites[self.scanline];
        if (active_sprites.len == 0)
            return bg;

        const oam = self.getOam();
        for (active_sprites.constSlice()) |sprite_hit| {
            const i = sprite_hit.index;
            const sprite = oam.get(i);
            const x = sprite.x;
            const in_bounds = prev_cycle >= x and prev_cycle < x + 8;
            if (!in_bounds) continue;
            const y = sprite.y +% 1;

            const attrs = sprite.attrs;
            const spt = switch (self.reg_ctrl.sprite_size) {
                .small => self.getSpritePatternTable(),
                .large => switch (sprite.tile_index & 1) {
                    0 => self.patternTableLeft(),
                    1 => self.patternTableRight(),
                    else => unreachable,
                },
            };
            const srow = if (attrs.flip_vertical) (self.spriteSize() - 1 - (self.scanline - y)) else (self.scanline - y);
            const sprite_palette = self.getSpritePalette(attrs.palette);
            const xpos = prev_cycle - x;
            const srow_index = if (attrs.flip_horizontal)
                7 - xpos
            else
                xpos;
            var upper = sprite_hit.upper;
            if (self.reg_ctrl.sprite_size == .large and attrs.flip_vertical)
                upper = !upper;
            const tile_index = sprite.tile_index + @as(u8, if (upper) 0 else 1);
            const sprite_color_index = spt.getTileRow(tile_index, srow % 8).getCol(srow_index);
            const sprite_pixel = sprite_palette[sprite_color_index];
            const sprite_opaque = sprite_color_index != 0;

            // Sprite 0 hit
            // https://www.nesdev.org/wiki/PPU_OAM#Sprite_0_hits
            if (i == 0 and oam_cache.sprite_zero_hit and
                self.reg_mask.show_background and bg_opaque and
                sprite_opaque and
                x != 255 and !((self.reg_mask.show_background_left or self.reg_mask.show_sprite_left) and x > 0 and x < 8))
            {
                self.reg_status.sprite_zero_hit = true;
            }

            const draw_sprite = ((sprite_opaque and !bg_opaque) or (sprite_opaque and bg_opaque and attrs.priority == 0));

            if (draw_sprite)
                return sprite_pixel;
        }

        return bg;
    }

    const SpriteHit = packed struct {
        index: u7,
        // Upper or lower half of 8x16 sprite
        upper: bool,
    };

    const OamCache = struct {
        active_sprites: [240]std.BoundedArray(SpriteHit, 8) = undefined,
        sprite_zero_hit: bool = false,

        const C = @This();

        fn invalidate(self: *C) void {
            self.sprite_zero_hit = false;
            for (&self.active_sprites) |*array| {
                array.len = 0;
            }
        }
    };

    var oam_cache: OamCache = .{};

    fn prepareOam(self: *Self) void {
        oam_cache.invalidate();
        const oam = self.getOam();
        var i: u8 = 0;
        while (i < Oam.len) : (i += 1) {
            const sprite = oam.get(i);
            const y = sprite.y +% 1;
            if (y > 1 and y < 239) {
                const x: u16 = sprite.x;
                if (x >= 0 and x < 248) {
                    var j: usize = 0;
                    while (j < self.spriteSize()) : (j += 1) {
                        const yy = y + j;
                        if (yy >= 240) break;
                        oam_cache.active_sprites[yy].append(.{ .index = @as(u7, @intCast(i)), .upper = j < 8 }) catch break;
                    }
                    if (i == 0) {
                        oam_cache.sprite_zero_hit = true;
                    }
                }
            }
        }
    }

    pub fn step(self: *Self) void {
        switch (self.scanline) {
            0...239 => {
                switch (self.cycle) {
                    0 => self.prepareOam(),
                    1...256 => {
                        self.pixels[self.cycle - 1] = self.fetchPixel();
                        if (self.reg_mask.show_background and ((self.cycle + self.vram_register.x_fine) % 8 == 0)) {
                            self.vram_register.incrementX();
                        }
                    },
                    257 => {
                        tile_cache.bg_palette = null;
                        if (self.reg_mask.show_background)
                            self.vram_register.incrementY();
                    },
                    258 => if (self.reg_mask.show_background) {
                        self.vram_register.reg.x_coarse = self.vram_register.tmp.x_coarse;
                        self.vram_register.reg.h_nametable = self.vram_register.tmp.h_nametable;
                    },
                    259...340 => {},
                    else => unreachable,
                }
            },
            240 => if (self.cycle == 0) {
                self.reg_status.vblank = true;
                if (self.reg_ctrl.generate_nmi) {
                    self.cpu.nmi = true;
                }
            },
            260 => switch (self.cycle) {
                0 => self.reg_status = @as(RegStatus, @bitCast(@as(u8, 0))),
                280 => if (self.reg_mask.show_background or self.reg_mask.show_sprites) {
                    self.vram_register.reg.y_coarse = self.vram_register.tmp.y_coarse;
                    self.vram_register.reg.y_fine = self.vram_register.tmp.y_fine;
                    self.vram_register.reg.v_nametable = self.vram_register.tmp.v_nametable;
                },
                else => {},
            },
            else => {},
        }

        self.cycle += 1;
        self.total_cycle += 1;

        if (self.cycle >= 341) {
            if (self.scanline < 240)
                self.frame[self.scanline] = self.pixels;
            self.cycle = 0;
            self.scanline += 1;
            if (self.scanline >= 262) {
                self.scanline = 0;
            }
        }
    }
};
