const std = @import("std");

// https://www.nesdev.org/wiki/INES
//
// An iNES file consists of the following sections, in order:
//
// 1. Header (16 bytes)
// 2. Trainer, if present (0 or 512 bytes)
// 3. PRG ROM data (16384 * x bytes)
// 4. CHR ROM data, if present (8192 * y bytes)
// 5. PlayChoice INST-ROM, if present (0 or 8192 bytes)
// 6. PlayChoice PROM, if present (16 bytes Data, 16 bytes CounterOut) (this is often missing; see PC10 ROM-Images for details)
//
// Some ROM-Images additionally contain a 128-byte (or sometimes 127-byte) title at the end of the file.

pub const Mirroring = enum(u1) {
    horizontal,
    vertical
};

pub const Flag6 = packed struct {
    mirroring: Mirroring,
    flag1: bool,
    flag2: bool,
    flag3: bool,
    mapper_number_lsn: u4,
};

pub const Flag7 = packed struct {
    unused: u4,
    mapper_number_msn: u4,
};

pub const Header = extern struct {
    constant: [4]u8,
    // in 16kb chunks
    prg_rom_size: u8,
    // in 8kb chunks
    chr_rom_size: u8,
    flags6: Flag6,
    flags7: Flag7,
    flags8: u8,
    flags9: u8,
    flags10: u8,
    _padding1: u16,
    _padding2: u8,
};

pub const INES = struct {
    bytes: []const u8,

    const Self = @This();

    pub fn header(self: *const Self) Header {
        return @bitCast(Header, self.bytes[0..16].*);
    }

    pub fn prgRom(self: *const Self) []const u8 {
        const h = self.header();
        return self.bytes[@sizeOf(Header)..][0..@as(usize, h.prg_rom_size) * 16384];
    }

    pub fn chrRom(self: *const Self) []const u8 {
        const h = self.header();
        return self.bytes[@sizeOf(Header) + @as(usize, h.prg_rom_size) * 16384..][0..@as(u16, h.chr_rom_size) * 8192];
    }

    pub fn mapperNumber(self: *const Self) u8 {
        return @as(u8, self.header().flags7.mapper_number_msn) << 4 ^ (@as(u8, self.header().flags6.mapper_number_lsn));
    }
};

pub fn load(rom_data: []const u8) !INES {
    var ines = INES{ .bytes = undefined };
    ines.bytes = rom_data;
    return ines;
}
