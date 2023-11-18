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

pub const MapperId = enum {
    nrom,
    mmc1,
};

pub const Mapper = struct {
    id: MapperId,
    bytes: []const u8,

    // MMC1 specific
    mmc_reg: u8 = 0,
    mmc_n_writes: u8 = 0,
    chr_bank_0: u8 = 0,
    chr_bank_1: u8 = 0,
    prg_bank: u8 = 0,

    const Self = @This();


    pub fn prgRom0(self: *const Self) []const u8 {
        return switch (self.id) {
            .nrom => self.getBank(0),
            .mmc1 => self.getBank(self.prg_bank),
        };
    }

    pub fn prgRom1(self: *const Self) []const u8 {
        return switch (self.id) {
            .nrom => self.getBank(1),
            .mmc1 => self.getBank(7), // TODO: don't hardcode
        };
    }

    pub fn chrRom0(self: *const Self) []const u8 {
        const h = self.header();
        return switch (self.id) {
            .nrom => self.bytes[@sizeOf(Header) + @as(usize, h.prg_rom_size) * 0x4000..][0..@as(u16, h.chr_rom_size) * 0x2000],
            .mmc1 => self.getBank(self.chr_bank_0),
        };
    }

    pub fn chrRom1(self: *const Self) []const u8 {
        return switch (self.id) {
            .nrom => unreachable,
            .mmc1 => self.getBank(self.chr_bank_1),
        };
    }

    pub fn write(self: *Self, addr: u16, byte: u8) void {
        switch (self.id) {
            .nrom => unreachable,
            .mmc1 => {
                const reset = byte >> 7 == 1;
                if (reset) {
                    self.mmc_reg = 0;
                    self.mmc_n_writes = 0;
                } else {
                    self.mmc_reg |= ((byte & 1) << 4);
                    self.mmc_n_writes += 1;
                    if (self.mmc_n_writes == 5) {
                        const value = self.mmc_reg & 0b1_1111;
                        switch (addr) {
                            0xA000 => self.chr_bank_0 = value,
                            0xC000 => self.chr_bank_1 = value,
                            0xE000 => self.prg_bank = value & 0b1111,
                            0x8000 => {},
                            else => @panic("Unexpected write in mapper!"),
                        }
                        self.mmc_n_writes = 0;
                        self.mmc_reg = 0;
                    } else {
                        self.mmc_reg >>= 1;
                    }
                }
            }
        }
    }

    pub fn header(self: *const Self) Header {
        return parseHeader(self.bytes);
    }

    inline fn getBank(self: *const Self, index: u8) []const u8 {
        std.debug.assert(index < self.header().prg_rom_size);
        const start = @as(usize, index) * 0x4000;
        const end = @as(usize, index + 1) * 0x4000;
        return self.bytes[@sizeOf(Header)..][start..end];
    }
};

pub fn parseHeader(bytes: []const u8) Header {
    return @bitCast(Header, bytes[0..16].*);
}

pub fn mapperNumber(header_: Header) u8 {
    return @as(u8, header_.flags7.mapper_number_msn) << 4 ^ (@as(u8, header_.flags6.mapper_number_lsn));
}

pub fn load(rom_data: []const u8) !Mapper {
    return .{
        .id = switch (mapperNumber(parseHeader(rom_data))) {
            0 => .nrom,
            1 => .mmc1,
            else => return error.UnsupportedMapper,
        },
        .bytes = rom_data
    };
}
