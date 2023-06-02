const std = @import("std");

const Config = @This();

roms_path: []const u8 = undefined,
paused: bool = false,
cpu_log: bool = false,
audio: bool = false,

pub fn parse(allocator: std.mem.Allocator, config_file: []const u8) !Config {
    var config = Config{};
    var lines = std.mem.split(u8, config_file, "\n");
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();

    while (lines.next()) |line| {
        if (line.len == 0 or line[0] == ';') continue;

        var tokens = std.mem.split(u8, line, " = ");

        const key = tokens.next().?;
        if (seen.contains(key)) {
            return error.DuplicateKey;
        }
        try seen.put(key, {});

        const value = tokens.next().?;
        if (std.mem.eql(u8, key, "roms_path")) {
            config.roms_path = value;
        } else if (std.mem.eql(u8, key, "paused")) {
            config.paused = try parseBool(value);
        } else if (std.mem.eql(u8, key, "cpu_log")) {
            config.cpu_log = try parseBool(value);
        } else if (std.mem.eql(u8, key, "audio")) {
            config.audio = try parseBool(value);
        } else {
            std.log.warn("Invalid config key '{s}', line {s}", .{key, line});
            return error.InvalidKey;
        }
    }

    return config;
}

fn parseBool(str: []const u8) !bool {
    return if (std.mem.eql(u8, str, "true"))
        true
    else if (std.mem.eql(u8, str, "false"))
        false
    else
        error.InvalidBool;
}
