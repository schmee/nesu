const std = @import("std");
const rl = @import("raylib");
const Allocator = std.mem.Allocator;

const Apu = @import("apu.zig").Apu;
const _cpu = @import("cpu.zig");
const Cpu = @import("cpu.zig").Cpu;
const _ppu = @import("ppu.zig");
const Ppu = @import("ppu.zig").Ppu;
const _config = @import("config.zig");
const mapper = @import("mapper.zig");
const Resampler = @import("Resampler.zig");
const hex = std.fmt.fmtSliceHexUpper;

pub const log_level: std.log.Level = .info;

const Reloader = struct {
    target: usize = 40,
    counter: usize = 0,

    const Self = @This();

    const State = enum {
        idle,
        ticking,
        triggered,
        locked,
    };

    fn tick(self: *Self) void {
        if (self.counter <= self.target + 1)
            self.counter += 1;
    }

    fn reset(self: *Self) void {
        self.counter = 0;
    }

    fn completion(self: *Self) f32 {
        return @intToFloat(f32, self.counter) / @intToFloat(f32, self.target);
    }

    fn state(self: *Self) State {
        return if (self.counter == 0)
            .idle
        else if (self.counter <= self.target)
            .ticking
        else if (self.counter == self.target + 1)
            .triggered
        else .locked;
    }
};

pub fn main() !void {
    var allocator = std.heap.c_allocator;

    const config_file = try std.fs.cwd().readFileAlloc(allocator, "config.ini", 100_000);
    const config = try _config.parse(allocator, config_file);

    const screen_width = 256;
    const screen_height = 240;
    rl.SetTargetFPS(60);
    rl.SetWindowState(.FLAG_VSYNC_HINT);
    rl.InitWindow(screen_width * 2, screen_height * 2, "Nesu");
    var framebuffer: [screen_height][screen_width]rl.Color = undefined;
    const image = rl.Image{
        .data = &framebuffer,
        .width = screen_width,
        .height = screen_height,
        .mipmaps = 1,
        .format = rl.PIXELFORMAT_UNCOMPRESSED_R8G8B8A8,
    };
    const texture = rl.LoadTextureFromImage(image);
    defer rl.UnloadTexture(texture);

    var ui_state: UiState = .game_list;

    resampler = try Resampler.init(allocator, sample_rate);
    var audio_stream: rl.AudioStream = undefined;
    if (config.audio) {
        rl.InitAudioDevice();
        rl.SetAudioStreamBufferSizeDefault(4096);
        audio_stream = rl.LoadAudioStream(sample_rate, 16, 1);
        rl.SetAudioStreamCallback(audio_stream, &audioInputCallback);
        rl.PlayAudioStream(audio_stream);
        rl.SetAudioStreamVolume(audio_stream, 0.2);
    }

    var loaded_rom: ?[]const u8 = null;
    var cpu_ram = std.mem.zeroes([_cpu.ram_size]u8);
    var cpu = Cpu{ .ram = &cpu_ram, .log_instructions = false, .ppu = undefined, .apu = undefined, .mapper = undefined };
    var ppu = Ppu{ .cpu = undefined };
    var apu = Apu{ .resampler = &resampler };

    var old_scanline: usize = 123;
    var paused = config.paused;
    var draw_fps_counter = false;
    var step = false;

    var reloader = Reloader{};
    var game_list = try GameList.init(allocator, config.roms_path);
    defer game_list.deinit();

    while (!rl.WindowShouldClose()) {
        var input = Input{};
        if (ui_state == .game_list) {
            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_TAB)) {
                ui_state = .play;
                game_list.reset();
            }

            if (rl.IsKeyPressed(.KEY_BACKSPACE)) {
                game_list.searchDelete();
            }

            var c: u8 = @intCast(u8, rl.GetCharPressed());
            while (c != 0) : (c = @intCast(u8, rl.GetCharPressed())) {
                try game_list.searchAdd(std.ascii.toLower(c));
            }

            if (rl.IsKeyPressed(.KEY_DOWN) or (rl.IsKeyDown(.KEY_LEFT_CONTROL) and rl.IsKeyPressed(.KEY_J))) {
                game_list.next();
            }

            if (rl.IsKeyPressed(.KEY_UP) or (rl.IsKeyDown(.KEY_LEFT_CONTROL) and rl.IsKeyPressed(.KEY_K))) {
                game_list.prev();
            }

            if (rl.IsKeyPressed(.KEY_ENTER)) {
                loaded_rom = try game_list.getSelected();
                try loadRom(allocator, &ui_state, loaded_rom.?, &cpu, &cpu_ram, &ppu, &apu);
                game_list.reset();
            }
        } else {
            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_P)) {
                paused = !paused;
            }

            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_EIGHT)) {
                // std.log.info("PRESSED 'L'", .{});
                cpu.log_instructions = !cpu.log_instructions;
            }

            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_G)) {
                ppu.logging = !ppu.logging;
            }

            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_S)) {
                step = !step;
            }

            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_N)) {
                switch (ui_state) {
                    .nametable => |*i| {
                        i.* += 1;
                        if (i.* > 4) ui_state = .play;
                    },
                    else => ui_state = UiState{ .nametable = 1 },
                }
            }

            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_O)) {
                ui_state = if (ui_state == .oam) .play else .oam;
            }

            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_C)) {
                draw_fps_counter = !draw_fps_counter;
            }

            if (rl.IsKeyDown(rl.KeyboardKey.KEY_LEFT)) {
                input.left = true;
            }

            if (rl.IsKeyDown(rl.KeyboardKey.KEY_RIGHT)) {
                input.right = true;
            }

            if (rl.IsKeyDown(rl.KeyboardKey.KEY_UP)) {
                input.up = true;
            }

            if (rl.IsKeyDown(rl.KeyboardKey.KEY_DOWN)) {
                input.down = true;
            }

            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_ENTER)) {
                input.start = true;
            }

            if (rl.IsKeyDown(rl.KeyboardKey.KEY_V)) {
                input.select = true;
            }

            if (rl.IsKeyDown(rl.KeyboardKey.KEY_F)) {
                input.a = true;
            }

            if (rl.IsKeyDown(rl.KeyboardKey.KEY_D)) {
                input.b = true;
            }

            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_X)) {
                apu.resampler.mutex.lock();
                apu.resampler.do_filter = !apu.resampler.do_filter;
                apu.resampler.mutex.unlock();
            }

            if (rl.IsKeyPressed(rl.KeyboardKey.KEY_L)) {
                ui_state = .game_list;
                std.log.info("state {}", .{ui_state});
            }

            if (rl.IsKeyDown(.KEY_R)) {
                reloader.tick();
            } else {
                reloader.reset();
            }
        }

        var read = false;
        // const emu_start = try std.time.Instant.now();
        if (loaded_rom != null and ui_state != .unsupported_mapper and (step or !paused)) {
            while (true) {
                old_scanline = ppu.scanline;

                if (cpu.read_input) {
                    read = true;
                    cpu.writeInput(@bitCast(u8, input));
                } else if (read and !cpu.read_input) {
                    read = false;
                }

                const cycles = cpu.step();

                var apu_cycle_target = cpu.cycle;
                var i: usize = apu.total_cycles;
                while (i < apu_cycle_target) : (i += 1) {
                    apu.step();
                }
                resampler.activate();

                var ppu_cycles: usize = 0;
                while (ppu_cycles < cycles) : (ppu_cycles += 1) {
                    ppu.step();
                    ppu.step();
                    ppu.step();
                }

                if (ppu.scanline == 0 and old_scanline != 0) {
                    break;
                }
            }
        }

        // const emu_end = try std.time.Instant.now();

        // const frame_start = try std.time.Instant.now();
        switch (ui_state) {
            .nametable => |i| renderNametable(&ppu, &framebuffer, i),
            .oam => {
                for (framebuffer) |*row| {
                    std.mem.set(rl.Color, row, rl.BLACK);
                }
                renderOam(&ppu, &framebuffer);
            },
            .unsupported_mapper => {},
            else => for (ppu.frame) |*scanline, i| {
                for (scanline) |*pixel, j| {
                    framebuffer[i][j] = colors[pixel.*];
                }
            }
        }
        // const frame_end = try std.time.Instant.now();

        // std.log.info("emu {} frame {} | c {d} p {d} a {d}", .{std.fmt.fmtDuration(emu_end.since(emu_start)), std.fmt.fmtDuration(frame_end.since(frame_start)), cpu.cycle, ppu.cycle, apu.total_cycles});
        step = false;

        var buf = std.mem.zeroes([30]u8);
        const fps_str = if (draw_fps_counter) try std.fmt.bufPrintZ(&buf, "FPS: {d}", .{rl.GetFPS()}) else undefined;

        rl.BeginDrawing();

        const scale: f32 = if (ui_state == .oam) 5 else 2;
        rl.UpdateTexture(texture, &framebuffer);
        rl.DrawTextureEx(texture, rl.Vector2{ .x = 0, .y = 0}, 0, scale, rl.WHITE);

        switch (ui_state) {
            .game_list => {
                rl.DrawRectangle(0, 0, 2 * screen_width, 2 * screen_height, rl.Color{ .r = 0, .g = 0, .b = 0, .a = 215});
                rl.DrawText("LOAD GAME", 20, 20, 36, rl.WHITE);
                rl.DrawText("Type to filter", 250, 15, 20, rl.WHITE);
                rl.DrawText("ENTER selects", 250, 45, 20, rl.WHITE);
                var subbuf = std.mem.zeroes([50]u8);
                const sub = try std.fmt.bufPrintZ(&subbuf, "{s}_", .{game_list.search_string.items});
                rl.DrawText(sub.ptr, 20, 50, 36, rl.WHITE);
                for (game_list.filtered.items) |name, i| {
                    const color = if (i == game_list.selected) rl.RED else rl.WHITE;
                    rl.DrawText(name.ptr, 20, 30 * @intCast(c_int, i) + 100, 32, color);
                }
            },
            .nametable => {
                var tbuf = std.mem.zeroes([100]u8);
                const out = try std.fmt.bufPrintZ(&tbuf, "NT INDEX {d}", .{ui_state.nametable});
                rl.DrawText(out.ptr, 100, 10, 20, rl.RED);
            },
            .unsupported_mapper => {
                rl.DrawRectangle(0, 0, 2 * screen_width, 2 * screen_height, rl.Color{ .r = 0, .g = 0, .b = 0, .a = 215});

                const font_size = 22;
                var tbuf = std.mem.zeroes([100]u8);

                const str1 = try std.fmt.bufPrintZ(&tbuf, "Could not load ROM '{s}'", .{loaded_rom.?});
                const len1 = rl.MeasureText(str1.ptr, font_size);
                rl.DrawText(str1.ptr, screen_width - @divFloor(len1, 2), screen_height - 12, font_size, rl.WHITE);

                const str2 = try std.fmt.bufPrintZ(&tbuf, "Unsupported mapper '{d}'", .{ui_state.unsupported_mapper});
                const len2 = rl.MeasureText(str2.ptr, font_size);
                rl.DrawText(str2.ptr, screen_width - @divFloor(len2, 2), screen_height + 12, font_size, rl.WHITE);
            },
            else => {},
        }

        if (draw_fps_counter) {
            rl.DrawRectangle(screen_width * 2 - 110, screen_height * 2 - 40, 100, 100, rl.Color{ .r = 0, .g = 0, .b = 0, .a = 150});
            rl.DrawText(fps_str.ptr, screen_width * 2 - 100, screen_height * 2 - 30, 20, rl.WHITE);
        }

        switch (reloader.state()) {
            .ticking => {
                rl.DrawText("R", 37, screen_height * 2 - 65, 46, rl.WHITE);
                const center = rl.Vector2{ .x = 50, .y = 2 * screen_height - 45};
                rl.DrawRing(center, 30, 40, 180, 180 - 360 * reloader.completion(), 50, rl.WHITE);
            },
            .triggered => {
                try loadRom(allocator, &ui_state, loaded_rom.?, &cpu, &cpu_ram, &ppu, &apu);
            },
            else => {},
        }

        rl.EndDrawing();
    }

    rl.CloseWindow();
}

const sample_rate: f32 = 48000.0;
var resampler: Resampler = undefined;
var n_callbacks: usize = 0;

fn audioInputCallback(ptr: *anyopaque, frames: i32) void {
    const buffer = @ptrCast([*]i16, @alignCast(2, ptr));
    // const start = std.time.Instant.now() catch unreachable;
    // std.log.info("{d} DRAIN -> START", .{n_callbacks});
    resampler.drain(buffer[0..@intCast(usize, frames)]);
    // const end = std.time.Instant.now() catch unreachable;
    // std.log.info("DRAIN -> DONE  | time {} frames {d}", .{std.fmt.fmtDuration(end.since(start)), frames});
    n_callbacks += 1;
}

const Input = packed struct {
    a: bool = false,
    b: bool = false,
    select: bool = false,
    start: bool = false,
    up: bool = false,
    down: bool = false,
    left: bool = false,
    right: bool = false
};

fn loadRom(allocator: Allocator, ui_state: *UiState, filename: []const u8, cpu: *Cpu, cpu_ram: *[_cpu.ram_size]u8, ppu: *Ppu, apu: *Apu) !void {
    const rom_data = try std.fs.cwd().readFileAlloc(allocator, filename, 500_000);
    var the_mapper = try allocator.create(mapper.Mapper);
    the_mapper.* = mapper.load(rom_data) catch |err| switch (err) {
        error.UnsupportedMapper => {
            const header = mapper.parseHeader(rom_data);
            ui_state.* = . { .unsupported_mapper = mapper.mapperNumber(header) };
            return;
        },
        else => |e| return e,
    };
    ui_state.* = .play;

    resampler = try Resampler.init(allocator, sample_rate);
    cpu.* = Cpu{ .ram = cpu_ram, .log_instructions = false, .ppu = undefined, .apu = undefined, .mapper = the_mapper };
    ppu.* = Ppu{ .cpu = undefined };
    apu.* = Apu{ .resampler = &resampler };
    cpu.apu = apu;
    cpu.ppu = ppu;
    ppu.cpu = cpu;

    ppu.mapRom(the_mapper);

    // Set the RAM to the same pattern as fceux
    for (cpu.ram[0x0000 .. 0x1FFF]) |*byte, i| {
        byte.* = switch (i % 8) {
            0...3 => 0x00,
            4...7 => 0xFF,
            else => unreachable
        };
    }
    cpu.pc = cpu.absoluteIndexed(0xFFFC);
}

const UiState = union(enum) {
    play: void,
    game_list: void,
    nametable: u8,
    oam: void,
    unsupported_mapper: u8,
};

const GameList = struct {
    allocator: Allocator,
    roms_path: []const u8,
    all: std.ArrayListUnmanaged([]const u8) = .{},
    filtered: std.ArrayListUnmanaged([]const u8) = .{},
    search_string: std.ArrayListUnmanaged(u8) = .{},
    selected: usize = 0,

    const Self = @This();

    fn init(allocator: Allocator, roms_path: []const u8) !Self {
        var game_list = GameList{
            .allocator = allocator,
            .roms_path = try allocator.dupeZ(u8, roms_path),
        };
        var dir = try std.fs.cwd().openIterableDir(roms_path, .{});
        defer dir.close();
        var it = dir.iterate();
        while (try it.next()) |e| {
            if (std.ascii.endsWithIgnoreCase(e.name, ".nes"))
                try game_list.all.append(allocator, try allocator.dupeZ(u8, e.name));
        }
        std.sort.sort([]const u8, game_list.all.items, {}, Self.orderStrings);
        try game_list.filtered.appendSlice(allocator, game_list.all.items);

        return game_list;
    }

    fn deinit(self: *Self) void {
        for (self.all.items) |name| {
            self.allocator.free(name);
        }
        self.all.deinit(self.allocator);
        self.filtered.deinit(self.allocator);
        self.search_string.deinit(self.allocator);
    }

    fn getSelected(self: *Self) ![]const u8 {
        return std.fmt.allocPrint(self.allocator, "{s}/{s}", .{self.roms_path, self.filtered.items[self.selected]});
    }

    fn refresh(self: *Self) !void {
        self.filtered.clearRetainingCapacity();
        if (self.search_string.items.len > 1) {
            for (self.all.items) |name| {
                if (std.ascii.indexOfIgnoreCase(name, self.search_string.items)) |_| {
                    try self.filtered.append(self.allocator, name);
                }
            }
        } else {
            self.filtered.appendSliceAssumeCapacity(self.all.items);
        }
        self.selected = @min(self.selected, self.filtered.items.len);
    }

    fn searchAdd(self: *Self, c: u8) !void {
        try self.search_string.append(self.allocator, c);
        try self.refresh();
    }

    fn searchDelete(self: *Self) void {
        _ = self.search_string.popOrNull();
        self.refresh() catch unreachable;
    }

    fn prev(self: *Self) void {
        self.selected -|= 1;
    }

    fn next(self: *Self) void {
        self.selected = @min(self.selected + 1, self.filtered.items.len - 1);
    }

    fn reset(self: *Self) void {
        self.search_string.clearRetainingCapacity();
        self.refresh() catch unreachable;
    }

    fn orderStrings(_: void, a: []const u8, b: []const u8) bool {
        return std.ascii.lessThanIgnoreCase(a, b);
    }
};

const colors = [64]rl.Color{
    .{ .r = 124, .g = 124, .b = 124, .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 252, .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 188, .a = 255 },
    .{ .r = 68,  .g = 40,  .b = 188, .a = 255 },
    .{ .r = 148, .g = 0,   .b = 132, .a = 255 },
    .{ .r = 168, .g = 0,   .b = 32,  .a = 255 },
    .{ .r = 168, .g = 16,  .b = 0,   .a = 255 },
    .{ .r = 136, .g = 20,  .b = 0,   .a = 255 },
    .{ .r = 80,  .g = 48,  .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 120, .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 104, .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 88,  .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 64,  .b = 88,  .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
    .{ .r = 188, .g = 188, .b = 188, .a = 255 },
    .{ .r = 0,   .g = 120, .b = 248, .a = 255 },
    .{ .r = 0,   .g = 88,  .b = 248, .a = 255 },
    .{ .r = 104, .g = 68,  .b = 252, .a = 255 },
    .{ .r = 216, .g = 0,   .b = 204, .a = 255 },
    .{ .r = 228, .g = 0,   .b = 88,  .a = 255 },
    .{ .r = 248, .g = 56,  .b = 0,   .a = 255 },
    .{ .r = 228, .g = 92,  .b = 16,  .a = 255 },
    .{ .r = 172, .g = 124, .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 184, .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 168, .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 168, .b = 68,  .a = 255 },
    .{ .r = 0,   .g = 136, .b = 136, .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
    .{ .r = 248, .g = 248, .b = 248, .a = 255 },
    .{ .r = 60,  .g = 188, .b = 252, .a = 255 },
    .{ .r = 104, .g = 136, .b = 252, .a = 255 },
    .{ .r = 152, .g = 120, .b = 248, .a = 255 },
    .{ .r = 248, .g = 120, .b = 248, .a = 255 },
    .{ .r = 248, .g = 88,  .b = 152, .a = 255 },
    .{ .r = 248, .g = 120, .b = 88,  .a = 255 },
    .{ .r = 252, .g = 160, .b = 68,  .a = 255 },
    .{ .r = 248, .g = 184, .b = 0,   .a = 255 },
    .{ .r = 184, .g = 248, .b = 24,  .a = 255 },
    .{ .r = 88,  .g = 216, .b = 84,  .a = 255 },
    .{ .r = 88,  .g = 248, .b = 152, .a = 255 },
    .{ .r = 0,   .g = 232, .b = 216, .a = 255 },
    .{ .r = 120, .g = 120, .b = 120, .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
    .{ .r = 252, .g = 252, .b = 252, .a = 255 },
    .{ .r = 164, .g = 228, .b = 252, .a = 255 },
    .{ .r = 184, .g = 184, .b = 248, .a = 255 },
    .{ .r = 216, .g = 184, .b = 248, .a = 255 },
    .{ .r = 248, .g = 184, .b = 248, .a = 255 },
    .{ .r = 248, .g = 164, .b = 192, .a = 255 },
    .{ .r = 240, .g = 208, .b = 176, .a = 255 },
    .{ .r = 252, .g = 224, .b = 168, .a = 255 },
    .{ .r = 248, .g = 216, .b = 120, .a = 255 },
    .{ .r = 216, .g = 248, .b = 120, .a = 255 },
    .{ .r = 184, .g = 248, .b = 184, .a = 255 },
    .{ .r = 184, .g = 248, .b = 216, .a = 255 },
    .{ .r = 0,   .g = 252, .b = 252, .a = 255 },
    .{ .r = 248, .g = 216, .b = 248, .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
    .{ .r = 0,   .g = 0,   .b = 0,   .a = 255 },
};

// For debug view rendering

fn renderSprite(ppu: *Ppu, buf: *[8][8]rl.Color, i: u8) void {
    const oam = ppu.getOam();
    const sprite = oam.get(i);

    const attrs = sprite.attrs;
    const spt = ppu.getSpritePatternTable();
    const sprite_palette = ppu.getSpritePalette(attrs.palette);

    var x: u16 = 0;
    while (x < 8) : (x += 1) {
        var y: u16 = 0;
        while (y < 8) : (y += 1) {
            const sprite_color_index = spt.getTileRow(sprite.tile_index, x).getCol(y);
            buf[x][y] = colors[sprite_palette[sprite_color_index]];
        }
    }
}

fn renderOam(ppu: *Ppu, buf: *[240][256]rl.Color) void {
    var sbuf = std.mem.zeroes([8][8]rl.Color);
    var row: usize = 2;
    var col: usize = 2;
    var col_base: usize = 2;
    var i: u8 = 0;
    while (i < _ppu.Oam.len) : (i += 1) {
        renderSprite(ppu, &sbuf, i);

        var x: u16 = 0;
        while (x < 8) : (x += 1) {
            var y: u16 = 0;
            while (y < 8) : (y += 1) {
                buf[row][col] = sbuf[x][y];
                col += 1;
            }
            col = col_base;
            row += 1;
        }
        col_base = ((i + 1) % 8) * 10 + 2;
        col = col_base;
        row = ((i + 1) / 8) * 10 + 2;
    }
}

fn renderNametable(ppu: *Ppu, buf: *[240][256]rl.Color, nametable_index: u16) void {
    const nametable = ppu.getNametable(nametable_index);
    const pattern_table = ppu.patternTableRight();
    const attribute_table = ppu.getAttributeTable(nametable_index);
    var row: u16 = 0;
    var col: u16 = 0;
    while (row < 240) : (row += 1) {
        while (col < 256) : (col += 8) {
            const tile_row = row / 8;
            const tile_col = col / 8;
            const nt_index = tile_row * 32 + tile_col;
            const tile_index = @as(u16, nametable[nt_index]);
            const palette_index = attribute_table.get(tile_row, tile_col);
            const palette = ppu.getPalette(palette_index);

            var i: u8 = 0;
            while (i < 8) : (i += 1) {
                const pixel = pattern_table.getTileRow(tile_index, row % 8).getCol(i);
                buf[row][col + i] = colors[palette[pixel]];
            }
        }
        col = 0;
    }
}
