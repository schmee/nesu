const std = @import("std");

const Cursor = struct {
    i: usize = 0,
    gen: usize = 0,
    len: usize,

    const Self = @This();

    pub fn dist(self: Self, other: Self) isize {
        std.debug.assert(self.len == other.len);
        const a = self.i + self.gen * self.len;
        const b = other.i + other.gen * other.len;
        return @as(isize, @intCast(b)) - @as(isize, @intCast(a));
    }

    pub fn inc(self: *Self, n: usize) void {
        self.i += n;
        if (self.i >= self.len) {
            self.i -= self.len;
            self.gen += 1;
        }
    }

    pub fn dec(self: *Self) void {
        if (self.i > 0) {
            self.i -= 1;
        }
        if (self.i == 0 and self.gen > 0) {
            self.i = self.len - 1;
            self.gen -= 1;
        }
    }

    pub fn compare(self: Self, other: Self) std.math.Order {
        return switch (std.math.order(self.gen, other.gen)) {
            .gt, .lt => |o| o,
            .eq => switch (std.math.order(self.i, other.i)) {
                .gt, .lt => |o| o,
                .eq => .eq,
            },
        };
    }
};

// floor(APU: 1789773hz / soundcard: 48000hz)
const decimation_factor = 37;

sample_rate: f32,
frame_size: usize,
active: bool = false,
buffer: []i16,
read_cursor: Cursor,
write_cursor: Cursor,

needs_more_data: bool = false,
needed: Cursor,

mutex: std.Thread.Mutex = .{},
condition: std.Thread.Condition = .{},

read_lags: [100]isize = std.mem.zeroes([100]isize),
read_lags_cursor: Cursor = .{ .len = 100 },
wait_counter: usize = 0,

do_filter: bool = true,

resampler_buf: [filter.len]f32 = std.mem.zeroes([filter.len]f32),
resampler_cursor: Cursor = .{ .len = filter.len },
resampler_count: usize = 0,

resampled_cache: [8]i16 = std.mem.zeroes([8]i16),
resampled_cache_cursor: usize = 0,

const Resampler = @This();

pub fn init(allocator: std.mem.Allocator, sample_rate: f32) !Resampler {
    const frame_size = @as(usize, @intFromFloat(sample_rate / 100));
    std.debug.assert(frame_size == 480);
    const buf_len = 25 * frame_size;
    return .{
        .sample_rate = sample_rate,
        .frame_size = frame_size,
        .buffer = try allocator.alloc(i16, buf_len),
        .needed = .{ .len = buf_len },
        .read_cursor = .{ .len = buf_len },
        .write_cursor = .{ .len = buf_len },
    };
}

pub fn deinit(self: *Resampler, allocator: std.mem.Allocator) void {
    allocator.free(self.buffer);
}

pub fn activate(self: *Resampler) void {
    self.mutex.lock();
    defer self.mutex.unlock();
    if (self.active) return;

    if (self.write_cursor.i > 4 * self.frame_size)
        self.active = true;
}

pub fn push(self: *Resampler, s: f32) void {
    self.resampler_buf[self.resampler_cursor.i] = s;
    self.resampler_count += 1;

    if (self.resampler_count == decimation_factor) {
        const y = if (!self.do_filter)
            self.resampler_buf[self.resampler_cursor.i]
        else blk: {
            var y: f32 = 0;
            var cursor = self.resampler_cursor;
            for (filter) |b| {
                y += b * self.resampler_buf[cursor.i];
                if (cursor.i == 0 and cursor.gen == 0) break;
                cursor.dec();
            }
            break :blk y;
        };
        self.resampled_cache[self.resampled_cache_cursor] = @as(i16, @intFromFloat(blockDc(60000 * y - 30000) + 0.5));

        self.resampler_count = 0;
        self.resampled_cache_cursor += 1;
    }

    self.resampler_cursor.inc(1);

    if (self.resampled_cache_cursor == self.resampled_cache.len) {
        self.mutex.lock();
        defer self.mutex.unlock();

        for (self.resampled_cache) |cs| {
            self.buffer[self.write_cursor.i] = cs;
            self.write_cursor.inc(1);
        }

        if (self.needs_more_data) {
            if (self.write_cursor.compare(self.needed) == .gt) {
                self.needs_more_data = false;
                self.condition.signal();
            }
        }

        self.resampled_cache_cursor = 0;
    }
}

var lastAmp: f32 = 0.0;
var lastOut: f32 = 0.0;
fn blockDc(sample: f32) f32 {
    const output = sample - lastAmp + 0.999 * lastOut;
    lastAmp = sample;
    lastOut = output;
    return output;
}

pub fn drain(self: *Resampler, out: []i16) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    if (!self.active) {
        @memset(out, 0);
        return;
    }

    if (self.read_cursor.dist(self.write_cursor) > self.buffer.len) {
        @panic("buffer overflow, write cursor > read cursor");
    }

    // Prevent underflow: check if there is enough data in the buffer, otherwise block and wait
    var read_end = self.read_cursor;
    read_end.inc(out.len);
    if (read_end.compare(self.write_cursor) == .gt) {
        self.needs_more_data = true;
        self.needed = read_end;
        self.wait_counter += 1;
        // std.log.info("waited for samples -> read cursor {d} write_cursor {d} waits {d} needed {d} avaiable {d}", .{self.read_cursor.i, self.write_cursor.i, self.wait_counter, out.len, self.read_cursor.dist(self.write_cursor)});
        while (self.needs_more_data) {
            // const start = std.time.Instant.now() catch unreachable;
            // TODO: timed wait, figure out max allowed block time
            self.condition.wait(&self.mutex);
            // const end = std.time.Instant.now() catch unreachable;
            // std.log.info("Unblocked after {}", .{std.fmt.fmtDuration(end.since(start))});
        }
    }

    // Copy the downsampled and filtered samples to Raylib's sample buffer
    for (out) |*s| {
        s.* = self.buffer[self.read_cursor.i];
        self.read_cursor.inc(1);
    }

    // Prevent overflow: if write cursor is pulling too far ahead, nudge the read cursor forward
    // to allow it to eventually catch up without introducing audible distortion.
    const lag = self.read_cursor.dist(self.write_cursor);
    self.read_lags[self.read_lags_cursor.i] = lag;
    self.read_lags_cursor.inc(1);
    var cursor = self.read_lags_cursor;
    const window_len = 10;
    const avg_lag = blk: {
        var s: isize = 0;
        var i: usize = 0;
        while (i < window_len) : (i += 1) {
            s += self.read_lags[cursor.i];
            if (cursor.i == 0 and cursor.gen == 0) break;
            cursor.dec();
        }
        break :blk @divFloor(s, @as(isize, @intCast(i)));
    };
    const deviation = avg_lag - 4 * @as(isize, @intCast(self.frame_size));
    if (deviation > 0) {
        const nudge = @min(3, @as(usize, @intCast(@divFloor(deviation, 10))));
        self.read_cursor.inc(nudge);
        // std.log.info("deviation {d} nudge {d} lag {d} avg lag {d}", .{deviation, nudge, lag, avg_lag});
    }
}

// Generated from https://fiiir.com/
// from __future__ import print_function
// from __future__ import division

// import numpy as np

// # Example code, computes the coefficients of a low-pass windowed-sinc filter.

// # Configuration.
// fS = 1789773  # Sampling rate.
// fL = 17897.73  # Cutoff frequency.
// N = 93  # Filter length, must be odd.
// beta = 5.653  # Kaiser window beta.

// # Compute sinc filter.
// h = np.sinc(2 * fL / fS * (np.arange(N) - (N - 1) / 2))

// # Apply window.
// h *= np.kaiser(N, beta)

// # Normalize to get unity gain.
// h /= np.sum(h)

// print(h)

// # Applying the filter to a signal s can be as simple as writing
// # s = np.convolve(s, h)
const filter = [_]f32{
    0.000044941999796296,
    0.000078448426264025,
    0.000125478941498944,
    0.000188698730767867,
    0.000270939463063436,
    0.000375166659923636,
    0.000504441278525424,
    0.000661875867299901,
    0.000850585787843018,
    0.001073636132247923,
    0.001333985095935917,
    0.001634424688427113,
    0.001977519859376312,
    0.002365547216245180,
    0.002800433827812290,
    0.003283698313139941,
    0.003816394851530020,
    0.004399061334045547,
    0.005031672986226442,
    0.005713602587054860,
    0.006443588340301538,
    0.007219710353297611,
    0.008039376550676623,
    0.008899318700415408,
    0.009795599059836908,
    0.010723627963516443,
    0.011678192476933642,
    0.012653496033217740,
    0.013643208759676579,
    0.014640527990390454,
    0.015638248255476123,
    0.016628839841160536,
    0.017604534831892994,
    0.018557419380500068,
    0.019479530808651467,
    0.020362958021054124,
    0.021199943625716915,
    0.021982986091660297,
    0.022704940246279007,
    0.023359114418223498,
    0.023939362568465676,
    0.024440169821754176,
    0.024856729911825717,
    0.025185013184710343,
    0.025421823962772275,
    0.025564846254688973,
    0.025612676999761476,
    0.025564846254688973,
    0.025421823962772275,
    0.025185013184710343,
    0.024856729911825717,
    0.024440169821754176,
    0.023939362568465704,
    0.023359114418223498,
    0.022704940246278987,
    0.021982986091660297,
    0.021199943625716915,
    0.020362958021054124,
    0.019479530808651467,
    0.018557419380500068,
    0.017604534831892994,
    0.016628839841160536,
    0.015638248255476123,
    0.014640527990390454,
    0.013643208759676579,
    0.012653496033217727,
    0.011678192476933642,
    0.010723627963516443,
    0.009795599059836908,
    0.008899318700415408,
    0.008039376550676623,
    0.007219710353297611,
    0.006443588340301538,
    0.005713602587054860,
    0.005031672986226442,
    0.004399061334045550,
    0.003816394851530020,
    0.003283698313139941,
    0.002800433827812290,
    0.002365547216245180,
    0.001977519859376312,
    0.001634424688427113,
    0.001333985095935918,
    0.001073636132247923,
    0.000850585787843018,
    0.000661875867299901,
    0.000504441278525424,
    0.000375166659923636,
    0.000270939463063436,
    0.000188698730767867,
    0.000125478941498944,
    0.000078448426264025,
    0.000044941999796296,
};
