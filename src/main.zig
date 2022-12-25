const std = @import("std");

pub fn main() !void {
    { // Day 1
        const start_a = try std.time.Instant.now();
        const result_a = try day1a("/Users/andreas/dev/aoc-2022/src/input-1");
        const end_a = try std.time.Instant.now();
        const time_a = end_a.since(start_a);

        const start_b = try std.time.Instant.now();
        const result_b = try day1b("/Users/andreas/dev/aoc-2022/src/input-1");
        const end_b = try std.time.Instant.now();
        const time_b = end_b.since(start_b);
        std.debug.print(
            "A: {} ({}μs), B: {} ({}μs)\n",
            .{ result_a, time_a / 1000, result_b, time_b / 1000 },
        );
    }

    { // Day 2
        const start_a = try std.time.Instant.now();
        const result_a = try day2a("/Users/andreas/dev/aoc-2022/src/input-2");
        const end_a = try std.time.Instant.now();
        const time_a = end_a.since(start_a);

        const start_b = try std.time.Instant.now();
        const result_b = try day2a("/Users/andreas/dev/aoc-2022/src/input-2");
        const end_b = try std.time.Instant.now();
        const time_b = end_b.since(start_b);
        std.debug.print(
            "A: {} ({}μs), B: {} ({}μs)\n",
            .{ result_a, time_a / 1000, result_b, time_b / 1000 },
        );
    }
}

// * Day 1

pub fn day1a(input_path: []const u8) !u32 {
    var file = try std.fs.openFileAbsolute(input_path, .{});
    var buf: [16]u8 = undefined;
    var current: u32 = 0;
    var max: u32 = 0;
    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();
    while (true) {
        var line = try reader.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null or line.?.len == 0) {
            if (current > max) {
                max = current;
            }
            current = 0;
        } else {
            var number = try std.fmt.parseUnsigned(u32, line.?, 10);
            current += number;
        }

        if (line == null) break;
    }
    return max;
}

pub fn day1b(input_path: []const u8) !u32 {
    var file = try std.fs.openFileAbsolute(input_path, .{});
    var result: [3]u32 = [_]u32{ 0, 0, 0 };

    var buf: [16]u8 = undefined;
    var current: u32 = 0;
    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();
    while (true) {
        var line = try reader.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null or line.?.len == 0) {
            insertResult(&result, current);
            current = 0;
        } else {
            var number = try std.fmt.parseUnsigned(u32, line.?, 10);
            current += number;
        }

        if (line == null) break;
    }

    var sum: u32 = 0;
    for (result) |n| {
        sum += n;
    }
    return sum;
}

fn insertResult(result: []u32, n: u32) void {
    var smallestIndex: usize = 0;
    for (result) |x, i| {
        if (x < result[smallestIndex]) {
            smallestIndex = i;
        }
    }

    if (result[smallestIndex] < n) {
        result[smallestIndex] = n;
    }
}

test "day 1a" {
    var result = try day1a("/Users/andreas/dev/aoc-2022/src/input-1-test");
    try std.testing.expectEqual(result, 24000);
}

test "day 1b" {
    var result = try day1b("/Users/andreas/dev/aoc-2022/src/input-1-test");
    try std.testing.expectEqual(result, 45000);
}

// * Day 2

const Shape = enum {
    const Self = @This();

    rock,
    paper,
    scissors,

    pub fn fromOpponentMove(byte: u8) Self {
        return switch (byte) {
            'A' => .rock,
            'B' => .paper,
            'C' => .scissors,
            else => @panic("Invalid input"),
        };
    }

    pub fn fromMeMove(byte: u8) Self {
        return switch (byte) {
            'X' => .rock,
            'Y' => .paper,
            'Z' => .scissors,
            else => @panic("Invalid input"),
        };
    }
};

pub fn day2a(input_path: []const u8) !u32 {
    var file = try std.fs.openFileAbsolute(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    var score_total: u32 = 0;

    while (true) {
        const move_opponent = Shape.fromOpponentMove(reader.readByte() catch break);
        _ = try reader.readByte();
        const move_me = Shape.fromMeMove(try reader.readByte());
        _ = reader.readByte() catch {};

        const score = @enumToInt(move_me) + 1;
        score_total += score;

        if (move_opponent == move_me) {
            score_total += 3;
        } else if (move_opponent == .rock and move_me == .paper) {
            score_total += 6;
        } else if (move_opponent == .paper and move_me == .scissors) {
            score_total += 6;
        } else if (move_opponent == .scissors and move_me == .rock) {
            score_total += 6;
        }
    }
    return score_total;
}

test "day 2a" {
    var result = try day2a("/Users/andreas/dev/aoc-2022/src/input-2-test");
    try std.testing.expectEqual(result, 15);
}
