const std = @import("std");

pub fn main() !void {
    const resultA = try day1a("/Users/andreas/dev/aoc-2022/src/input-1");
    const resultB = try day1b("/Users/andreas/dev/aoc-2022/src/input-1");
    std.log.info("A: {}, B: {}", .{ resultA, resultB });
}

pub fn day1a(input_path: []const u8) !u32 {
    var file = try std.fs.openFileAbsolute(input_path, .{});
    var buf: [128]u8 = undefined;
    var current: u32 = 0;
    var max: u32 = 0;
    var i: usize = 0;
    while (true) : (i += 1) {
        var line = try file.reader().readUntilDelimiterOrEof(&buf, '\n');
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

    var buf: [128]u8 = undefined;
    var current: u32 = 0;
    while (true) {
        var line = try file.reader().readUntilDelimiterOrEof(&buf, '\n');
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
