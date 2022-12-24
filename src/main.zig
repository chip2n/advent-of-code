const std = @import("std");

pub fn main() !void {
    const result = try day1("/Users/andreas/dev/aoc-2022/src/input-1");
    std.log.info("{}", .{result});
}

pub fn day1(input_path: []const u8) !u32 {
    var file = try std.fs.openFileAbsolute(input_path, .{});
    var buf: [128]u8 = undefined;
    var current: u32 = 0;
    var max: u32 = 0;
    var i: usize = 0;
    while (true) : (i += 1) {
        var line = try file.reader().readUntilDelimiterOrEof(&buf, '\n') orelse break;
        if (line.len == 0) {
            if (current > max) {
                max = current;
            }
            current = 0;
            continue;
        }

        var number = try std.fmt.parseUnsigned(u32, line, 10);
        current += number;
    }
    return max;
}

test "input 1" {
    var result = try day1("/Users/andreas/dev/aoc-2022/src/input-1-test");
    try std.testing.expectEqual(result, 24000);
}
