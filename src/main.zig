const std = @import("std");
const Allocator = std.mem.Allocator;

const Output = []const u8;
const Solution = fn (Allocator, []const u8) anyerror!Output;

const DayResult = struct {
    elapsed_micros: u64,
    output: Output,
};

fn runDay(allocator: Allocator, input_path: []const u8, comptime func: Solution) !DayResult {
    const start = try std.time.Instant.now();

    const output = try func(allocator, input_path);

    const end = try std.time.Instant.now();
    const time = end.since(start);

    return DayResult{
        .elapsed_micros = time / 1000,
        .output = output,
    };
}

fn printResult(a: DayResult, b: DayResult) void {
    std.debug.print(
        "A: {s} ({}μs), B: {s} ({}μs)\n",
        .{ a.output, a.elapsed_micros, b.output, b.elapsed_micros },
    );
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    { // Day 1
        const a = try runDay(allocator, "inputs/input-1", day1a);
        const b = try runDay(allocator, "inputs/input-1", day1b);
        printResult(a, b);
    }

    { // Day 2
        const a = try runDay(allocator, "inputs/input-2", day2a);
        const b = try runDay(allocator, "inputs/input-2", day2b);
        printResult(a, b);
    }

    { // Day 3
        const a = try runDay(allocator, "inputs/input-3", day3a);
        const b = try runDay(allocator, "inputs/input-3", day3b);
        printResult(a, b);
    }

    { // Day 4
        const a = try runDay(allocator, "inputs/input-4", day4a);
        const b = try runDay(allocator, "inputs/input-4", day4b);
        printResult(a, b);
    }

    { // Day 5
        const a = try runDay(allocator, "inputs/input-5", day5a);
        const b = try runDay(allocator, "inputs/input-5", day5b);
        printResult(a, b);
    }

    { // Day 6
        const a = try runDay(allocator, "inputs/input-6", day6a);
        const b = try runDay(allocator, "inputs/input-6", day6b);
        printResult(a, b);
    }
}

// * Day 1

pub fn day1a(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});
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

    return try outputNum(allocator, max);
}

pub fn day1b(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});
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
    return outputNum(allocator, sum);
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
    var result = try day1a(std.testing.allocator, "inputs/input-1-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "24000");
}

test "day 1b" {
    var result = try day1b(std.testing.allocator, "inputs/input-1-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "45000");
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

pub fn day2a(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});

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
    return outputNum(allocator, score_total);
}

pub fn day2b(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    var score_total: u32 = 0;

    while (true) {
        const move_opponent = Shape.fromOpponentMove(reader.readByte() catch break);
        _ = try reader.readByte();
        const outcome = try reader.readByte();
        _ = reader.readByte() catch {};

        var move_me: Shape = undefined;
        if (outcome == 'X') {
            move_me = switch (move_opponent) {
                .rock => .scissors,
                .paper => .rock,
                .scissors => .paper,
            };
        } else if (outcome == 'Y') {
            move_me = move_opponent;
            score_total += 3;
        } else {
            move_me = switch (move_opponent) {
                .rock => .paper,
                .paper => .scissors,
                .scissors => .rock,
            };
            score_total += 6;
        }

        const score = @enumToInt(move_me) + 1;
        score_total += score;
    }
    return outputNum(allocator, score_total);
}

test "day 2a" {
    var result = try day2a(std.testing.allocator, "inputs/input-2-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "15");
}

test "day 2b" {
    var result = try day2b(std.testing.allocator, "inputs/input-2-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "12");
}

// * Day 3

pub fn day3a(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    var result: u32 = 0;
    var buf: [64]u8 = undefined;
    while (true) {
        const line = try reader.readUntilDelimiterOrEof(&buf, '\n') orelse break;

        const compartment_len = line.len / 2;
        const items = readItems(line[0..compartment_len]);

        for (line[compartment_len..line.len]) |item| {
            var index = indexOfItem(item);
            if (items[index] > 0) {
                result += @intCast(u32, index) + 1;
                break;
            }
        }
    }

    return outputNum(allocator, result);
}

pub fn day3b(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    var result: u32 = 0;
    var buf: [64]u8 = undefined;
    while (true) {
        const line1 = try reader.readUntilDelimiterOrEof(&buf, '\n') orelse break;
        const items1 = readItems(line1);
        const line2 = try reader.readUntilDelimiterOrEof(&buf, '\n') orelse break;
        const items2 = readItems(line2);
        const line3 = try reader.readUntilDelimiterOrEof(&buf, '\n') orelse break;
        const items3 = readItems(line3);

        for (items1) |item1, i| {
            const item2 = items2[i];
            const item3 = items3[i];
            if (item1 + item2 + item3 == 3) {
                result += @intCast(u32, i) + 1;
                break;
            }
        }
    }

    return outputNum(allocator, result);
}

fn indexOfItem(item: u8) usize {
    var index: usize = undefined;
    if (item >= 97) {
        index = item - 97;
    } else {
        index = item - 65 + 26;
    }
    return index;
}

fn readItems(line: []u8) [52]u8 {
    var items: [52]u8 = [_]u8{0} ** 52;
    for (line) |item| {
        var index = indexOfItem(item);
        items[index] = 1;
    }
    return items;
}

test "day 3a" {
    var result = try day3a(std.testing.allocator, "inputs/input-3-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "157");
}

test "day 3b" {
    var result = try day3b(std.testing.allocator, "inputs/input-3-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "70");
}

// * Day 4

pub fn day4a(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    var result: u32 = 0;
    var buf: [64]u8 = undefined;
    while (true) {
        const min1 = readNumUntilDelimiter(u32, reader, &buf, '-') catch break;
        const max1 = try readNumUntilDelimiter(u32, reader, &buf, ',');
        const min2 = try readNumUntilDelimiter(u32, reader, &buf, '-');
        const max2 = try readNumUntilDelimiter(u32, reader, &buf, '\n');

        if (min1 < min2) {
            if (max1 >= max2) {
                result += 1;
            }
        } else if (min1 > min2) {
            if (max2 >= max1) {
                result += 1;
            }
        } else {
            result += 1;
        }
    }

    return outputNum(allocator, result);
}

pub fn day4b(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    var result: u32 = 0;
    var buf: [64]u8 = undefined;
    while (true) {
        const min1 = readNumUntilDelimiter(u32, reader, &buf, '-') catch break;
        const max1 = try readNumUntilDelimiter(u32, reader, &buf, ',');
        const min2 = try readNumUntilDelimiter(u32, reader, &buf, '-');
        const max2 = try readNumUntilDelimiter(u32, reader, &buf, '\n');

        if (min1 == min2 or max1 == max2) {
            result += 1;
        } else if (min1 < min2 and max1 >= min2) {
            result += 1;
        } else if (min2 < min1 and max2 >= min1) {
            result += 1;
        }
    }

    return outputNum(allocator, result);
}

test "day 4a" {
    var result = try day4a(std.testing.allocator, "inputs/input-4-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "2");
}

test "day 4b" {
    var result = try day4b(std.testing.allocator, "inputs/input-4-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "4");
}

// * Day 5

pub fn day5a(allocator: Allocator, input_path: []const u8) !Output {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var file = try std.fs.cwd().openFile(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    var buf: [64]u8 = undefined;

    var stacks = try readStacks(arena.allocator(), reader);

    while (try readMoveCommand(reader, &buf)) |cmd| {
        var i: usize = 0;
        while (i < cmd.amount) : (i += 1) {
            const item = stacks[cmd.from].pop();
            try stacks[cmd.to].append(item);
        }
    }

    // Output is top element of each crate stack
    var output = std.ArrayList(u8).init(allocator);
    for (stacks) |stack| {
        if (stack.items.len == 0) continue;
        const item = stack.items[stack.items.len - 1];
        try output.append(item);
    }

    return output.toOwnedSlice();
}

pub fn day5b(allocator: Allocator, input_path: []const u8) !Output {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var file = try std.fs.cwd().openFile(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    var buf: [64]u8 = undefined;

    var stacks = try readStacks(arena.allocator(), reader);

    while (try readMoveCommand(reader, &buf)) |cmd| {
        var temp = std.ArrayList(u8).init(allocator);
        defer temp.deinit();

        {
            var i: usize = 0;
            while (i < cmd.amount) : (i += 1) {
                const item = stacks[cmd.from].pop();
                try temp.append(item);
              }
        }

        { // Append from end of temp list
            var i: usize = temp.items.len;
            while (i > 0) {
                i -= 1;
                const item = temp.items[i];
                try stacks[cmd.to].append(item);
            }
        }
    }

    // Output is top element of each crate stack
    var output = std.ArrayList(u8).init(allocator);
    for (stacks) |stack| {
        if (stack.items.len == 0) continue;
        const item = stack.items[stack.items.len - 1];
        try output.append(item);
    }

    return output.toOwnedSlice();
}

const MoveCommand = struct {
    amount: u8,
    from: usize,
    to: usize,
};

const CrateStack = std.ArrayList(u8);

fn readMoveCommand(reader: anytype, buf: []u8) !?MoveCommand {
    _ = reader.readUntilDelimiter(buf, ' ') catch return null;
    const amount = try readNumUntilDelimiter(u8, reader, buf, ' ');
    _ = try reader.readUntilDelimiter(buf, ' ');
    const from = try readNumUntilDelimiter(u32, reader, buf, ' ') - 1;
    _ = try reader.readUntilDelimiter(buf, ' ');
    const to = try readNumUntilDelimiter(u32, reader, buf, '\n') - 1;
    return MoveCommand{ .amount = amount, .from = from, .to = to };
}

fn readStacks(allocator: Allocator, reader: anytype) ![]CrateStack {
    var lines = std.ArrayListUnmanaged([]u8){};

    while (true) {
        const line = try reader.readUntilDelimiterAlloc(allocator, '\n', 64);
        if (line.len == 0) {
            break;
        }
        try lines.append(allocator, line);
    }

    const num_stacks = (lines.items[0].len + 1) / 4;

    var result = std.ArrayList(CrateStack).init(allocator);

    { // Initialize each stack
        var i: usize = 0;
        while (i < num_stacks) : (i += 1) {
            try result.append(CrateStack.init(allocator));
        }
    }

    {
        var i = lines.items.len;
        while (i > 0) {
            i -= 1;
            const line = lines.items[i];

            // Add items on to every stack
            var x: usize = 0;
            while (x < num_stacks) : (x += 1) {
                const item = line[4 * x + 1];
                if (item == ' ') continue;
                try result.items[x].append(item);
            }
        }
    }

    return result.toOwnedSlice();
}

test "day 5a" {
    var result = try day5a(std.testing.allocator, "inputs/input-5-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "CMZ");
}

test "day 5b" {
    var result = try day5b(std.testing.allocator, "inputs/input-5-test");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "MCD");
}

// * Day 6

pub fn day6a(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    const signal = try reader.readUntilDelimiterAlloc(allocator, '\n', 4096);
    defer allocator.free(signal);

    var i: u32 = 4;
    while (i < signal.len) : (i += 1) {
        if (checkUniqueBytes(signal[i - 4 .. i])) break;
    }

    return try outputNum(allocator, i);
}

pub fn day6b(allocator: Allocator, input_path: []const u8) !Output {
    var file = try std.fs.cwd().openFile(input_path, .{});

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    const signal = try reader.readUntilDelimiterAlloc(allocator, '\n', 4096);
    defer allocator.free(signal);

    var i: u32 = 14;
    while (i < signal.len) : (i += 1) {
        if (checkUniqueBytes(signal[i - 14 .. i])) break;
    }

    return try outputNum(allocator, i);
}

fn checkUniqueBytes(win: []const u8) bool {
    var i: usize = 0;
    while (i < win.len) : (i += 1) {
        var offset: usize = 0;
        while (offset < win.len - i - 1) : (offset += 1) {
            if (win[i] == win[i + 1 + offset]) {
                return false;
            }
        }
    }
    return true;
}

test "day 6a 1" {
    var result = try day6a(std.testing.allocator, "inputs/input-6-test1");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "7");
}

test "day 6a 2" {
    var result = try day6a(std.testing.allocator, "inputs/input-6-test2");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "5");
}

test "day 6a 3" {
    var result = try day6a(std.testing.allocator, "inputs/input-6-test3");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "6");
}

test "day 6a 4" {
    var result = try day6a(std.testing.allocator, "inputs/input-6-test4");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "10");
}

test "day 6a 5" {
    var result = try day6a(std.testing.allocator, "inputs/input-6-test5");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "11");
}

test "day 6b 1" {
    var result = try day6b(std.testing.allocator, "inputs/input-6-test1");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "19");
}

test "day 6b 2" {
    var result = try day6b(std.testing.allocator, "inputs/input-6-test2");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "23");
}

test "day 6b 3" {
    var result = try day6b(std.testing.allocator, "inputs/input-6-test3");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "23");
}

test "day 6b 4" {
    var result = try day6b(std.testing.allocator, "inputs/input-6-test4");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "29");
}

test "day 6b 5" {
    var result = try day6b(std.testing.allocator, "inputs/input-6-test5");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(result, "26");
}

// * Utils

inline fn readNumUntilDelimiter(comptime T: type, reader: anytype, buf: []u8, delimiter: u8) !T {
    const line = try reader.readUntilDelimiter(buf, delimiter);
    const number = try std.fmt.parseUnsigned(T, line, 10);
    return number;
}

fn outputNum(allocator: Allocator, answer: u32) !Output {
    return try std.fmt.allocPrint(allocator, "{}", .{answer});
}
