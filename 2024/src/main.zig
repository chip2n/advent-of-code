const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const out = std.io.getStdOut().writer();

    { // Day 1a
        const result = try day1a(allocator, @embedFile("input1"));
        try out.print("Day 1a: {}\n", .{result});
    }

    { // Day 1b
        const result = try day1b(allocator, @embedFile("input1"));
        try out.print("Day 1b: {}\n", .{result});
    }

    { // Day 2a
        const result = try day2a(allocator, @embedFile("input2"));
        try out.print("Day 2a: {}\n", .{result});
    }

    { // Day 2b
        const result = try day2b(allocator, @embedFile("input2"));
        try out.print("Day 2b: {}\n", .{result});
    }
}

// * Day 1

pub fn day1a(allocator: std.mem.Allocator, input: []const u8) !u32 {
    var lines = std.mem.tokenizeScalar(u8, input, '\n');
    var l1 = std.ArrayList(u32).init(allocator);
    var l2 = std.ArrayList(u32).init(allocator);
    while (lines.next()) |line| {
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        try l1.append(try parseInt(tokens.next().?));
        try l2.append(try parseInt(tokens.next().?));
    }

    std.mem.sort(u32, l1.items, {}, std.sort.asc(u32));
    std.mem.sort(u32, l2.items, {}, std.sort.asc(u32));

    var result: u32 = 0;
    for (l1.items, l2.items) |a, b| {
        result += if (a < b) b - a else a - b;
    }
    return result;
}

pub fn day1b(allocator: std.mem.Allocator, input: []const u8) !u32 {
    var lines = std.mem.tokenizeScalar(u8, input, '\n');
    var l1 = std.ArrayList(u32).init(allocator);
    var l2 = std.ArrayList(u32).init(allocator);
    while (lines.next()) |line| {
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        try l1.append(try parseInt(tokens.next().?));
        try l2.append(try parseInt(tokens.next().?));
    }

    var count = std.AutoHashMap(u32, u32).init(allocator);
    for (l2.items) |n| {
        const r = try count.getOrPut(n);
        if (r.found_existing) {
            r.value_ptr.* += 1;
        } else {
            r.value_ptr.* = 1;
        }
    }

    var result: u32 = 0;
    for (l1.items) |n| {
        result += n * (count.get(n) orelse 0);
    }
    return result;
}

test "day 1" {
    try testSolution(day1a, "input1-test", 11);
    try testSolution(day1b, "input1-test", 31);
}

// * Day 2

pub fn day2a(allocator: std.mem.Allocator, input: []const u8) !u32 {
    _ = allocator;
    var lines = std.mem.tokenizeScalar(u8, input, '\n');
    var result: u32 = 0;
    while (lines.next()) |line| {
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        var prev: ?u32 = null;
        var increasing: ?bool = null;
        while (tokens.next()) |token| {
            const n = try parseInt(token);
            if (prev) |p| {
                if (increasing == null) {
                    increasing = p < n;
                }
                if (p == n) break;
                if (increasing.?) {
                    if (p > n) break;
                    if (n - p > 3) break;
                } else {
                    if (p < n) break;
                    if (p - n > 3) break;
                }
            }
            prev = n;
        } else {
            result += 1;
        }
    }
    return result;
}

pub fn day2b(allocator: std.mem.Allocator, input: []const u8) !u32 {
    var lines = std.mem.tokenizeScalar(u8, input, '\n');
    var result: u32 = 0;

    const Helpers = struct {
        fn validate(nums: []const u32, skip: usize) bool {
            var prev: ?u32 = null;
            var increasing: ?bool = null;
            for (nums, 0..) |n, i| {
                if (i == skip) continue;
                if (prev) |p| {
                    if (increasing == null) {
                        increasing = p < n;
                    }
                    if (p == n) break;
                    if (increasing.?) {
                        if (p > n) break;
                        if (n - p > 3) break;
                    } else {
                        if (p < n) break;
                        if (p - n > 3) break;
                    }
                }
                prev = n;
            } else {
                return true;
            }
            return false;
        }
    };

    while (lines.next()) |line| {
        var token_iter = std.mem.tokenizeScalar(u8, line, ' ');
        var nums = std.ArrayList(u32).init(allocator);
        while (token_iter.next()) |token| {
            const n = try parseInt(token);
            try nums.append(n);
        }
        for (nums.items, 0..) |_, i| {
            const valid = Helpers.validate(nums.items, i);
            if (valid) {
                result += 1;
                break;
            }
        }
    }

    return result;
}

test "day2a" {
    try testSolution(day2a, "input2-test", 2);
    try testSolution(day2b, "input2-test", 4);
}

// * Utils

fn parseInt(s: []const u8) !u32 {
    return try std.fmt.parseInt(u32, s, 10);
}

fn testSolution(solver: anytype, comptime input: []const u8, expected: u32) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const result = try solver(arena.allocator(), @embedFile(input));
    try std.testing.expectEqual(expected, result);
}
