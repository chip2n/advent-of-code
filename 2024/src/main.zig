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
}

// * Day 1

pub fn day1a(allocator: std.mem.Allocator, input: []const u8) !u32 {
    var lines = std.mem.tokenizeAny(u8, input, "\n");
    var l1 = std.ArrayList(u32).init(allocator);
    var l2 = std.ArrayList(u32).init(allocator);
    while (lines.next()) |line| {
        var tokens = std.mem.tokenizeAny(u8, line, " ");
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
    var lines = std.mem.tokenizeAny(u8, input, "\n");
    var l1 = std.ArrayList(u32).init(allocator);
    var l2 = std.ArrayList(u32).init(allocator);
    while (lines.next()) |line| {
        var tokens = std.mem.tokenizeAny(u8, line, " ");
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
