const std = @import("std");

// ALGORITHMS

const seed: u64 = 0x3243F6A8885A308D; // digits of pi

// folded multiply with a faster 32-bit path
// borrowed from foldhash and modified a bit
inline fn mix(x: u64, y: u64) u64 {
    const builtin = @import("builtin");
    const target = builtin.target;
    const cpu = builtin.cpu;

    // sparc64 and wasm64 do not have 128-bit widening multiplication
    // x86-64 and aarch64 should have it regardless of abi, but abi may reduce ptr bit width
    // Zig 0.14.1 doesn't have wide arithmetic in the wasm feature set, add a check for that eventually
    const wide_mul = (target.ptrBitWidth() == 64 and cpu.arch != .sparc64 and cpu.arch != .wasm64) or
        (cpu.arch == .x86_64 or cpu.arch.isAARCH64());

    if (wide_mul) {
        const full = @as(u128, x) * @as(u128, y);
        const lo: u64 = @truncate(full);
        const hi: u64 = @truncate(full >> 64);
        return lo ^ hi;
    }

    // we don't need a super accurate approximation, so we do half the work here that foldhash does
    // this should still do a good job of mixing bits around, but it's a lot faster

    const lx: u32 = @truncate(x);
    const ly: u32 = @truncate(y);
    const hx: u32 = @truncate(x >> 32);
    const hy: u32 = @truncate(y >> 32);

    const ll = @as(u64, lx) * @as(u64, ly);
    const hh = @as(u64, hx) * @as(u64, hy);

    return ll ^ hh;
}

// safe readInt by copy to buffer for len <= 8 bytes
inline fn readPartial(ptr: [*]const u8, len: usize) u64 {
    var buf: [8]u8 = .{0} ** 8;
    @memcpy(buf[0..8].ptr, ptr[0..len]);
    return std.mem.readInt(u64, &buf, .little);
}

/// Hash function optimized for small strings.
pub fn microhash(input: []const u8) u64 {
    var ptr = input.ptr;
    var len = input.len;
    var out = seed;

    while (len > 16) : (len -= 16) {
        const x = std.mem.readInt(u64, @ptrCast(ptr), .little);
        const y = std.mem.readInt(u64, @ptrCast(ptr + 8), .little);
        out = mix(out ^ x, y);
        out = std.math.rotr(u64, out, 23);
        ptr += 16;
    }

    if (len > 8) {
        const x = std.mem.readInt(u64, @ptrCast(ptr), .little);
        const y = readPartial(ptr + 8, len - 8);
        out = mix(out ^ x, y);
    } else if (len > 0) {
        const x = readPartial(ptr, len);
        out = mix(out, x);
    }

    return out;
}

/// Hash function which can be used for any strings.
pub fn rapidhash(input: []const u8) u64 {
    return @call(.always_inline, std.hash.RapidHash.hash, .{ seed, input });
}

// FAST STRING HASHMAPS

const HashFn = fn ([]const u8) u64;

pub fn StringHashMap(comptime V: type, comptime hash_fn: HashFn) type {
    return std.HashMap([]const u8, V, StringContext(hash_fn), std.hash_map.default_max_load_percentage);
}
pub fn StringHashMapUnmanaged(comptime V: type, comptime hash_fn: HashFn) type {
    return std.HashMapUnmanaged([]const u8, V, StringContext(hash_fn), std.hash_map.default_max_load_percentage);
}
fn StringContext(comptime hash_fn: HashFn) type {
    return struct {
        pub fn hash(_: @This(), s: []const u8) u64 {
            return hash_fn(s);
        }
        pub fn eql(_: @This(), a: []const u8, b: []const u8) bool {
            return std.mem.eql(u8, a, b);
        }
    };
}

pub fn StringArrayHashMap(comptime V: type, comptime hash_fn: HashFn) type {
    return std.ArrayHashMap([]const u8, V, StringArrayContext(hash_fn), true);
}
pub fn StringArrayHashMapUnmanaged(comptime V: type, comptime hash_fn: HashFn) type {
    return std.ArrayHashMapUnmanaged([]const u8, V, StringArrayContext(hash_fn), true);
}
fn StringArrayContext(comptime hash_fn: HashFn) type {
    return struct {
        pub fn hash(_: @This(), s: []const u8) u32 {
            return @truncate(hash_fn(s));
        }
        pub fn eql(_: @This(), a: []const u8, b: []const u8) bool {
            return std.mem.eql(u8, a, b);
        }
    };
}

const StaticStringMapContext = @import("static-map").StaticStringMapContext;
pub fn StaticStringMap(comptime V: type, comptime capacity: u32) type {
    const Context = struct {
        pub fn hash(_: @This(), s: []const u8) u32 {
            return @truncate(microhash(s));
        }
        pub fn eql(_: @This(), a: []const u8, b: []const u8, _: u32) bool {
            return std.mem.eql(u8, a, b);
        }
    };
    return StaticStringMapContext(V, capacity, Context);
}
