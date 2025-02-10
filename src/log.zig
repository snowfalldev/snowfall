const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;

const Module = @import("Module.zig");

const ast = @import("ast.zig");
const Pos = ast.Pos;
const Span = ast.Span;

const unicode = @import("unicode.zig");

pub fn Logger(comptime scope: @TypeOf(.EnumLiteral)) type {
    return struct {
        allocator: Allocator,
        mod: ?*Module,
        errs: Messages,
        warns: Messages,
        infos: Messages,
        debugs: Messages,

        const Self = @This();

        const Messages = std.ArrayList(Message);
        const Message = struct {
            msg: []const u8,
            info: []const u8,
            data: []const u8,

            pub fn deinit(self: Message, allocator: std.mem.Allocator) void {
                allocator.free(self.msg);
                allocator.free(self.info);
                allocator.free(self.data);
            }
        };

        pub fn init(allocator: Allocator, mod: ?*Module) Self {
            return .{
                .allocator = allocator,
                .mod = mod,
                .errs = Messages.init(allocator),
                .warns = Messages.init(allocator),
                .infos = Messages.init(allocator),
                .debugs = Messages.init(allocator),
            };
        }

        pub fn deinit(self: Self) void {
            for (self.errs.items) |m| m.deinit(self.allocator);
            for (self.warns.items) |m| m.deinit(self.allocator);
            for (self.infos.items) |m| m.deinit(self.allocator);
            for (self.debugs.items) |m| m.deinit(self.allocator);

            self.errs.deinit();
            self.warns.deinit();
            self.infos.deinit();
            self.debugs.deinit();
        }

        inline fn mkMsg(self: Self, comptime fmt: []const u8, args: anytype, span: ?Span, hi: ?Pos) !Message {
            return .{
                .msg = try allocPrint(self.allocator, fmt, args),
                .info = try self.infoString(span),
                .data = try self.dataString(span, hi),
            };
        }

        pub inline fn err(self: *Self, comptime fmt: []const u8, args: anytype, span: ?Span, hi: ?Pos) !void {
            @branchHint(.cold);
            const msg = try self.mkMsg(fmt, args, span, hi);
            try self.errs.append(msg);
            std.log.err("{s} {s}\n{s}", msg);
        }

        pub inline fn warn(self: *Self, comptime fmt: []const u8, args: anytype, span: ?Span, hi: ?Pos) !void {
            const msg = try self.mkMsg(fmt, args, span, hi);
            try self.warns.append(msg);
            std.log.warn("{s} {s}\n{s}", msg);
        }

        pub inline fn info(self: *Self, comptime fmt: []const u8, args: anytype, span: ?Span, hi: ?Pos) !void {
            const msg = try self.mkMsg(fmt, args, span, hi);
            try self.infos.append(msg);
            std.log.info("{s} {s}\n{s}", msg);
        }

        pub inline fn debug(self: *Self, comptime fmt: []const u8, args: anytype, span: ?Span, hi: ?Pos) !void {
            const msg = try self.mkMsg(fmt, args, span, hi);
            try self.debugs.append(msg);
            std.log.debug("{s} {s}\n{s}", msg);
        }

        fn infoString(self: Self, span: ?Span) ![]const u8 {
            var out = std.ArrayList(u8).init(self.allocator);
            errdefer out.deinit();
            if (builtin.mode == .Debug) try out.appendSlice("[" ++ @tagName(scope));

            if (self.mod == null or (self.mod != null and span == null)) {
                if (builtin.mode == .Debug) try out.append(']');
                return out.toOwnedSlice();
            }

            try if (builtin.mode != .Debug) out.append('[') else out.appendSlice(" | ");

            const writer = out.writer();
            try writer.print("{s}", .{self.mod.?.name.buf});
            try if (span) |s| writer.print(" ({}:{})]", .{ s[0].row + 1, s[0].col + 1 }) else out.append(']');
            return out.toOwnedSlice();
        }

        fn dataString(self: Self, span: ?Span, hi: ?Pos) ![]const u8 {
            if (self.mod == null or span == null) return self.allocator.alloc(u8, 0);
            const s = span.?;
            const d = self.mod.?.data.buf;

            const line_start_pos = s[0].raw - s[0].col;
            var line_end_pos: usize = line_start_pos;
            while (line_end_pos < d.len and !unicode.isNewLine(d[line_end_pos])) line_end_pos += 1;

            var out = std.ArrayList(u8).init(self.allocator);
            errdefer out.deinit();
            try out.appendSlice(d[line_start_pos..line_end_pos]);
            try out.append('\n');

            try out.appendNTimes(' ', s[0].col);
            var i: usize = s[0].col;
            if (s[1].col >= s[0].col) {
                if (hi) |h| while (i < h.col) : (i += 1) try out.append('~');
                try out.append('^');
                i += 1;
                while (i < s[1].col + 1) : (i += 1) try out.append('~');
            }
            try out.append('\n');

            return out.toOwnedSlice();
        }
    };
}
