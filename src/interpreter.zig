const std = @import("std");
const common = @import("common.zig");
const messages = @import("messages.zig");
const parser = @import("parser.zig");

pub const Interpreter = struct {
    block: parser.Block,
    statement: usize,

    const Self = @This();

    pub fn interpretFull(self: *Self) !void {
        var i: usize = 0;
        while (i < self.block.items.len) : (i += 1) {
            try self.interpretStatement();
        }
    }

    pub fn interpretStatement(self: *Self) !void {
        var statement = self.block.items[self.statement];

        switch (statement) {
            else => @panic("hammond you blithering idiot"),
        }
    }
};
