const std = @import("std");
const Allocator = std.mem.Allocator;

const Script = @import("../Script.zig");

const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;

// MESSAGES

const log = @import("../log.zig");

// zig fmt: off
const message = log.simpleMessage(&.{}, &.{});
// zig fmt: on

pub const Message = message[0];
pub const Error = Allocator.Error || message[1];
const Logger = log.Logger(Message, message[1], message[2]);

// STRUCTURE

script: *Script,

logger: Logger,

const Self = @This();

// INIT / DEINIT

pub fn init(script: *Script) Self {
    return .{
        .script = script,
    };
}

pub fn deinit(self: Self) void {
    _ = self;
}
