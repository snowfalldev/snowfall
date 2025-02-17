const Script = @import("../Script.zig");

const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;

const Self = @This();

script: *Script,

// INIT / DEINIT

pub fn init(script: *Script) Self {
    return .{
        .script = script,
    };
}

pub fn deinit(self: Self) void {
    _ = self;
}
