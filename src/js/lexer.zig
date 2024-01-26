const std = @import("std");
const common = @import("common.zig");
const messages = @import("messages.zig");

const Allocator = std.mem.Allocator;

const allocPrint = std.fmt.allocPrint;
const minInt = std.math.minInt;
const maxInt = std.math.maxInt;
const floatMin = std.math.floatMin;
const floatMax = std.math.floatMax;
const parseUnsigned = std.fmt.parseUnsigned;
const parseInt = std.fmt.parseInt;
const parseFloat = std.fmt.parseFloat;

const getOptional = common.getOptional;
const isNumberChar = common.isNumberChar;
const isHexadecimalChar = common.isHexadecimalChar;
const containsChar = common.containsChar;
const containsCharRuntime = common.containsCharRuntime;
const containsString = common.containsString;

const Position = common.Position;
const Span = common.Span;
const Messages = messages.Messages;

// A lexer number.
// Can be an integer; integer math is quicker so use it for constant operations if appropriate.
pub const Number = union(enum) {
    float: f64,
    int: i53,
};

// A lexer BigInt.
// Can be an integer; integer math is quicker so use it for constant operations if appropriate.
pub const BigInt = union(enum) {
    int: isize,
};

// A lexer token.
pub const Token = union(enum) {
    // values
    null,
    undefined,
    boolean: bool,
    string: []u16,
    number: Number,
    bigint: BigInt,

    // other
    symbol: u8,
    identifier: []u16,

    const Self = @This();

    // Convert from a string representing a basic token (if possible).
    pub fn fromBasicString(allocator: Allocator, string: []u8) ?Self {
        if (std.mem.eql(u8, string, "null")) {
            return Self.null;
        } else if (std.mem.eql(u8, string, "undefined")) {
            return Self.undefined;
        } else if (std.mem.eql(u8, string, "true")) {
            return .{ .boolean = true };
        } else if (std.mem.eql(u8, string, "false")) {
            return .{ .boolean = false };
        } else if (isIdentifier(string)) {
            return .{ .identifier = utf8ToUtf16(allocator, string) orelse return null };
        }

        return null;
    }

    // Convert a token to string representation.
    pub fn toString(self: Self, allocator: Allocator) ![]u8 {
        return switch (self) {
            Token.null => try allocPrint(allocator, "null", .{}),
            Token.undefined => try allocPrint(allocator, "undefined", .{}),
            Token.boolean => |v| try allocPrint(allocator, "boolean: {}", .{v}),
            Token.string => |v| try allocPrint(allocator, "string: \"{s}\"", .{v}),
            Token.number => |v| try allocPrint(allocator, "uint: {}", .{v}),
            Token.bigint => |v| try allocPrint(allocator, "int: {}", .{v}),

            Token.symbol => |v| try allocPrint(allocator, "symbol: {c}", .{v}),
            Token.identifier => |v| try allocPrint(allocator, "identifier: {s}", .{v}),
        };
    }

    // Is the token the end of an expression or an expression itself?
    pub fn isExpression(self: Self) bool {
        return switch (self) {
            Token.null => true,
            Token.undefined => true,
            Token.boolean => true,
            Token.string => true,
            Token.number => true,
            Token.bigint => true,

            Token.symbol => |symbol| switch (symbol) {
                ')' => true, // End of function call or expression in parenthesis.
                else => false,
            },

            Token.identifier => true,
        };
    }
};

pub const LocatedToken = struct {
    span: Span,
    token: Token,
};

pub const Tokens = std.ArrayList(LocatedToken);

// Non-decimal representation characters in numbers.
pub const non_dec_number_chars = [_]u8{
    'x', 'X', // hexadecimal
    'b', 'B', // binary
    'O', 'O', // octal
    'e', 'E', // exponent
};

// All valid separating symbols.
pub const separating_symbols = [_]u8{
    '&', // logical/bitwise and
    '|', // logical/bitwise or
    '!', // logical not
    '~', // bitwise not
    '^', // bitwise xor
    '<', // less than comparison operator / left shift
    '>', // greater than comparison operator / right shift
    '*', // reference / multiplication operator / comment
    '/', // division operator / comment
    '{', // open block / array
    '(', // open tuple / arguments
    '[', // open array slicer
    '}', // close block / array
    ')', // close tuple / arguments
    ']', // close array slicer
    ':', // explicit types (TS) / ternary selection
    ',', // separate arguments and array items
    '=', // set & equals comparison operator
    '%', // modulus operator
    '+', // addition operator
    '-', // subtraction operator
    '?', // ternary operator
    '@', // builtins / annotations
    ';', // statement separator
    '\n', // semicolons are optional
};

// All valid symbols.
pub const symbols = separating_symbols ++ [_]u8{
    '.', // field access
};

// Check if "content" (array of bytes) is a valid identifier.
pub inline fn isIdentifier(content: []u8) bool {
    return content.len > 0 and !isNumberChar(content[0]);
}

pub const State = enum {
    string,
    number,
    regex,
    none,
};

pub const Lexer = struct {
    // Stores the allocator used by the lexer.
    allocator: Allocator,

    // Stores the output of the lexer run.
    output: Tokens,

    // Stores the name of the file being processed.
    file: ?[]const u8,

    // Stores the file data.
    data: []const u8,

    // Stores the current position in the data.
    pos: Position = .{ .raw = 0, .row = 0, .col = 0 },
    // Stores the starting position of the next token. Only used for some tokens.
    token_start: Position = .{ .raw = 0, .row = 0, .col = 0 },

    // Stores the current and next characters.
    chars: struct { u8, u8 },
    // Stores characters currently in use to create the next token.
    buffer: StringUnmanaged,
    // Stores the current lexer state.
    state: State = State.none,

    const Self = @This();

    // Initializes a new lexer.
    pub fn init(allocator: Allocator, name: ?[]const u8, data: []const u8) Self {
        return .{
            .allocator = allocator,
            .output = Tokens.init(allocator),
            .data = data,
            .chars = .{
                getOptional(u8, @constCast(data), 0) orelse 0,
                getOptional(u8, @constCast(data), 1) orelse 0,
            },
            .buffer = std.ArrayList(u8).init(allocator),
        };
    }

    // De-initializes the lexer.
    // This should only be run after the output of the lexer is done being used.
    pub fn deinit(self: *Self) void {
        self.output.deinit();
        self.buffer.deinit();
        self.messages.deinit();
    }

    // Reinitializes the lexer with new data retaining capacity.
    // This is the optimal solution for lexing multiple source files back to back.
    pub fn reinitRetainingCapacity(self: *Self, data: []const u8) void {
        self.data = data;
        self.clearRetainingCapacity();
    }

    // Reinitializes the lexer with new data and frees.
    pub fn reinitAndFree(self: *Self, data: []const u8) void {
        self.data = data;
        self.clearAndFree();
    }

    // Clears the lexer retaining capacity, making it available to parse again.
    pub fn clearRetainingCapacity(self: *Self) void {
        self.output.clearRetainingCapacity();
        self.chars = .{
            getOptional(u8, @constCast(self.data), 0) orelse 0,
            getOptional(u8, @constCast(self.data), 1) orelse 0,
        };
        self.pos = .{ .raw = 0, .row = 0, .col = 0 };
        self.buffer.clearRetainingCapacity();
        self.state = State.none;
        self.token_start = .{ .raw = 0, .row = 0, .col = 0 };
        self.messages.clearRetainingCapacity();
    }

    // Clears the lexer and frees, making it available to parse again.
    pub fn clearAndFree(self: *Self) void {
        self.output.clearAndFree();
        self.chars = .{
            getOptional(u8, @constCast(self.data), 0) orelse 0,
            getOptional(u8, @constCast(self.data), 1) orelse 0,
        };
        self.pos = .{ .raw = 0, .row = 0, .col = 0 };
        self.buffer.clearAndFree();
        self.state = State.none;
        self.token_start = .{ .raw = 0, .row = 0, .col = 0 };
        self.messages.clearAndFree();
    }

    // Lexes the entire queue.
    pub fn lexFull(self: *Self) !Tokens {
        while (self.pos.raw < self.data.len) try self.lexNext();
        return self.output;
    }

    // Returns the last token the lexer outputted.
    pub fn getLastToken(self: *Self) ?LocatedToken {
        if (self.output.items.len == 0) return null;
        return self.output.items[self.output.items.len - 1];
    }

    // Get a character in the queue relative to the current position.
    pub inline fn getCharRelative(self: *Self, offset: isize) u8 {
        return getOptional(u8, @constCast(self.data), self.pos.raw + offset) orelse 0;
    }

    // Start a new token.
    pub inline fn startToken(self: *Self) void {
        self.token_start = self.pos;
    }

    // Get the starting position of the current token.
    pub inline fn getTokenStart(self: *Self) Position {
        return if (self.state != State.none) self.token_start else self.pos.back(self.buffer.items.len);
    }

    // Adds the basic token from the current buffer to the output.
    pub fn addBasicToken(self: *Self) !void {
        if (self.buffer.items.len == 0) return;

        const content = (try self.buffer.clone()).items;
        const start = self.getTokenStart();
        const end = self.pos.back(1);

        if (Token.fromBasicString(content)) |token| {
            try self.output.append(.{ .start = start, .end = end, .token = token });
        }

        self.buffer.clearRetainingCapacity();
    }

    // Moves the lexer position, and sets the current and next characters.
    // Skips newline characters.
    pub fn advance(self: *Self) !void {
        self.pos.raw += 1;
        self.pos.col += 1;

        var new_lines: usize = 0;

        while (self.pos.raw < self.data.len and self.data[self.pos.raw] == '\n') : (new_lines += 1) {
            if (self.state == State.string) try self.buffer.append('\n');

            if (new_lines == 0 and self.state == State.none) {
                try self.addBasicToken();
            }

            self.pos.raw += 1;
            self.pos.row += 1;
            self.pos.col = 0;
        }

        self.chars = .{
            self.data[self.pos.raw],
            self.getCharRelative(1),
        };
    }

    // Lex string and character escape sequences.
    pub fn lexEscapeSequences(self: *Self, comptime exit: u8) !void {
        try self.advance();

        switch (self.chars[1]) {
            'n' => try self.buffer.append('\n'),
            'r' => try self.buffer.append('\r'),
            't' => try self.buffer.append('\t'),
            '\\' => try self.buffer.append('\\'),
            exit => try self.buffer.append(exit),
            'x' => {
                if (self.data.len > self.pos.raw + 3) {
                    try self.advance(); // Advance past '\'.
                    try self.advance(); // Advance past 'x'.

                    try self.buffer.append(parseUnsigned(u8, &self.chars, 16) catch ret: {
                        try self.messages.printError("bad hexadecimal number.", .{}, self.pos, null); // Overflow is unreachable.
                        break :ret 0;
                    });
                } else {
                    try self.messages.printError("invalid escape sequence.", .{}, self.pos, null);
                    try self.buffer.append(0);
                }
            },
            'u' => {
                try self.advance(); // Advance past '\'.
                try self.advance(); // Advance past 'u'.

                if (self.chars[0] == '{') {
                    try self.advance();

                    var second = false;
                    var tmp = try std.ArrayList(u8).initCapacity(self.allocator, 2);
                    var hex = std.ArrayList(u8).init(self.allocator);
                    defer tmp.deinit();
                    defer hex.deinit();

                    while (self.pos.raw < self.data.len) : (try self.advance()) {
                        if (!isHexadecimalChar(self.chars[0])) {
                            try self.messages.printError("invalid escape sequence.", .{}, self.pos, null);
                            try self.buffer.append(0);
                            return;
                        }

                        tmp.appendAssumeCapacity(self.chars[0]);

                        if (second or !isHexadecimalChar(self.chars[1])) {
                            try hex.append(try parseInt(u8, tmp.items, 16)); // Don't catch; next if statement ensures this will work.
                            tmp.clearRetainingCapacity();
                        }

                        if (self.chars[1] == '}') break;

                        second = !second;
                    }

                    try self.buffer.appendSlice(try hex.toOwnedSlice());
                } else {
                    _ = self.data[self.pos.raw .. self.pos.raw + 4];
                }
            },
            else => {
                try self.messages.printError("invalid escape sequence.", .{}, self.pos, null);
                try self.buffer.append(0);
            },
        }
    }

    // Lex characters into the next token(s).
    pub fn lexNext(self: *Self) !void {
        if (self.pos.raw >= self.data.len) return;

        var initial_len = self.output.items.len;

        while (self.pos.raw < self.data.len and initial_len == self.output.items.len) : (try self.advance()) {
            var explicit_sign_number = (self.chars[0] == '+' or self.chars[0] == '-') and isNumberChar(self.chars[1]);

            // Explicitly +/- numbers; we must ensure the last token was not an expression.
            if (explicit_sign_number) {
                var last_token = self.getLastToken();
                if (last_token == null) try self.messages.printFatal("invalid syntax.", .{}, self.pos, null);
                explicit_sign_number = explicit_sign_number and !last_token.?.token.isExpression();
            }

            // Number lexer.
            if (!isIdentifier(self.buffer.items) and (isNumberChar(self.chars[0]) or explicit_sign_number)) {
                self.startToken();
                self.state = State.number;

                var basic = !(self.chars[0] == '0' and containsChar(@constCast(&non_dec_number_chars), self.chars[1]));
                var float = false;
                var bigint = false;

                if (explicit_sign_number) {
                    try self.buffer.append(self.chars[0]);
                    try self.advance();
                }

                while (self.pos.raw < self.data.len) : (try self.advance()) {
                    if (isNumberChar(self.chars[0]) or self.chars[0] == '_' or (isHexadecimalChar(self.chars[0]) and !basic)) {
                        try self.buffer.append(self.chars[0]);
                    } else if (self.chars[0] == '.') {
                        if (!basic) try self.messages.printFatal("invalid number format.", .{}, self.pos, self.pos);
                        if (float) break;
                        float = true;
                        try self.buffer.append(self.chars[0]);
                    } else if (containsChar(@constCast(&symbols), self.chars[0])) {
                        break;
                    } else if (self.chars[1] == '\n') {
                        try self.buffer.append(self.chars[0]);
                        try self.advance();
                        break;
                    } else if (self.chars[0] == 'n' and !float) {
                        bigint = true;
                        break;
                    } else {
                        try self.messages.printFatal("invalid number format.", .{}, self.pos, self.pos);
                    }
                }

                var number_start = self.getTokenStart();
                var number_data = (try self.buffer.clone()).items;
                var number_end = number_start.forward(number_data.len - 1);

                self.startToken();
                self.buffer.clearRetainingCapacity();

                if (bigint) {} else {
                    if (float) {}

                    var min = int_min.get(type_string).?;
                    var max = int_max.get(type_string).?;

                    if (parseFloat(f64, number_data, 0)) |number_value| {
                        if (number_value < min or number_value > max) try self.messages.printError("number cannot fit in type {s}.", .{type_string}, number_start, number_end);
                        break :int .{ .int = number_value };
                    } else |_| { // InvalidCharacter unreachable.
                        try self.messages.printError("number cannot fit in type {s}.", .{type_string}, number_start, number_end);
                    }
                }

                try self.output.append(.{
                    .start = number_start,
                    .end = number_end,
                    .token = number,
                });

                if (explicit_type_char != null) try self.output.append(.{
                    .start = self.getTokenStart(),
                    .end = self.pos.back(1),
                    .token = Token{ .identifier = type_string },
                });

                self.state = State.none;
                self.buffer.clearRetainingCapacity();
                return;
            }

            switch (self.chars[0]) {
                '"', '\'' => { // String lexer.
                    const exit = self.chars[0];
                    self.startToken();
                    self.state = State.string;

                    while (self.pos.raw < self.data.len) : (try self.advance()) {
                        if (self.chars[1] == exit) {
                            try self.advance();
                            break;
                        } else if (self.chars[1] == '\\') {
                            try self.lexEscapeSequences(exit);
                        } else {
                            try self.buffer.append(self.chars[1]);
                        }
                    }

                    try self.output.append(.{
                        .start = self.getTokenStart(),
                        .end = self.pos,
                        .token = .{ .string = (try self.buffer.clone()).items },
                    });

                    self.state = State.none;
                    self.buffer.clearRetainingCapacity();
                },
                '/' => switch (self.chars[1]) {
                    '/' => while (self.chars[1] != '\n') {
                        // Single-line comment.
                        try self.advance();
                    },
                    '*' => {
                        // Multi-line comment.
                        while (self.chars[0] != '*' and self.chars[1] != '/') {
                            try self.advance();
                        }
                        try self.advance();
                    },
                    else => {
                        const last_token = self.getLastToken();

                        if (last_token != null and !last_token.?.token.isExpression()) {
                            self.state = State.regex;
                        } else {
                            try self.addBasicToken();
                            try self.output.append(.{
                                .start = self.pos,
                                .end = self.pos,
                                .token = .{ .symbol = '/' },
                            });
                        }
                    },
                },
                ' ' => try self.addBasicToken(), // Separator; like a symbol but not stored.
                else => if (!containsChar(@constCast(&symbols), self.chars[0])) {
                    try self.buffer.append(self.chars[0]);
                } else { // Symbol.
                    try self.addBasicToken();
                    try self.output.append(.{
                        .start = self.pos,
                        .end = self.pos,
                        .token = .{ .symbol = self.chars[0] },
                    });
                },
            }
        }
    }
};
