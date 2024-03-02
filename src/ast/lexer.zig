const std = @import("std");
const log = @import("../log.zig");
const source = @import("source.zig");
const common = @import("../common.zig");

const Allocator = std.mem.Allocator;

const Logger = @import("../log.zig").Logger;

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

const Position = source.Position;
const Span = source.Span;

// A lexer token.
pub const Token = union(enum) {
    // values
    null,
    undefined,
    boolean: bool,
    char: u8,
    string: []u8,
    uint: u128,
    int: i128,
    float: f128,

    // other
    symbol: u8,
    identifier: []u8,
    keyword: []u8,

    const Self = @This();

    // Convert from a string representing a basic token (if possible).
    pub fn fromBasicString(string: []u8) ?Self {
        if (std.mem.eql(u8, string, "null")) {
            return Self.null;
        } else if (std.mem.eql(u8, string, "undefined")) {
            return Self.undefined;
        } else if (std.mem.eql(u8, string, "true")) {
            return .{ .boolean = true };
        } else if (std.mem.eql(u8, string, "false")) {
            return .{ .boolean = false };
        } else if (isIdentifier(string)) {
            if (containsString(@constCast(&keywords), string)) {
                return .{ .keyword = string };
            } else {
                return .{ .identifier = string };
            }
        }

        return null;
    }

    // Convert a token to string representation.
    pub fn toString(self: Self, allocator: Allocator) ![]u8 {
        return switch (self) {
            Token.null => try allocPrint(allocator, "null", .{}),
            Token.undefined => try allocPrint(allocator, "undefined", .{}),
            Token.boolean => |v| try allocPrint(allocator, "boolean: {}", .{v}),
            Token.char => |v| try allocPrint(allocator, "char: {}", .{v}),
            Token.string => |v| try allocPrint(allocator, "string: \"{s}\"", .{v}),
            Token.uint => |v| try allocPrint(allocator, "uint: {}", .{v}),
            Token.int => |v| try allocPrint(allocator, "int: {}", .{v}),
            Token.float => |v| try allocPrint(allocator, "int: {}", .{v}),

            Token.symbol => |v| try allocPrint(allocator, "symbol: {c}", .{v}),
            Token.identifier => |v| try allocPrint(allocator, "identifier: {s}", .{v}),
            Token.keyword => |v| try allocPrint(allocator, "keyword: {s}", .{v}),
        };
    }

    // Is the token the end of an expression or an expression itself?
    pub fn isExpression(self: Self) bool {
        return switch (self) {
            Token.symbol => |symbol| switch (symbol) {
                ')' => true, // End of function call or expression in parenthesis.
                else => false,
            },

            else => true,
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
    ':', // explicit types / ternary selection
    ',', // separate arguments and array items
    '=', // set & equals comparison operator
    '%', // modulus operator
    '+', // addition operator
    '-', // subtraction operator
    '?', // ternary operator
    '@', // builtins / annotations
    ';', // statement separator
};

// All valid symbols.
pub const symbols = separating_symbols ++ [_]u8{
    '.', // field access
};

// All valid keywords.
pub const keywords = [_][]const u8{
    "as", // casting
    "enum", // define enum
    "error", // define error
    "throws", // function throws error
    "raise", // raise error
    "try", // try error returning block
    "catch", // catch error from try block
    "if", // define conditional if statement
    "else", // define conditional else statement
    "switch", // define switch statement
    "pub", // publicize declaration
    "const", // declare constant value
    "let", // declare variable
    "mut", // allow variable mutation
    "fn", // define function
    "return", // return from function
    "section", // define section
    "goto", // go to section
    "struct", // define struct
    "interface", // define interface
    "loop", // define infinite loop
    "while", // define while loop
    "for", // define for loop
    "in", // in selector
    "range", // produce range
    "break", // loop break
    "continue", // loop continue
};

// Check if "content" (array of bytes) is a valid identifier.
pub inline fn isIdentifier(content: []u8) bool {
    return content.len > 0 and !isNumberChar(content[0]);
}

// Number types.
// *size types are platform based.
pub const number_types = [_][]const u8{
    "usize", // unsigned platform-based integer
    "u128", // unsigned 128-bit integer
    "u64", // unsigned 64-bit integer
    "u32", // unsigned 32-bit integer
    "u16", // unsigned 16-bit integer
    "u8", // unsigned 8-bit integer
    "isize", // signed platform-based integer
    "i128", // signed 128-bit integer
    "i64", // signed 64-bit integer
    "i32", // signed 32-bit integer
    "i16", // signed 16-bit integer
    "i8", // signed 8-bit integer
    "f128", // 128-bit float
    "f64", // 64-bit float
    "f32", // 32-bit float
    "f16", // 16-bit float
};

// Maximum values for uint types.
pub const uint_max = std.ComptimeStringMap(u128, .{
    .{ "usize", maxInt(usize) },
    .{ "u128", maxInt(u128) },
    .{ "u64", maxInt(u64) },
    .{ "u32", maxInt(u32) },
    .{ "u16", maxInt(u16) },
    .{ "u8", maxInt(u8) },
});

// Minimum values for int types.
pub const int_min = std.ComptimeStringMap(i128, .{
    .{ "isize", minInt(isize) },
    .{ "i128", minInt(i128) },
    .{ "i64", minInt(i64) },
    .{ "i32", minInt(i32) },
    .{ "i16", minInt(i16) },
    .{ "i8", minInt(i8) },
});

// Maximum values for int types.
pub const int_max = std.ComptimeStringMap(i128, .{
    .{ "isize", maxInt(isize) },
    .{ "i128", maxInt(i128) },
    .{ "i64", maxInt(i64) },
    .{ "i32", maxInt(i32) },
    .{ "i16", maxInt(i16) },
    .{ "i8", maxInt(i8) },
});

// Minimum values for float types.
pub const float_min = std.ComptimeStringMap(f128, .{
    .{ "f128", floatMin(f128) },
    .{ "f64", floatMin(f64) },
    .{ "f32", floatMin(f32) },
    .{ "f16", floatMin(f16) },
});

// Maximum values for float types.
pub const float_max = std.ComptimeStringMap(f128, .{
    .{ "f128", floatMax(f128) },
    .{ "f64", floatMax(f64) },
    .{ "f32", floatMax(f32) },
    .{ "f16", floatMax(f16) },
});

pub const NumberState = enum {
    uint,
    int,
    float,
};

pub const builtin_types = number_types ++ [_][]const u8{
    "type",
    "bool",
    "void",
    "any",
};

pub const State = enum {
    string,
    char,
    number,
    none,
};

pub const Lexer = struct {
    allocator: Allocator,

    output: Tokens,

    src: source.Source,
    pos: Position = .{},
    token_start: Position = .{}, // Stores the starting position of the next token. Only used for some tokens.

    chars: struct { u8, u8 }, // Stores the current and next characters.
    buffer: std.ArrayList(u8),
    state: State = State.none,

    logger: Logger(.lexer),

    const Self = @This();

    // Initializes a new lexer.
    pub fn init(allocator: Allocator, src: source.Source) Self {
        return .{
            .allocator = allocator,
            .output = Tokens.init(allocator),
            .src = src,
            .chars = .{
                getOptional(u8, @constCast(src.data), 0) orelse 0,
                getOptional(u8, @constCast(src.data), 1) orelse 0,
            },
            .buffer = std.ArrayList(u8).init(allocator),
            .logger = Logger(.lexer).init(allocator, &src),
        };
    }

    // De-initializes the lexer.
    // This should only be run after the output of the lexer is done being used.
    pub fn deinit(self: *Self) void {
        self.output.deinit();
        self.buffer.deinit();
    }

    // Clears the lexer retaining capacity, making it available to parse again.
    pub fn clearRetainingCapacity(self: *Self) void {
        self.output.clearRetainingCapacity();
        self.chars = .{
            getOptional(u8, @constCast(self.src.data), 0) orelse 0,
            getOptional(u8, @constCast(self.src.data), 1) orelse 0,
        };
        self.pos = .{ .raw = 0, .row = 0, .col = 0 };
        self.buffer.clearRetainingCapacity();
        self.state = State.none;
        self.token_start = .{ .raw = 0, .row = 0, .col = 0 };
    }

    // Clears the lexer and frees, making it available to parse again.
    pub fn clearAndFree(self: *Self) void {
        self.output.clearAndFree();
        self.chars = .{
            getOptional(u8, @constCast(self.src.data), 0) orelse 0,
            getOptional(u8, @constCast(self.src.data), 1) orelse 0,
        };
        self.pos = .{ .raw = 0, .row = 0, .col = 0 };
        self.buffer.clearAndFree();
        self.state = State.none;
        self.token_start = .{ .raw = 0, .row = 0, .col = 0 };
    }

    // Lexes the entire queue.
    pub fn lexFull(self: *Self) !Tokens {
        while (self.pos.raw < self.src.data.len) try self.lexNext();
        return self.output;
    }

    // Returns the last token the lexer outputted.
    pub fn getLastToken(self: *Self) ?LocatedToken {
        if (self.output.items.len == 0) return null;
        return self.output.items[self.output.items.len - 1];
    }

    // Get a character in the queue relative to the current position.
    pub inline fn getCharRelative(self: *Self, offset: isize) u8 {
        return getOptional(u8, @constCast(self.src.data), self.pos.raw + offset) orelse 0;
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

        if (Token.fromBasicString(content)) |token| {
            const start = self.getTokenStart();
            const end = self.pos.back(1);
            try self.output.append(.{ .span = .{ start, end }, .token = token });
        }

        self.buffer.clearRetainingCapacity();
    }

    // Moves the lexer position, and sets the current and next characters.
    // Skips newline characters.
    pub fn advance(self: *Self) !void {
        self.pos.raw += 1;
        self.pos.col += 1;

        var new_lines: usize = 0;

        while (self.pos.raw < self.src.data.len and self.src.data[self.pos.raw] == '\n') : (new_lines += 1) {
            if (self.state == State.string) try self.buffer.append('\n');

            if (new_lines == 0 and self.state == State.none) {
                try self.addBasicToken();
            }

            self.pos.raw += 1;
            self.pos.row += 1;
            self.pos.col = 0;
        }

        self.chars = .{
            self.getCharRelative(0),
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
                if (self.src.data.len > self.pos.raw + 3) {
                    try self.advance(); // Advance past '\'.
                    try self.advance(); // Advance past 'x'.

                    try self.buffer.append(parseUnsigned(u8, &self.chars, 16) catch ret: {
                        try self.logger.err("bad hexadecimal number.", .{}, .{ self.pos, self.pos }); // Overflow is unreachable.
                        break :ret 0;
                    });
                } else {
                    try self.logger.err("invalid escape sequence.", .{}, .{ self.pos, self.pos });
                    try self.buffer.append(0);
                }
            },
            'u' => {
                if (exit == '\'') {
                    const esc_seq_start = self.pos;
                    while (self.pos.raw < self.src.data.len and self.chars[1] != '}') {
                        try self.advance();
                    }
                    try self.logger.err("UTF-8 multi-character escape sequence used inside a character.", .{}, .{ esc_seq_start, self.pos });
                    try self.logger.err("single characters can be escaped with the '\\x__' format.", .{}, null);
                    try self.buffer.append(0);
                    return;
                }

                try self.advance(); // Advance past '\'.
                try self.advance(); // Advance past 'u'.

                if (self.chars[0] == '{') {
                    try self.advance();

                    var second = false;
                    var tmp = try std.ArrayList(u8).initCapacity(self.allocator, 2);
                    var hex = std.ArrayList(u8).init(self.allocator);
                    defer tmp.deinit();
                    defer hex.deinit();

                    while (self.pos.raw < self.src.data.len) : (try self.advance()) {
                        if (!isHexadecimalChar(self.chars[0])) {
                            try self.logger.err("invalid escape sequence.", .{}, .{ self.pos, self.pos });
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
                    _ = self.src.data[self.pos.raw .. self.pos.raw + 4];
                }
            },
            else => {
                try self.logger.err("invalid escape sequence.", .{}, .{ self.pos, self.pos });
                try self.buffer.append(0);
            },
        }
    }

    // Lex characters into the next token(s).
    pub fn lexNext(self: *Self) !void {
        if (self.pos.raw >= self.src.data.len) return;

        const initial_len = self.output.items.len;

        while (self.pos.raw < self.src.data.len and initial_len == self.output.items.len) : (try self.advance()) {
            var explicit_sign_number = (self.chars[0] == '+' or self.chars[0] == '-') and isNumberChar(self.chars[1]);

            // Explicitly +/- numbers; we must ensure the last token was not an expression.
            if (explicit_sign_number) {
                var last_token = self.getLastToken();
                if (last_token == null) try self.logger.fatal("invalid syntax.", .{}, .{ self.pos, self.pos });
                explicit_sign_number = explicit_sign_number and !last_token.?.token.isExpression();
            }

            // Number lexer.
            if (!isIdentifier(self.buffer.items) and (isNumberChar(self.chars[0]) or explicit_sign_number)) {
                self.startToken();
                self.state = State.number;

                const basic = !(self.chars[0] == '0' and containsChar(&non_dec_number_chars, self.chars[1]));
                var number_state = NumberState.uint;
                var explicit_type_char: ?u8 = null;

                if (explicit_sign_number) {
                    number_state = NumberState.int;
                    try self.buffer.append(self.chars[0]);
                    try self.advance();
                }

                while (self.pos.raw < self.src.data.len) : (try self.advance()) {
                    if (isNumberChar(self.chars[0]) or self.chars[0] == '_' or (isHexadecimalChar(self.chars[0]) and !basic)) {
                        try self.buffer.append(self.chars[0]);
                    } else if (self.chars[0] == '.') {
                        if (!basic) try self.logger.fatal("invalid number format.", .{}, .{ self.pos, self.pos });
                        if (number_state == NumberState.float) break;
                        number_state = NumberState.float;
                        try self.buffer.append(self.chars[0]);
                    } else if (containsChar(@constCast(&symbols), self.chars[0])) {
                        break;
                    } else if (self.chars[1] == '\n') {
                        try self.buffer.append(self.chars[0]);
                        try self.advance();
                        break;
                    } else if ((self.chars[0] == 'u' or self.chars[0] == 'i' or self.chars[0] == 'f') and basic) {
                        explicit_type_char = self.chars[0];
                        break;
                    } else {
                        try self.logger.fatal("invalid number format.", .{}, .{ self.pos, self.pos });
                    }
                }

                const number_data = (try self.buffer.clone()).items;
                const number_start = self.getTokenStart();
                const number_span = Span{ number_start, number_start.forward(number_data.len - 1) };

                self.startToken();
                self.buffer.clearRetainingCapacity();

                var type_string: []u8 = switch (number_state) {
                    NumberState.uint => @constCast("usize"),
                    NumberState.int => @constCast("isize"),
                    NumberState.float => @constCast("f32"),
                };

                if (explicit_type_char) |char| {
                    while (self.pos.raw < self.src.data.len) : (try self.advance()) {
                        if (isNumberChar(self.chars[0]) or
                            self.chars[0] == 'u' or
                            self.chars[0] == 'f' or
                            self.chars[0] == 's' or
                            self.chars[0] == 'i' or
                            self.chars[0] == 'z' or
                            self.chars[0] == 'e')
                        {
                            try self.buffer.append(self.chars[0]);
                        } else {
                            type_string = (try self.buffer.clone()).items;
                            self.buffer.clearRetainingCapacity();
                            break;
                        }
                    }

                    if (char == 'u') {
                        if (number_state == NumberState.int) {
                            try self.logger.err("Unsigned integers cannot be negative.", .{}, .{ self.getTokenStart(), self.pos.back(1) });
                        } else if (number_state == NumberState.float) {
                            try self.logger.err("Floats cannot become unsigned integers.", .{}, .{ self.getTokenStart(), self.pos.back(1) });
                        }
                    } else if (char == 'i' and number_state == NumberState.float) {
                        try self.logger.err("Floats cannot become signed integers.", .{}, .{ self.getTokenStart(), self.pos.back(1) });
                    } else if (char == 'f') {
                        number_state = NumberState.float;
                    }

                    if (!containsString(@constCast(&number_types), type_string)) {
                        try self.logger.fatal("Unknown type.", .{}, .{ self.getTokenStart(), self.pos.back(1) });
                    }
                }

                const number: Token = switch (number_state) {
                    NumberState.uint => uint: {
                        const max = uint_max.get(type_string).?;

                        if (parseUnsigned(u128, number_data, 0)) |number_value| {
                            if (number_value > max) try self.logger.err("number cannot fit in type {s}.", .{type_string}, number_span);
                            break :uint .{ .uint = number_value };
                        } else |_| { // InvalidCharacter unreachable.
                            try self.logger.err("number cannot fit in type {s}.", .{type_string}, number_span);
                        }
                    },
                    NumberState.int => int: {
                        const min = int_min.get(type_string).?;
                        const max = int_max.get(type_string).?;

                        if (parseInt(i128, number_data, 0)) |number_value| {
                            if (number_value < min or number_value > max) try self.logger.err("number cannot fit in type {s}.", .{type_string}, number_span);
                            break :int .{ .int = number_value };
                        } else |_| { // InvalidCharacter unreachable.
                            try self.logger.err("number cannot fit in type {s}.", .{type_string}, number_span);
                        }
                    },
                    NumberState.float => float: {
                        const min = float_min.get(type_string).?;
                        const max = float_max.get(type_string).?;

                        if (parseFloat(f128, number_data)) |number_value| {
                            if (number_value < min or number_value > max) try self.logger.err("number cannot fit in type {s}.", .{type_string}, number_span);
                            break :float .{ .float = number_value };
                        } else |_| unreachable;
                    },
                };

                try self.output.append(.{
                    .span = number_span,
                    .token = number,
                });

                if (explicit_type_char != null) try self.output.append(.{
                    .span = .{ self.getTokenStart(), self.pos.back(1) },
                    .token = Token{ .identifier = type_string },
                });

                self.state = State.none;
                self.buffer.clearRetainingCapacity();
                return;
            }

            switch (self.chars[0]) {
                '"' => { // String lexer.
                    self.startToken();
                    self.state = State.string;

                    while (self.pos.raw < self.src.data.len) : (try self.advance()) {
                        if (self.chars[1] == '"') {
                            try self.advance();
                            break;
                        } else if (self.chars[1] == '\\') {
                            try self.lexEscapeSequences('"');
                        } else {
                            try self.buffer.append(self.chars[1]);
                        }
                    }

                    try self.output.append(.{
                        .span = .{ self.getTokenStart(), self.pos },
                        .token = .{ .string = (try self.buffer.clone()).items },
                    });

                    self.state = State.none;
                    self.buffer.clearRetainingCapacity();
                },
                '\'' => { // Character lexer.
                    self.startToken();
                    self.state = State.char;
                    var char: ?u8 = null;

                    if (self.chars[1] == '\'') {
                        try self.logger.err("empty characters not allowed.", .{}, .{ self.pos, self.pos.forward(1) });
                    } else if (self.chars[1] == '\\') {
                        try self.lexEscapeSequences('\'');
                        char = self.buffer.items[0];
                    } else {
                        char = self.chars[1];
                    }

                    try self.advance();

                    if (char != null and self.chars[1] != '\'') {
                        const start = self.getTokenStart();
                        while (self.pos.raw < self.src.data.len and self.chars[1] != '\'') {
                            try self.advance();
                        }
                        try self.logger.err("character is too large.", .{}, .{ start, self.pos });
                        try self.logger.info("strings are defined with double quotes in tungsten.", .{}, null);
                    }

                    try self.advance();

                    try self.output.append(.{
                        .span = .{ self.getTokenStart(), self.pos },
                        .token = .{ .char = char orelse 0 },
                    });

                    self.state = State.none;
                    self.buffer.clearRetainingCapacity();
                },
                '/' => switch (self.chars[1]) { // Division or comment.
                    '/' => while (self.chars[1] != '\n') { // Single-line comment.
                        try self.advance();
                    },
                    '*' => { // Multi-line comment.
                        while (!std.mem.eql(u8, &self.chars, "*/")) {
                            try self.advance();
                        }
                        try self.advance();
                    },
                    else => { // Symbol.
                        try self.addBasicToken();
                        try self.output.append(.{
                            .span = .{ self.pos, self.pos },
                            .token = .{ .symbol = '/' },
                        });
                    },
                },
                ' ' => try self.addBasicToken(), // Separator; like a symbol but not stored.
                else => if (!containsChar(&symbols, self.chars[0])) {
                    try self.buffer.append(self.chars[0]);
                } else { // Symbol.
                    try self.addBasicToken();
                    try self.output.append(.{
                        .span = .{ self.pos, self.pos },
                        .token = .{ .symbol = self.chars[0] },
                    });
                },
            }
        }
    }
};
