const std = @import("std");
const log = @import("../log.zig");
const util = @import("util.zig");
const source = @import("source.zig");
const utftools = @import("utftools");

const Allocator = std.mem.Allocator;

const Logger = @import("../log.zig").Logger;
const value = @import("../vm/value.zig");
const NumberType = value.BuiltinValue.NumberType;
const Number = value.BuiltinValue.Number;

const allocPrint = std.fmt.allocPrint;
const parseUnsigned = std.fmt.parseUnsigned;
const parseInt = std.fmt.parseInt;
const parseFloat = std.fmt.parseFloat;

const getOptional = util.getOptional;
const isNumberChar = util.isNumberChar;
const isHexadecimalChar = util.isHexadecimalChar;
const containsString = util.containsString;

const Position = source.Position;
const Span = source.Span;

// A lexer token.
pub const Token = union(enum) {
    // values
    null,
    undefined,
    bool: bool,
    char: u21,
    string: []const u8,
    number: Number,

    // other
    symbol: u8,
    keyword: []const u8,
    builtin_type: []const u8,
    identifier: []const u8,

    // Convert from a string representing a basic token (if possible).
    pub fn fromBasicString(string: []const u8) ?Token {
        if (std.mem.eql(u8, string, "null")) {
            return .null;
        } else if (std.mem.eql(u8, string, "undefined")) {
            return .undefined;
        } else if (std.mem.eql(u8, string, "true")) {
            return .{ .bool = true };
        } else if (std.mem.eql(u8, string, "false")) {
            return .{ .bool = false };
        } else if (isIdentifier(string)) {
            if (containsString(&keywords, string)) {
                return .{ .keyword = string };
            } else if (value.BuiltinType.string_map.has(string)) {
                return .{ .builtin_type = string };
            } else {
                return .{ .identifier = string };
            }
        }

        return null;
    }

    // Write a token to a writer.
    pub fn write(self: Token, writer: anytype) !void {
        return switch (self) {
            .null => try writer.print("null", .{}),
            .undefined => try writer.print("undefined", .{}),
            .bool => |v| try writer.print("bool: {}", .{v}),
            .char => |v| {
                try writer.writeAll("char: '");
                try utftools.writeCodepointToUtf8(v, writer);
                try writer.writeByte('\'');
            },
            .string => |v| try writer.print("string: \"{s}\"", .{v}),
            .number => |v| {
                try writer.writeAll("number: { ");
                try v.write(writer);
                try writer.writeAll(" }");
            },

            .symbol => |v| try writer.print("symbol: {c}", .{v}),
            .identifier => |v| try writer.print("identifier: {s}", .{v}),
            .builtin_type => |v| try writer.print("built-in type: {s}", .{v}),
            .keyword => |v| try writer.print("keyword: {s}", .{v}),
        };
    }

    // Convert a token to string representation.
    pub inline fn toString(self: Token, allocator: Allocator) ![]const u8 {
        var arraylist = std.ArrayList(u8).init(allocator);
        try self.write(arraylist.writer());
        return arraylist.toOwnedSlice();
    }

    // Is the token the end of an expression or an expression itself?
    pub fn isExpression(self: Token) bool {
        return switch (self) {
            .symbol => |symbol| switch (symbol) {
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
inline fn isNonDecNumberChar(char: u8) bool {
    return switch (char) {
        'x',
        'X', // hexadecimal
        'b',
        'B', // binary
        'o',
        'O', // octal
        'e',
        'E', // exponent
        => true,
        else => false,
    };
}

// All valid separating symbols.
inline fn isSeparatingSymbol(char: u8) bool {
    return switch (char) {
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
        => true,
        else => false,
    };
}

// All valid symbols.
inline fn isSymbol(char: u8) bool {
    return char == '.' or isSeparatingSymbol(char);
}

// Check if "content" (string with no spaces) is a valid identifier.
inline fn isIdentifier(content: []const u8) bool {
    return content.len > 0 and !isNumberChar(content[0]);
}

// zig fmt: off

// All valid keywords.
pub const keywords = [25][]const u8{
    "as", // casting
    
    "throw",  // raise error
    "try",    // try function which raises errors
    "catch",  // catch error
    "assert", // raise error if condition not true
              // in a function which doesn't throw errors, this will crash the program!
    
    "if",     // define conditional if statement
    "else",   // define conditional else statement
    "switch", // define switch statement
    
    "pub",   // publicize declaration
    "const", // declare constant variable
    "var",   // declare mutable variable
    
    "func",   // define function
    "return", // return from function
    "ref",    // get reference to param instead of copying
    
    "struct",    // define struct
    "enum",      // define enum
    "union",     // define union
    "interface", // define interface
    
    "loop",     // define infinite loop
    "while",    // define while loop
    "for",      // define for loop
    "break",    // loop break
    "continue", // loop continue

    "mod", // define a module
    "use", // use a module
};

// zig fmt: on

pub const State = enum { string, char, number, none };

// the actual lexer

allocator: Allocator,

output: Tokens,

src: source.Source,
pos: Position = .{},
last: Position = .{},
start: Position = .{},

chars: [2]u8,
basic: []const u8 = "",
buffer: std.ArrayList(u8),
state: State = .none,

logger: Logger(.lexer),
failed: bool = false,

const Self = @This();

// Initializes a new lexer.
pub fn init(allocator: Allocator, src: source.Source) Self {
    return .{
        .allocator = allocator,
        .output = Tokens.init(allocator),
        .src = src,
        .chars = .{
            getOptional(u8, src.data, 0) orelse 0,
            getOptional(u8, src.data, 1) orelse 0,
        },
        .buffer = std.ArrayList(u8).init(allocator),
        .logger = Logger(.lexer).init(allocator, src),
    };
}

// De-initializes the lexer.
// This should only be run after the output of the lexer is done being used.
pub fn deinit(self: *Self) void {
    self.output.deinit();
    self.buffer.deinit();
    self.logger.deinit();
}

// Clears the lexer, making it available to parse again.
pub fn clear(self: *Self, free: bool) void {
    if (free) {
        self.output.clearAndFree();
        self.buffer.clearAndFree();
    } else {
        self.output.clearRetainingCapacity();
        self.buffer.clearRetainingCapacity();
    }

    self.state = .none;
    self.chars = .{
        getOptional(u8, self.src.data, 0) orelse 0,
        getOptional(u8, self.src.data, 1) orelse 0,
    };
    self.pos = .{ .raw = 0, .row = 0, .col = 0 };
    self.start = .{ .raw = 0, .row = 0, .col = 0 };
}

// Lexes the entire queue.
pub fn lexFull(self: *Self) !Tokens {
    while (self.pos.raw < self.src.data.len and !self.failed) try self.lexNext();
    return self.output;
}

// Returns the last token the lexer outputted.
pub fn getLastToken(self: *Self) ?LocatedToken {
    if (self.output.items.len == 0) return null;
    return self.output.items[self.output.items.len - 1];
}

// Adds the current basic token to the output.
pub fn addBasicToken(self: *Self, comptime symbol: bool) !void {
    if (self.basic.len != 0) {
        self.basic.ptr = @ptrCast(&self.src.data[self.start.raw]);

        if (Token.fromBasicString(self.basic)) |token| {
            try self.output.append(.{ .span = .{ self.start, self.last }, .token = token });
        }

        self.basic.len = 0;
    }

    if (symbol) try self.output.append(.{
        .span = .{ self.pos, self.pos },
        .token = .{ .symbol = self.chars[0] },
    });
}

// Moves the lexer position, and sets the current and next characters.
// Skips newline characters.
pub fn advance(self: *Self) !void {
    return self.advanceInner(1, false);
}

fn advanceSpecial(self: *Self, n: usize, comptime cp: bool) !void {
    return self.advanceInner(n, cp);
}

inline fn advanceInner(self: *Self, n: usize, comptime cp: bool) !void {
    for (n) |_| {
        self.last = self.pos;
        self.pos.raw += 1;
        self.pos.col += 1;

        if (!cp) {
            var new_lines: usize = 0;

            while (self.pos.raw < self.src.data.len and self.src.data[self.pos.raw] == '\n') : (new_lines += 1) {
                if (self.state == .string) try self.buffer.append('\n');

                if (new_lines == 0 and self.state == .none) {
                    try self.addBasicToken(false);
                }

                self.pos.raw += 1;
                self.pos.row += 1;
                self.pos.col = 0;
            }
        } else if (self.src.data[self.pos.raw] == '\n') return error.NewLineInCodepointAdvance;
    }

    self.chars = .{
        getOptional(u8, self.src.data, self.pos.raw) orelse 0,
        getOptional(u8, self.src.data, self.pos.raw + 1) orelse 0,
    };
}

// Lex string and character escape sequences.
pub fn parseEscapeSequence(self: *Self, comptime exit: u8) !u21 {
    const esc_seq_start = self.pos;

    return switch (self.chars[1]) {
        'n', 'r', 't', '\\', exit => {
            const char = self.chars[1];
            try self.advance();
            return switch (char) {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                exit => exit,
                else => unreachable,
            };
        },
        'x' => {
            if (self.src.data.len > self.pos.raw + 3) {
                try self.advance(); // Advance past '\'.
                try self.advance(); // Advance past 'x'.
                const chars = self.chars;
                try self.advance();

                return parseUnsigned(u8, &chars, 16) catch {
                    try self.logger.err("bad hexadecimal number.", .{}, .{ esc_seq_start, self.pos }); // Overflow is unreachable.
                    return 0xFFFD;
                };
            } else {
                try self.logger.err("invalid escape sequence.", .{}, .{ self.pos, self.pos });
                return 0xFFFD;
            }
        },
        'u' => {
            try self.advance(); // Advance past '\'.
            try self.advance(); // Advance past 'u'.

            if (self.chars[0] == '{') parse: {
                try self.advance(); // Advance past '{'.

                const esc_char_start = self.pos;
                var idx: usize = 5;

                while (self.pos.raw < self.src.data.len) : (try self.advance()) {
                    if (self.chars[0] == '}') break;
                    if (!isHexadecimalChar(self.chars[0]) or self.chars[1] == '\n') break :parse;
                    if (idx == 0) {
                        while (self.pos.raw < self.src.data.len and self.chars[0] != '}') try self.advance();
                        try self.logger.err("escape sequence is too large.", .{}, .{ esc_seq_start, self.pos });
                        return 0xFFFD;
                    }
                    idx -%= 1;
                }

                const char = parseUnsigned(u24, self.src.data[esc_char_start.raw..self.pos.raw], 16) catch unreachable;
                if (char > 0x10FFFF) {
                    try self.logger.err("escape sequence is too large.", .{}, .{ esc_seq_start, self.pos });
                    return 0xFFFD;
                } else return @truncate(char);
            }

            while (self.pos.raw < self.src.data.len and isHexadecimalChar(self.chars[1])) try self.advance();
            try self.advance();
            try self.logger.err("invalid escape sequence.", .{}, .{ esc_seq_start, self.pos });
            return 0xFFFD;
        },
        else => {
            try self.logger.err("invalid escape sequence.", .{}, .{ self.pos, self.pos });
            return 0xFFFD;
        },
    };
}

inline fn fatal(self: *Self, comptime fmt: []const u8, args: anytype, span: ?Span) !void {
    try self.logger.err(fmt, args, span);
    self.failed = true;
}

// Lex characters into the next token(s).
pub fn lexNext(self: *Self) !void {
    if (self.pos.raw >= self.src.data.len or self.failed) return;

    const initial_len = self.output.items.len;

    while (self.pos.raw < self.src.data.len and initial_len == self.output.items.len) : (try self.advance()) {
        if (self.basic.len == 0) self.start = self.pos;
        var explicit_sign_number = (self.chars[0] == '+' or self.chars[0] == '-') and isNumberChar(self.chars[1]);

        // Explicitly +/- numbers; we must ensure the last token was not an expression.
        if (explicit_sign_number) {
            var last_token = self.getLastToken();
            if (last_token == null) return self.fatal("invalid syntax.", .{}, .{ self.pos, self.pos });
            explicit_sign_number = explicit_sign_number and !last_token.?.token.isExpression();
        }

        // Number lexer.
        if (!isIdentifier(self.basic) and (isNumberChar(self.chars[0]) or explicit_sign_number)) {
            var number_span = Span{ self.pos, self.pos };
            self.state = .number;

            const decimal = !(self.chars[0] == '0' and isNonDecNumberChar(self.chars[1]));
            var number_type = NumberType.u64;
            var explicit_type_char: ?u8 = null;

            if (explicit_sign_number) {
                number_type = NumberType.i64;
                try self.advance();
            }

            while (self.pos.raw < self.src.data.len) : (try self.advance()) {
                number_span[1] = self.pos;
                // TODO: use labeled switch
                switch (self.chars[0]) {
                    '0'...'9', '_' => {},
                    'u', 'i', 'f' => if (decimal) {
                        explicit_type_char = self.chars[0];
                        break;
                    } else if (self.chars[0] != 'f') return self.fatal("invalid number format.", .{}, number_span),
                    'a'...'d', 'A'...'F' => if (decimal) return self.fatal("invalid number format.", .{}, number_span),
                    '.' => {
                        if (!decimal) return self.fatal("invalid number format.", .{}, number_span);
                        if (number_type == NumberType.f64) break;
                        number_type = NumberType.f64;
                    },
                    else => return self.fatal("invalid number format.", .{}, number_span),
                }

                if (isSeparatingSymbol(self.chars[1]) or self.chars[1] == '\n') {
                    try self.advance();
                    break;
                }
            }

            const number_data = self.src.data[number_span[0].raw..self.pos.raw];

            var explicit_type_span = Span{ self.pos, self.pos };
            var type_string: []const u8 = @tagName(number_type);

            var err = false;
            if (explicit_type_char) |char| get_type: {
                while (self.pos.raw < self.src.data.len) {
                    if (isSymbol(self.chars[0]) or self.chars[0] == ' ') break;
                    explicit_type_span[1] = self.pos;
                    try self.advance();
                }

                type_string = self.src.data[explicit_type_span[0].raw..self.pos.raw];

                var new_type: ?NumberType = null;
                if (type_string.len > 1) {
                    new_type = NumberType.string_map.get(type_string);
                    if (new_type == null) {
                        try self.logger.err("unknown type.", .{}, explicit_type_span);
                        err = true;
                        break :get_type;
                    }
                } else new_type = switch (char) {
                    'i' => .i64,
                    'u' => .u64,
                    'f' => .f64,
                    else => unreachable,
                };

                if (char == 'u') {
                    if (number_type == .i64 and number_data[0] == '-') {
                        try self.logger.err("unsigned integers cannot be negative.", .{}, explicit_type_span);
                        err = true;
                    } else if (number_type == .f64) {
                        try self.logger.err("floating-point numbers cannot become unsigned integers.", .{}, explicit_type_span);
                        err = true;
                    }
                } else if (char == 'i' and number_type == .f64) {
                    try self.logger.err("floating-point numbers cannot become signed integers.", .{}, explicit_type_span);
                    err = true;
                }

                number_type = new_type.?;
                number_span[1] = explicit_type_span[1];
            }

            try self.output.append(.{
                .span = number_span,
                .token = .{ .number = Number.parse(number_type, number_data, 0) catch num: {
                    try self.logger.err("number cannot fit in {s}.", .{type_string}, number_span);
                    break :num Number{ .u64 = 0 };
                } },
            });

            self.state = .none;
            return;
        }

        switch (self.chars[0]) {
            '"' => { // String lexer.
                self.state = .string;
                const start = self.pos;
                try self.advance();
                var last = self.pos.raw;

                var out: []const u8 = "";

                while (self.pos.raw < self.src.data.len) : (try self.advance()) {
                    if (self.chars[0] == '"') {
                        out = if (last == start.raw) self.src.data[last..self.pos.raw] else esc: {
                            try self.buffer.appendSlice(self.src.data[last..self.pos.raw]);
                            break :esc try self.buffer.toOwnedSlice();
                        };
                        break;
                    } else if (self.chars[0] == '\\') {
                        try self.buffer.appendSlice(self.src.data[last..self.pos.raw]);
                        const c = try self.parseEscapeSequence('"');
                        try utftools.writeCodepointToUtf8(c, self.buffer.writer());
                        last = self.pos.raw + 1;
                    }
                }

                try self.output.append(.{
                    .span = .{ start, self.pos },
                    .token = .{ .string = out },
                });

                self.state = .none;
            },
            '\'' => { // Character lexer.
                self.state = .char;
                const start = self.pos;
                var char: ?u21 = null;

                if (self.chars[1] == '\'') {
                    try self.advance();
                    try self.logger.err("empty characters not allowed.", .{}, .{ start, self.pos });
                } else if (self.chars[1] == '\\') {
                    try self.advance();
                    char = try self.parseEscapeSequence('\'');
                    try self.advance();
                } else {
                    while (self.pos.raw < self.src.data.len and self.chars[1] != '\'') try self.advance();
                    const end = self.pos;
                    try self.advance();

                    var err = false;
                    const c = utftools.codepointFromUtf8(self.src.data[start.raw + 1 .. self.pos.raw]) catch blk: {
                        try self.logger.err("invalid character.", .{}, .{ start, end });
                        err = true;
                        break :blk .{ 0xFFFD, 3 };
                    };

                    if (!err) if (c[1] < self.buffer.items.len) {
                        try self.logger.err("character is too large.", .{}, .{ start, end });
                        try self.logger.info("strings are defined with double quotes in tungsten.", .{}, null);
                    };

                    char = c[0];
                    self.buffer.clearRetainingCapacity();
                }

                try self.output.append(.{
                    .span = .{ start, self.pos },
                    .token = .{ .char = char orelse 0xFFFD },
                });

                self.state = .none;
            },
            // Division or comment.
            '/' => switch (self.chars[1]) {
                // Single-line comment.
                '/' => while (self.chars[1] != '\n') try self.advance(),
                // Multi-line comment.
                '*' => {
                    while (!(self.chars[0] == '*' and self.chars[1] == '/')) try self.advance();
                    try self.advance();
                },
                // Symbol.
                else => try self.addBasicToken(true),
            },
            ' ' => try self.addBasicToken(false),
            else => if (!isSymbol(self.chars[0])) {
                const char = utftools.codepointFromUtf8(self.src.data[self.pos.raw..]) catch {
                    return self.fatal("invalid unicode sequence in source code.", .{}, .{});
                };
                const size = char[1];
                self.basic.len +%= size;
                if (size != 1) self.advanceSpecial(size - 1, true) catch |e| {
                    if (e != error.NewLineInCodepointAdvance) return e;
                    return self.fatal("invalid unicode sequence in source code.", .{}, .{});
                };
            } else try self.addBasicToken(true), // Symbol.
        }
    }
}
