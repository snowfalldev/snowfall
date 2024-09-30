const std = @import("std");
const log = @import("../log.zig");
const util = @import("util.zig");
const source = @import("source.zig");

const Allocator = std.mem.Allocator;

const Logger = @import("../log.zig").Logger;
const value = @import("../vm/value.zig");
const NumberType = value.NumberType;
const Number = value.Number;

const utftools = @import("utftools");
const code_point = @import("code_point");
const CodePoint = code_point.CodePoint;
const GenCatData = @import("GenCatData");

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
    string: String,
    number: Number,

    // other
    symbol: u21,
    keyword: []const u8,
    builtin_type: []const u8,
    identifier: []const u8,

    pub const String = struct {
        buf: []const u8 = "",
        managed: bool = true,
    };

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
        } else if (string.len > 0 and !isNumberChar(string[0])) {
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
            .string => |v| {
                try writer.print("string: \"{s}\"", .{v.buf});
                if (v.managed) try writer.print(" (managed)", .{});
            },
            .number => |v| {
                try writer.writeAll("number: { ");
                try v.write(true, writer);
                try writer.writeAll(" }");
            },

            .symbol => |v| {
                try writer.writeAll("symbol: ");
                try utftools.writeCodepointToUtf8(v, writer);
            },
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

pub fn deinitTokens(tokens: []LocatedToken, allocator: Allocator) void {
    for (tokens) |token|
        if (token.token == .string and token.token.string.managed)
            allocator.free(token.token.string.buf);

    allocator.free(tokens);
}

// Non-decimal representation characters in numbers.
inline fn numberBaseCharToBase(char: u21) ?u8 {
    return switch (char) {
        'b', 'B' => 2,
        'o', 'O' => 8,
        'x', 'X' => 16,
        else => null,
    };
}

// All valid separating symbols.
inline fn isSeparatingSymbol(char: u21) bool {
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
inline fn isSymbol(char: u21) bool {
    return char == '.' or isSeparatingSymbol(char);
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

iter: code_point.Iterator,
gcd: GenCatData,
current: CodePoint,
next: ?CodePoint = null,
basic: []const u8 = "",
state: State = .none,

logger: Logger(.lexer),
failed: bool = false,

const Self = @This();

// Initializes a new lexer.
pub fn init(allocator: Allocator, src: source.Source) !Self {
    var iter = code_point.Iterator{ .bytes = src.data };
    const current = iter.next();
    return .{
        .allocator = allocator,
        .output = Tokens.init(allocator),
        .src = src,
        .iter = iter,
        .gcd = try GenCatData.init(allocator),
        .current = current orelse undefined,
        .logger = Logger(.lexer).init(allocator, src),
    };
}

// De-initializes the lexer.
// This should only be run after the output of the lexer is done being used.
pub fn deinit(self: *Self) void {
    self.gcd.deinit();
    self.output.deinit();
    self.logger.deinit();
}

// Clears the lexer, making it available to parse again.
pub fn clear(self: *Self, free: bool) void {
    if (free) self.output.clearAndFree() else self.output.clearRetainingCapacity();

    self.pos = .{};
    self.last = .{};
    self.start = .{};

    self.iter.i = 0;
    self.current = self.iter.next() orelse undefined;
    self.next = null;
    self.state = .none;
}

// Lexes the entire queue.
pub inline fn lexFull(self: *Self) ![]LocatedToken {
    while (!self.finished()) try self.lexNext();
    return self.output.toOwnedSlice();
}

// Returns the last token the lexer outputted.
pub inline fn getLastToken(self: *Self) ?LocatedToken {
    if (self.output.items.len == 0) return null;
    return self.output.items[self.output.items.len - 1];
}

// Adds the current basic token to the output.
pub fn addBasicToken(self: *Self, comptime symbol: bool) !void {
    if (self.basic.len != 0) {
        self.basic.ptr = @ptrCast(&self.src.data[self.start.raw]);

        if (Token.fromBasicString(self.basic)) |token|
            try self.output.append(.{ .span = .{ self.start, self.last }, .token = token });

        self.basic.len = 0;
    }

    if (symbol) try self.output.append(.{
        .span = .{ self.pos, self.pos },
        .token = .{ .symbol = self.current.code },
    });
}

// Moves the lexer position, and sets the current and next characters.
// Skips separator characters.
pub fn advance(self: *Self) !void {
    self.next = null;
    self.current = self.iter.next() orelse {
        self.pos.raw += 1;
        self.pos.col += 1;
        return;
    };

    self.last = self.pos;
    self.pos.raw = self.current.offset;
    self.pos.col += self.current.len;

    var separators: u32 = 0;
    var separators_this_line: u32 = 0;
    var new_lines: u32 = 0;

    while (self.isSeparator(self.current.code)) : (separators += self.current.len) {
        separators_this_line += self.current.len;

        if (self.current.code == '\n') {
            new_lines += 1;
            separators_this_line = 0;
        }

        if (separators == 0 and self.state == .none) try self.addBasicToken(false);

        self.current = self.iter.next() orelse undefined;
    }

    if (separators != 0) {
        self.pos.raw += separators;
        self.pos.col += separators;
        if (new_lines != 0) {
            self.pos.row += new_lines;
            self.pos.col = separators_this_line;
        }
        self.start = self.pos;
    }
}

pub inline fn peek(self: *Self) CodePoint {
    if (self.next == null) self.next = self.iter.peek();
    return self.next orelse undefined;
}

pub inline fn finished(self: Self) bool {
    return (self.pos.raw >= self.src.data.len or self.failed);
}

inline fn isSeparator(self: *Self, char: u21) bool {
    return char == '\n' or (self.state == .none and self.gcd.isSeparator(self.current.code));
}

inline fn parseEscapeSequence(self: *Self, comptime exit: u8) !u21 {
    const esc_seq_start = self.pos;

    return switch (self.peek().code) {
        'n', 'r', 't', '\\', exit => {
            const char = self.peek().code;
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
                const chars: [2]u8 = .{
                    @truncate(self.current.code),
                    @truncate(self.peek().code),
                };
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

            if (self.current.code == '{') parse: {
                try self.advance(); // Advance past '{'.

                const esc_char_start = self.pos;
                var idx: usize = 5;

                while (!self.finished()) : (try self.advance()) {
                    if (self.current.code == '}') break;
                    if (!isHexadecimalChar(self.current.code) or self.isSeparator(self.peek().code)) break :parse;
                    if (idx == 0) {
                        while (!self.finished() and self.current.code != '}') try self.advance();
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

            while (!self.finished() and isHexadecimalChar(self.peek().code)) try self.advance();
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

inline fn parseNumber(self: *Self, explicit_sign_number: bool) !void {
    var number_span = Span{ self.pos, self.pos };
    self.state = .number;

    const obase = numberBaseCharToBase(self.peek().code);
    const base = if (self.current.code == '0') obase orelse 10 else 10;
    var number_type = NumberType.u64;
    var exponent_reached = false;
    var explicit_type_char: ?CodePoint = null;

    if (explicit_sign_number) {
        number_type = NumberType.i64;
        try self.advance();
    }

    while (!self.finished()) : (try self.advance()) {
        number_span[1] = self.pos;
        // TODO: use labeled switch
        switch (self.current.code) {
            '0'...'9', '_' => {},
            'n', 'i', 'u' => {
                explicit_type_char = self.current;
                break;
            },
            'f' => if (base == 10) {
                explicit_type_char = self.current;
                break;
            } else return self.fatal("invalid number format.", .{}, number_span),
            'a'...'d', 'A'...'D', 'F' => if (base != 16) return self.fatal("hex char in base {} number.", .{base}, number_span),
            'e', 'E' => switch (base) {
                16 => {},
                10 => if (number_type == .f64) {
                    if (!exponent_reached) {
                        exponent_reached = true;
                    } else return self.fatal("hex char in float (past possible exponent).", .{}, number_span);
                } else return self.fatal("hex char in base 10 number (not known to be float yet).", .{}, number_span),
                else => return self.fatal("hex char in base {} number.", .{base}, number_span),
            },
            '.' => {
                if (number_type == NumberType.f64 or base != 10) break;
                number_type = NumberType.f64;
            },
            else => return self.fatal("invalid number format.", .{}, number_span),
        }

        if (isSeparatingSymbol(self.peek().code) or self.isSeparator(self.peek().code)) {
            try self.advance();
            break;
        }
    }

    const number_data = self.src.data[number_span[0].raw..self.pos.raw];

    var explicit_type_span = Span{ self.pos, self.pos };
    var type_string: []const u8 = @tagName(number_type);

    var err = false;
    if (explicit_type_char) |char| get_type: {
        while (!self.finished()) {
            if (isSymbol(self.current.code) or self.isSeparator(self.current.code)) break;
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
        } else new_type = switch (char.code) {
            'n' => .bigint,
            'i' => .i64,
            'u' => .u64,
            'f' => .f64,
            else => unreachable,
        };

        if (char.code == 'u') {
            if (number_type == .i64 and number_data[0] == '-') {
                try self.logger.err("unsigned integers cannot be negative.", .{}, explicit_type_span);
                err = true;
            } else if (number_type == .f64) {
                try self.logger.err("floating-point numbers cannot become unsigned integers.", .{}, explicit_type_span);
                err = true;
            }
        } else if (char.code == 'i' and number_type == .f64) {
            try self.logger.err("floating-point numbers cannot become signed integers.", .{}, explicit_type_span);
            err = true;
        }

        number_type = new_type.?;
        number_span[1] = explicit_type_span[1];
    }

    const number = if (!err) Number.parse(number_type, number_data, base, self.allocator) catch num: {
        try self.logger.err("number cannot fit in {s}.", .{type_string}, number_span);
        break :num Number{ .u64 = 0 };
    } else Number{ .u64 = 0 };

    try self.output.append(.{
        .span = number_span,
        .token = .{ .number = number },
    });

    self.state = .none;
}

inline fn fatal(self: *Self, comptime fmt: []const u8, args: anytype, span: ?Span) !void {
    try self.logger.err(fmt, args, span);
    self.failed = true;
}

// Lex characters into the next token(s).
pub fn lexNext(self: *Self) !void {
    if (self.finished()) return;

    const initial_len = self.output.items.len;

    while (!self.finished() and initial_len == self.output.items.len) : (try self.advance()) {
        if (self.basic.len == 0) self.start = self.pos;
        var explicit_sign_number = (self.current.code == '+' or self.current.code == '-') and isNumberChar(self.peek().code);

        // Explicitly +/- numbers; we must ensure the last token was not an expression.
        if (explicit_sign_number) {
            var last_token = self.getLastToken();
            if (last_token == null) return self.fatal("invalid syntax.", .{}, .{ self.pos, self.pos });
            explicit_sign_number = explicit_sign_number and !last_token.?.token.isExpression();
        }

        if (self.basic.len == 0 and (isNumberChar(self.current.code) or explicit_sign_number))
            return self.parseNumber(explicit_sign_number);

        switch (self.current.code) {
            '"' => { // String parser.
                self.state = .string;
                const start = self.pos;
                try self.advance();
                const inner_start = self.pos.raw;
                var last = self.pos.raw;

                var buffer = std.ArrayList(u8).init(self.allocator);
                var out = Token.String{};

                while (!self.finished()) : (try self.advance()) {
                    if (self.current.code == '"') {
                        if (last == inner_start) {
                            out.buf = self.src.data[last..self.pos.raw];
                            out.managed = false;
                        } else {
                            try buffer.appendSlice(self.src.data[last..self.pos.raw]);
                            out.buf = try buffer.toOwnedSlice();
                        }
                        break;
                    } else if (self.current.code == '\\') {
                        try buffer.appendSlice(self.src.data[last..self.pos.raw]);
                        const c = try self.parseEscapeSequence('"');
                        try utftools.writeCodepointToUtf8(c, buffer.writer());
                        last = self.peek().offset;
                    }
                }

                try self.output.append(.{
                    .span = .{ start, self.pos },
                    .token = .{ .string = out },
                });

                self.state = .none;
            },
            '\'' => { // Character parser.
                self.state = .char;
                const start = self.pos;
                var char: ?u21 = null;

                try self.advance();
                if (self.current.code == '\'') {
                    try self.logger.err("empty characters not allowed.", .{}, .{ start, self.pos });
                } else if (self.current.code == '\\') {
                    char = try self.parseEscapeSequence('\'');
                    try self.advance();
                } else if (self.peek().code == '\'') {
                    char = self.current.code;
                    try self.advance();
                }

                if (self.current.code != '\'') {
                    while (!self.finished() and self.current.code != '\'') try self.advance();
                    try self.logger.err("character is too large.", .{}, .{ start, self.pos });
                    try self.logger.info("strings are defined with double quotes in tungsten.", .{}, null);
                }

                try self.output.append(.{
                    .span = .{ start, self.pos },
                    .token = .{ .char = char orelse 0xFFFD },
                });

                self.state = .none;
            },
            // Division or comment.
            '/' => switch (self.peek().code) {
                // Single-line comment.
                '/' => while (self.peek().code != '\n') try self.advance(),
                // Multi-line comment.
                '*' => {
                    while (!(self.current.code == '*' and self.peek().code == '/')) try self.advance();
                    try self.advance();
                },
                // Symbol.
                else => try self.addBasicToken(true),
            },
            else => if (!isSymbol(self.current.code)) {
                self.basic.len +%= self.current.len;
            } else try self.addBasicToken(true), // Symbol.
        }
    }
}
