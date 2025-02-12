const std = @import("std");
const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;
const parseUnsigned = std.fmt.parseUnsigned;
const parseInt = std.fmt.parseInt;
const parseFloat = std.fmt.parseFloat;

const grapheme = @import("grapheme");

const Script = @import("../Script.zig");
const Logger = @import("../log.zig").Logger;
const NumberType = @import("../interp/value/number.zig").NumberType;
const BuiltinType = @import("../interp/type.zig").BuiltinType;

const util = @import("../util.zig");

const unicode = @import("../unicode.zig");
const isNewLine = unicode.isNewLine;
const isNumberChar = unicode.isNumber;
const isHexadecimalChar = unicode.isHexadecimal;

const ast = @import("../ast.zig");
const Pos = ast.Pos;
const Span = ast.Span;

// A lexer token.
pub const Token = union(enum) {
    // values
    null,
    undefined,
    bool: bool,
    char: u21,
    number: Number,
    string: String,

    // other
    symbol: u8,
    keyword: Keyword,
    builtin_type: BuiltinType,
    identifier: []const u8,
    doc_comment: DocComment,

    pub const Number = struct {
        buf: []const u8 = "",
        typ: ?NumberType = null,
        base: u8 = 10,
    };

    pub const String = struct {
        data: ast.String = .{},
        formatting: ?struct {
            end: bool = false,
        } = null,
    };

    // zig fmt: off
    pub const Keyword = enum {
        as, // casting

        @"and", @"or", // boolean operations

        throw,    // raise error
        @"try",   // try function which raises errors
        @"catch", // catch error
        assert,   // raise error if condition not true
                  // in a function which doesn't throw errors, this will crash the program!

        @"if",     // define conditional if statement
        @"else",   // define conditional else statement
        @"switch", // define switch statement

        @"pub",   // publicize declaration
        @"const", // declare constant variable
        @"var",   // declare mutable variable

        func,      // define function
        @"return", // return from function
        @"defer",  // execute statement when function returns
        ref,       // get reference to param instead of copying

        @"struct", // define struct
        @"enum",   // define enum
        @"union",  // define union
        interface, // define interface

        loop,        // define infinite loop
        @"while",    // define while loop
        @"for",      // define for loop
        @"break",    // loop break
        @"continue", // loop continue

        import, // import a script

        pub const string_map = util.mkStringMap(Keyword);
    };
    // zig fmt: on

    pub const DocComment = struct {
        text: []const u8 = "",
        top_level: bool = false,
    };

    // Convert from a word representing a token (if possible).
    pub fn fromWord(string: []const u8) ?Token {
        if (std.mem.eql(u8, string, "null")) return .null;
        if (std.mem.eql(u8, string, "undefined")) return .undefined;
        if (std.mem.eql(u8, string, "true")) return .{ .bool = true };
        if (std.mem.eql(u8, string, "false")) return .{ .bool = false };
        if (string.len > 0 and !isNumberChar(string[0])) {
            if (Keyword.string_map.get(string)) |kw| return .{ .keyword = kw };
            if (BuiltinType.string_map.get(string)) |t| return .{ .builtin_type = t };
            return .{ .identifier = string };
        } else return null;
    }

    // Write a token to a writer. Intended for debugging.
    pub fn writeDebug(self: Token, writer: anytype) !void {
        return switch (self) {
            .null => try writer.writeAll("null"),
            .undefined => try writer.writeAll("undefined"),
            .bool => |v| try writer.print("bool: {}", .{v}),
            .char => |v| {
                try writer.writeAll("char: '");
                try unicode.writeCodepointToUtf8(v, writer);
                try writer.writeByte('\'');
            },
            .number => |v| {
                try writer.print("number: {s} (base {})", .{ v.buf, v.base });
                if (v.typ) |t| try writer.print(" ({s})", .{@tagName(t)});
            },
            .string => |v| {
                try writer.print("string: \"{s}\"", .{v.data.text});
                if (v.data.managed) try writer.writeAll(" (managed)");
                if (v.formatting) |formatting| {
                    try writer.writeAll(" (");
                    if (formatting.end) try writer.writeAll("last ");
                    try writer.writeAll("format segment)");
                }
            },

            .symbol => |v| {
                try writer.writeAll("symbol: ");
                try writer.writeByte(v);
            },
            .keyword => |v| try writer.print("keyword: {s}", .{@tagName(v)}),
            .builtin_type => |v| try writer.print("built-in type: {s}", .{@tagName(v)}),
            .identifier => |v| try writer.print("identifier: {s}", .{v}),
            .doc_comment => |v| {
                try writer.print("doc comment: \"{s}\"", .{v.text});
                if (v.top_level) try writer.writeAll(" (top-level)");
            },
        };
    }

    // Convert a token to string representation. Intended for debugging.
    pub inline fn toDebugString(self: Token, allocator: Allocator) ![]const u8 {
        var arraylist = std.ArrayList(u8).init(allocator);
        try self.writeDebug(arraylist.writer());
        return arraylist.toOwnedSlice();
    }

    // Is the token the end of an expression or an expression itself?
    pub fn isExpression(self: Token) bool {
        return switch (self) {
            .symbol => |symbol| switch (symbol) {
                ')' => true, // End of function call or expression in parenthesis.
                else => false,
            },

            .keyword,
            .doc_comment,
            => false,

            else => true,
        };
    }
};

pub const LocatedToken = struct { span: Span, token: Token };

pub const State = enum { string, char, number, none };

// Grapheme data + the first codepoint
const Char = struct { code: u21, len: u8, offset: u32 };

inline fn isSymbol(char: u21, comptime dot: bool) bool {
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
        '.', // field access
        => dot, // not allowed in floats
        else => false,
    };
}

// the actual lexer

allocator: Allocator,

output: std.ArrayList(LocatedToken),

script: *Script,
data: []const u8,
pos: Pos = .{},
last: Pos = .{},
start: Pos = .{},

iter: grapheme.Iterator,
buf: struct { Char, ?Char } = .{ undefined, null },
word: []const u8 = "",
basic: bool = true,
fmt_string: bool = false,

logger: Logger(.lexer),
failed: bool = false,

const Self = @This();

// Initializes a new lexer.
pub fn init(script: *Script) !Self {
    const allocator = script.arena.allocator();

    const gd = try grapheme.GraphemeData.init(allocator);
    const iter = grapheme.Iterator.init(script.data.buf, &gd);

    var lexer = Self{
        .allocator = allocator,
        .output = std.ArrayList(LocatedToken).init(allocator),
        .script = script,
        .data = script.data.buf,
        .iter = iter,
        .logger = Logger(.lexer).init(allocator, script),
    };

    try lexer.initialRead();
    return lexer;
}

// De-initializes the lexer.
// This should only be run after the output of the lexer is done being used.
pub fn deinit(self: *Self) void {
    self.output.deinit();
    self.iter.data.deinit();
    self.logger.deinit();
}

inline fn initialRead(self: *Self) !void {
    if (try self.readgc(self.iter.next())) |char| self.buf[0] = char;
    self.buf[1] = try self.readgc(self.iter.next());
}

// Resets the lexer, making it available to parse again.
pub fn reset(self: *Self, free: bool) void {
    if (free) self.output.clearAndFree() else self.output.clearRetainingCapacity();

    self.pos = .{};
    self.last = .{};
    self.start = .{};

    const gd = self.iter.data;
    self.iter = grapheme.Iterator.init(self.data, gd);
    try self.initialRead();

    self.word = "";
    self.basic = true;
}

// Lexes the entire queue.
pub inline fn finish(self: *Self) !void {
    while (!self.finished()) try self.next();
}

// Returns the last token the lexer outputted.
pub inline fn getLastToken(self: *Self) ?LocatedToken {
    if (self.output.items.len == 0) return null;
    return self.output.items[self.output.items.len - 1];
}

// Adds the current word to the output.
fn addWord(self: *Self, comptime symbol: bool) !void {
    if (self.word.len != 0) {
        self.word.ptr = @ptrCast(&self.data[self.start.raw]);

        if (Token.fromWord(self.word)) |token|
            try self.output.append(.{ .span = .{ self.start, self.last }, .token = token });

        self.word.len = 0;
    }

    if (symbol) try self.output.append(.{
        .span = .{ self.pos, self.pos },
        .token = .{ .symbol = @truncate(self.buf[0].code) },
    });
}

inline fn readgc(self: Self, gc: ?grapheme.Grapheme) !?Char {
    return if (gc) |g| blk: {
        const buf = g.bytes(self.data);
        const code = (try unicode.codepointFromUtf8(buf))[0];
        break :blk .{ .code = code, .len = g.len, .offset = g.offset };
    } else null;
}

// Returns true if we've hit EOF.
inline fn advanceRead(self: *Self) !bool {
    self.buf[0] = self.buf[1] orelse {
        // no next char, we're done
        self.pos.raw = self.data.len;
        self.pos.col += 1;
        return true;
    };

    self.buf[1] = try self.readgc(self.iter.next());
    return false;
}

// Advances the lexer position.
// Skips separator characters.
fn advance(self: *Self) !void {
    if (self.finished()) return error.UnexpectedEOF;

    if (self.buf[1]) |next_char| {
        self.buf[0] = next_char;
    } else {
        // no next char, we're done
        // make sure we know we are
        if (!self.finished()) {
            self.pos.raw = self.data.len;
            self.pos.col += 1;
        }

        return;
    }

    self.last = self.pos;
    self.pos.col += 1;

    self.buf[1] = try self.readgc(self.iter.next());

    if (isSepInternal(self.buf[0].code, true)) {
        if (self.basic) try self.addWord(false);

        while (isSepInternal(self.buf[0].code, true)) {
            const code1 = self.buf[0].code;
            if (isNewLine(code1)) {
                self.pos.row += 1;
                self.pos.col = 0;
            } else self.pos.col += 1;

            if (try self.advanceRead()) return;

            const code2 = self.buf[0].code;
            if ((code1 == '\r' and code2 == '\n') or
                (code1 == '\n' and code2 == '\r'))
            {
                // encountered CRLF or LFCR, skip another character
                // this allows us to treat these as one new line
                if (try self.advanceRead()) return;
            }
        }
    }

    self.pos.raw = self.buf[0].offset;
}

pub inline fn finished(self: Self) bool {
    return (self.pos.raw >= self.data.len or self.failed);
}

inline fn isSepInternal(code: u21, include_space: bool) bool {
    return unicode.isNewLine(code) or (include_space and unicode.isEffectiveSpace(code));
}

inline fn isSeparator(self: *Self, code: u21) bool {
    return isSepInternal(code, self.basic);
}

inline fn fatal(self: *Self, comptime fmt: []const u8, args: anytype, span: ?Span, hi: ?Pos) !void {
    try self.logger.err(fmt, args, span, hi);
    self.failed = true;
}

inline fn tokenizeNumber(self: *Self, signed: bool) !void {
    self.basic = false;
    defer self.basic = true;

    var span = Span{ self.pos, self.pos };
    var out = Token.Number{};

    if (self.buf[0].code == '0') {
        out.base = switch (self.buf[1].?.code) {
            'b', 'B' => 2,
            'o', 'O' => 8,
            'x', 'X' => 16,
            else => 10,
        };

        if (out.base != 10) {
            try self.advance();
            try self.advance();
        }
    }

    const after_base = self.pos.raw;

    var state = NumberType.u64;
    var exponent_reached = false;
    var exp_sign_reached = false;
    var type_char: ?Char = null;

    if (signed) {
        state = NumberType.i64;
        try self.advance();
    }

    while (!self.finished()) : (try self.advance()) {
        span[1] = self.pos;
        num: switch (self.buf[0].code) {
            '0'...'9', '_' => {},
            'f' => continue :num if (out.base == 10) 'i' else 'F',
            'n', 'i', 'u' => {
                type_char = self.buf[0];
                break;
            },
            'a'...'d', 'A'...'D', 'F' => if (out.base != 16)
                return self.fatal("hex char in base {} number.", .{out.base}, span, self.pos),
            'e', 'E' => switch (out.base) {
                16 => {},
                10 => if (state == .f64) {
                    if (!exponent_reached) {
                        exponent_reached = true;
                    } else return self.fatal("hex char in float exponent.", .{}, span, self.pos);
                } else return self.fatal("hex char in base 10 number.", .{}, span, self.pos),
                else => return self.fatal("hex char in base {} number.", .{out.base}, span, self.pos),
            },
            '+', '-' => if (exponent_reached) {
                if (exp_sign_reached) break;
                exp_sign_reached = true;
            } else break,
            '.' => {
                if (state == NumberType.f64 or out.base != 10) break;
                // if character after . in number is not a number
                // then it's not a float, instead a field access
                if (!isNumberChar(self.buf[1].?.code)) break;
                state = NumberType.f64;
            },
            else => return self.fatal("unexpected character in number.", .{}, span, self.pos),
        }

        if (exponent_reached and (self.buf[1].?.code == '+' or self.buf[1].?.code == '-')) continue;

        if (isSymbol(self.buf[1].?.code, false) or self.isSeparator(self.buf[1].?.code)) {
            try self.advance();
            break;
        }
    }

    out.buf = self.data[after_base..self.pos.raw];
    var type_span = Span{ self.pos, self.pos };
    var err = false;

    if (type_char) |char| get_type: {
        while (!self.finished()) {
            if (isSymbol(self.buf[0].code, true) or self.isSeparator(self.buf[0].code)) break;
            type_span[1] = self.pos;
            try self.advance();
        }

        const type_string = self.data[type_span[0].raw..self.pos.raw];

        if (type_string.len > 1) {
            out.typ = NumberType.string_map.get(type_string);
            if (out.typ == null) {
                try self.logger.err("unknown type.", .{}, type_span, null);
                err = true;
                break :get_type;
            }
        } else out.typ = switch (char.code) {
            'n' => .bigint,
            'i' => .i64,
            'u' => .u64,
            'f' => .f64,
            else => unreachable,
        };

        span[1] = type_span[1];

        if ((char.code == 'i' or char.code == 'n') and state == .f64) {
            try self.logger.err("floating-point numbers cannot become signed integers.", .{}, span, type_span[0]);
            err = true;
        } else if (char.code == 'u') {
            if (state == .i64 and out.buf[0] == '-') {
                try self.logger.err("unsigned integers cannot be negative.", .{}, span, type_span[0]);
                err = true;
            } else if (state == .f64) {
                try self.logger.err("floating-point numbers cannot become unsigned integers.", .{}, span, type_span[0]);
                err = true;
            }
        }
    }

    try self.output.append(.{ .span = span, .token = .{ .number = out } });
}

inline fn parseEscapeSequence(self: *Self, exit: u8) !u21 {
    const esc_seq_start = self.pos;
    try self.advance();

    switch (self.buf[0].code) {
        'n', 'r', 't', '\\', exit => |esc| {
            return switch (esc) {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                else => esc,
            };
        },
        '{' => if (exit == '`') return '{',
        'x' => {
            try self.advance(); // Advance past 'x'.

            if (self.pos.raw + 1 + 2 >= self.data.len)
                return error.UnexpectedEOF;

            const hex_start = self.pos;
            const chars: [2]u8 = .{
                @truncate(self.buf[0].code),
                @truncate(self.buf[1].?.code),
            };
            try self.advance();

            return parseUnsigned(u8, &chars, 16) catch {
                try self.logger.err("bad hexadecimal number.", .{}, .{ hex_start, self.pos }, null); // Overflow is unreachable.
                return 0xFFFD;
            };
        },
        'u' => {
            try self.advance(); // Advance past 'u'.

            var fail_pos = Pos{};

            if (self.buf[0].code == '{') parse: {
                try self.advance(); // Advance past '{'.

                const esc_char_start = self.pos;
                var idx: usize = 5;

                while (true) : (try self.advance()) {
                    if (self.buf[0].code == '}') break;

                    if (!isHexadecimalChar(self.buf[0].code)) {
                        fail_pos = self.pos;
                        break :parse;
                    } else if (self.isSeparator(self.buf[1].?.code)) {
                        try self.advance();
                        fail_pos = self.pos;
                        break :parse;
                    }

                    if (idx == 0) {
                        while (!self.finished() and self.buf[0].code != '}') try self.advance();
                        try self.logger.err("escape sequence is too large.", .{}, .{ esc_seq_start, self.pos }, null);
                        return 0xFFFD;
                    } else idx -|= 1;
                }

                const char = parseUnsigned(u24, self.data[esc_char_start.raw..self.pos.raw], 16) catch unreachable;
                if (char > 0x10FFFF) {
                    try self.logger.err("escape sequence is too large.", .{}, .{ esc_seq_start, self.pos }, null);
                    return 0xFFFD;
                } else return @truncate(char);
            }

            try self.logger.err("invalid escape sequence.", .{}, .{ esc_seq_start, fail_pos }, fail_pos);
            return 0xFFFD;
        },
        else => {},
    }

    try self.logger.err("invalid escape sequence.", .{}, .{ esc_seq_start, self.pos }, self.pos);
    return 0xFFFD;
}

// Lex characters into the next token(s).
pub fn next(self: *Self) !void {
    if (self.finished()) return;

    const initial_len = self.output.items.len;

    while (initial_len == self.output.items.len) : (try self.advance()) {
        if (self.word.len == 0) self.start = self.pos;

        var signed_number = (self.buf[0].code == '+' or self.buf[0].code == '-') and isNumberChar(self.buf[1].?.code);
        if (signed_number) {
            var last_token = self.getLastToken();
            if (last_token == null) return self.fatal("invalid syntax.", .{}, .{ self.pos, self.pos }, null);
            // if the last token was an expression, it's arithmetic. otherwise, it's a signed number.
            signed_number = !last_token.?.token.isExpression();
        }

        if (self.word.len == 0 and (isNumberChar(self.buf[0].code) or signed_number))
            return self.tokenizeNumber(signed_number);

        char: switch (self.buf[0].code) {
            inline '"', '`' => |e| { // String parser.
                self.basic = false;
                defer self.basic = true;

                const start = self.pos;
                if (!self.fmt_string) try self.advance();
                const inner_start = self.pos.raw;
                var last = self.pos.raw;

                const exit: u8 = @truncate(e);
                const formatted = exit == '`';
                if (formatted) self.fmt_string = true;

                var buffer = std.ArrayList(u8).init(self.allocator);
                var out = Token.String{};

                while (!self.finished()) : (try self.advance()) {
                    const end = self.buf[0].code == exit;

                    if (end or (formatted and self.buf[0].code == '{')) {
                        if (last == inner_start) {
                            out.data.text = self.data[last..self.pos.raw];
                            out.data.managed = false;
                        } else {
                            try buffer.appendSlice(self.data[last..self.pos.raw]);
                            out.data.text = try buffer.toOwnedSlice();
                        }

                        if (formatted) {
                            out.formatting = .{ .end = end };
                            if (end) self.fmt_string = false;
                        }

                        break;
                    } else if (self.buf[0].code == '\\') {
                        try buffer.appendSlice(self.data[last..self.pos.raw]);
                        const c = try self.parseEscapeSequence(exit);
                        try unicode.writeCodepointToUtf8(c, buffer.writer());
                        last = self.buf[1].?.offset;
                    }
                }

                try self.output.append(.{
                    .span = .{ start, self.pos },
                    .token = .{ .string = out },
                });
            },
            '\'' => { // Character parser.
                self.basic = false;
                defer self.basic = true;

                const start = self.pos;
                var char: ?u21 = null;

                try self.advance();
                if (self.buf[0].code == '\'') {
                    try self.logger.err("empty characters not allowed.", .{}, .{ start, self.pos }, null);
                } else if (self.buf[0].code == '\\') {
                    char = try self.parseEscapeSequence('\'');
                    try self.advance();
                } else if (self.buf[1].?.code == '\'') {
                    char = self.buf[0].code;
                    try self.advance();
                }

                if (self.buf[0].code != '\'') {
                    while (!self.finished() and self.buf[0].code != '\'') try self.advance();
                    try self.logger.err("character is too large.", .{}, .{ start, self.pos }, null);
                    try self.logger.info("strings are defined with double quotes.", .{}, null, null);
                }

                try self.output.append(.{
                    .span = .{ start, self.pos },
                    .token = .{ .char = char orelse 0xFFFD },
                });
            },
            // Symbol or comment.
            '/' => if (self.buf[1].?.code == '/' or self.buf[1].?.code == '*') {
                var span = Span{ self.pos, self.pos };
                var start = self.pos;
                try self.advance();
                var doc = false;
                var out = Token.DocComment{};

                switch (self.buf[0].code) {
                    // Single-line comment.
                    '/' => {
                        if (isNewLine(self.buf[1].?.code)) continue;
                        try self.advance();
                        doc = self.buf[0].code == '/';
                        out.top_level = self.buf[0].code == '!';
                        if (isNewLine(self.buf[1].?.code)) continue;
                        if (doc or out.top_level) try self.advance();

                        start = self.pos;
                        while (!isNewLine(self.buf[1].?.code)) try self.advance();
                        span[1] = self.pos;
                    },
                    // Multi-line comment.
                    '*' => {
                        try self.advance();
                        doc = self.buf[0].code == '*';
                        if (doc) try self.advance();

                        start = self.pos;
                        while (!(self.buf[0].code == '*' and self.buf[1].?.code == '/')) : (try self.advance()) span[1] = self.pos;
                        try self.advance();
                    },
                    else => unreachable,
                }

                if (out.top_level and span[0].raw != 0)
                    return self.fatal("top-level doc comment must be at the start of the file.", .{}, span, null);

                out.text = self.data[start.raw .. span[1].raw + 1];

                if (doc or out.top_level) try self.output.append(.{
                    .span = span,
                    .token = .{ .doc_comment = out },
                });
            } else try self.addWord(true), // Symbol.
            // Symbol or end of format string expression.
            '}' => if (self.fmt_string) {
                try self.advance();
                continue :char '`';
            } else try self.addWord(true), // Symbol.
            else => if (!isSymbol(self.buf[0].code, true)) {
                self.word.len +%= self.buf[0].len;
            } else try self.addWord(true), // Symbol.
        }
    }
}
