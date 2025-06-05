const std = @import("std");
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;
const allocPrint = std.fmt.allocPrint;
const parseUnsigned = std.fmt.parseUnsigned;
const parseInt = std.fmt.parseInt;
const parseFloat = std.fmt.parseFloat;

const code_point = @import("code_point");
const CodePoint = code_point.CodePoint;

const Script = @import("../Script.zig");
const BuiltinType = @import("../vm/types.zig").BuiltinType;

const util = @import("../util.zig");

const unicode = @import("../unicode.zig");
const isGap = unicode.isGap;
const isNewLine = unicode.isNewLine;
const isNumberChar = unicode.isNumber;
const isHexadecimalChar = unicode.isHexadecimal;

const ast = @import("../ast.zig");
const Pos = ast.Pos;
const Span = ast.Span;

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
        float: bool = false,
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

        throw,    // throw error
        @"try",   // try function which throws errors
        @"catch", // catch error
        assert,   // throw error if condition not true

        @"if",     // define conditional if statement
        @"else",   // define conditional else statement
        @"switch", // define switch statement

        @"pub",   // publicize declaration
        @"const", // declare constant variable
        @"var",   // declare mutable variable

        func,        // define function
        @"return",   // return from function
        @"defer",    // execute when function returns
        @"errdefer", // execute when function throws error
        ref,         // get mutable reference to param

        @"struct", // define struct
        @"enum",   // define enum
        @"union",  // define union
        interface, // define interface

        @"while",    // define while loop
        @"for",      // define for loop
        @"break",    // loop break
        @"continue", // loop continue

        @"async", // define function as async
        @"await", // await async function

        import, // import a script
        from, // cherry-pick imports

        pub const string_map = util.enumStringMap(Keyword);
    };
    // zig fmt: on

    pub const DocComment = struct {
        text: []const u8 = "",
        top_level: bool = false,
    };

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
            .number => |v| try writer.print("number: {s}", .{v.buf}),
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
};

pub const LocatedToken = struct { span: Span, token: Token };

inline fn isSymbol(char: u21) bool {
    return switch (char) {
        '&', // bitwise and
        '|', // bitwise or
        '!', // logical not
        '~', // bitwise not
        '^', // bitwise xor
        '<', // less than comparison operator / left shift
        '>', // greater than comparison operator / right shift
        '*', // reference / multiplication operator / comment
        '/', // division operator / comment
        '.', // field access
        '{', // open block / array
        '(', // open tuple / arguments
        '[', // open array access
        '}', // close block / array
        ')', // close tuple / arguments
        ']', // close array access
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

// MESSAGES

const Logger = @import("../log.zig").Logger(.lexer, Message);

pub const Message = union(enum) {
    // numbers
    mismatched_bases: struct { u8, u8 },
    unexpected_char_number,

    // escape sequences
    invalid_esc,
    esc_no_end,
    esc_too_large,
    non_hex_in_hex_esc,

    // characters
    empty_char,
    char_no_end,
    char_too_large,

    // miscellaneous
    misplaced_top_level_doc,

    pub fn format(
        self: *const Message,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try switch (self.*) {
            // numbers
            .mismatched_bases => |b| writer.print("base {} digit in base {} number", b),
            .unexpected_char_number => writer.writeAll("unexpected character in number"),

            // escape sequences
            .invalid_esc => writer.writeAll("invalid escape sequence"),
            .esc_no_end => writer.writeAll("escape sequence has no end"),
            .esc_too_large => writer.writeAll("escape sequence is too large"),
            .non_hex_in_hex_esc => writer.writeAll("non-hex character in hex escape sequence"),

            // characters
            .empty_char => writer.writeAll("character cannot be empty"),
            .char_no_end => writer.writeAll("character has no end"),
            .char_too_large => writer.writeAll("character is too large"),

            // miscellaneous
            .misplaced_top_level_doc => writer.writeAll("top-level doc comments must be at the start of the script"),
        };
    }
};

// STRUCTURE

script: *Script,

arena: Arena,
allocator: Allocator,
output: std.ArrayListUnmanaged(LocatedToken) = .{},

pos: Pos = .{},
last: Pos = .{},
start: Pos = .{},

iter: code_point.Iterator,
buf: struct { CodePoint, ?CodePoint } = .{ undefined, null },
word: []const u8 = "",
basic: bool = true,
fmt_string: bool = false,

logger: Logger,
failed: bool = false,

const Self = @This();

// INIT / DEINIT

pub fn init(script: *Script) !*Self {
    var lexer = try script.engine.allocator.create(Self);

    lexer.* = .{
        .script = script,
        .arena = Arena.init(script.engine.allocator),
        .allocator = undefined,
        .output = undefined,
        .iter = .{ .bytes = script.src },
        .logger = undefined,
    };

    if (lexer.iter.next()) |char| lexer.buf[0] = char;
    lexer.buf[1] = lexer.iter.next();

    lexer.allocator = lexer.arena.allocator();
    lexer.logger = Logger.init(lexer.allocator, script);

    return lexer;
}

pub inline fn deinit(self: Self) void {
    self.arena.deinit();
}

// MISCELLANEOUS UTILITIES

pub inline fn getLastToken(self: *Self) ?LocatedToken {
    if (self.output.items.len == 0) return null;
    return self.output.items[self.output.items.len - 1];
}

pub inline fn finished(self: Self) bool {
    return (self.pos.raw >= self.script.src.len or self.failed);
}

// IMPLEMENTATION

inline fn addToken(self: *Self, span: Span, token: Token) !void {
    try self.output.append(self.allocator, .{ .span = span, .token = token });
}

fn addWord(self: *Self, comptime symbol: bool) !void {
    if (self.word.len > 0) {
        self.word.ptr = self.script.src.ptr + self.start.raw;

        const token: ?Token = blk: {
            if (std.mem.eql(u8, self.word, "null")) break :blk .null;
            if (std.mem.eql(u8, self.word, "undefined")) break :blk .undefined;
            if (std.mem.eql(u8, self.word, "true")) break :blk .{ .bool = true };
            if (std.mem.eql(u8, self.word, "false")) break :blk .{ .bool = false };
            if (self.word.len > 0 and !isNumberChar(self.word[0])) {
                if (Token.Keyword.string_map.get(self.word)) |kw| break :blk .{ .keyword = kw };
                if (BuiltinType.string_map.get(self.word)) |t| break :blk .{ .builtin_type = t };
                break :blk .{ .identifier = self.word };
            } else break :blk null;
        };

        if (token) |tok| try self.addToken(.{ self.start, self.last }, tok);

        self.word.len = 0;
    }

    if (symbol) try self.addToken(.{ self.pos, self.pos }, .{ .symbol = @truncate(self.buf[0].code) });
}

inline fn advanceRead(self: *Self) !void {
    defer self.buf[1] = self.iter.next();
    self.buf[0] = self.buf[1] orelse {
        // no next char, we're done
        self.pos.raw = self.script.src.len;
        self.pos.col += 1;
        return error.UnexpectedEOF;
    };
}

inline fn advanceCheck(self: Self, char: u21) bool {
    return isNewLine(char) or (self.basic and unicode.isSpace(char));
}

fn advance(self: *Self) !void {
    if (self.finished()) return error.UnexpectedEOF;

    self.last = self.pos;
    try self.advanceRead();
    self.pos.col += 1;

    if (self.advanceCheck(self.buf[0].code)) {
        // wont do anything if word len is 0, always call it
        try @call(.always_inline, addWord, .{ self, false });

        while (self.advanceCheck(self.buf[0].code)) {
            const code1 = self.buf[0].code;
            if (isNewLine(code1)) {
                self.pos.row += 1;
                self.pos.col = 0;
            } else self.pos.col += 1;

            self.advanceRead() catch return;

            const code2 = self.buf[0].code;
            if ((code1 == '\r' and code2 == '\n') or
                (code1 == '\n' and code2 == '\r'))
            {
                // encountered CRLF or LFCR, skip another character
                // this allows us to treat these as one new line
                self.advanceRead() catch return;
            }
        }
    }

    self.pos.raw = self.buf[0].offset;
}

inline fn fatal(self: *Self, msg: Message, span: Span, hi: ?Pos) !void {
    try self.logger.log(msg, span, hi);
    self.failed = true;
}

inline fn tokenizeNumber(self: *Self, signed: bool) !void {
    self.basic = false;
    defer self.basic = true;

    var span = Span{ self.pos, self.pos };
    var out = Token.Number{};
    var base: u8 = 10;

    if (signed) try self.advance();
    if (self.buf[0].code == '0') {
        base = switch (self.buf[1].?.code) {
            'b', 'B' => 2,
            'o', 'O' => 8,
            'x', 'X' => 16,
            else => 10,
        };

        if (base != 10) {
            try self.advance();
            try self.advance();
        }
    }

    var end_reached = false;
    var exponent_reached = false;
    var exp_sign_reached = false;

    while (!end_reached) : (try self.advance()) {
        switch (self.buf[0].code) {
            '0'...'1', '_' => {},
            '2'...'7' => if (base == 2) return self.fatal(.{ .mismatched_bases = .{ 8, 2 } }, span, self.pos),
            '8'...'9' => if (base < 10) return self.fatal(.{ .mismatched_bases = .{ 10, base } }, span, self.pos),
            'a'...'d', 'A'...'D', 'f', 'F' => if (base != 16) return self.fatal(.{ .mismatched_bases = .{ 16, base } }, span, self.pos),
            'e', 'E' => if (base != 16) {
                if (base != 10) return self.fatal(.{ .mismatched_bases = .{ 16, base } }, span, self.pos);
                if (out.float) {
                    if (!exponent_reached) {
                        exponent_reached = true;
                    } else return self.fatal(.unexpected_char_number, span, self.pos);
                } else return self.fatal(.{ .mismatched_bases = .{ 16, 10 } }, span, self.pos);
            },
            '+', '-' => if (exponent_reached and !exp_sign_reached) {
                exp_sign_reached = true;
            } else break,
            '.' => {
                if (out.float or base != 10) break;
                // if character after . in number is not a number
                // then it's not a float, instead a field access
                if (!isNumberChar(self.buf[1].?.code)) break;
                out.float = true;
            },
            else => return self.fatal(.unexpected_char_number, span, self.pos),
        }

        span[1] = self.pos;
        end_reached = switch (self.buf[1].?.code) {
            '+', '-', '.' => false,
            else => |c| isSymbol(c) or isGap(c),
        };
    }

    out.buf = self.script.src[span[0].raw .. span[1].raw + 1];

    try self.addToken(span, .{ .number = out });
}

fn parseEscapeSequence(self: *Self, exit: u21) !u21 {
    self.basic = false;
    defer self.basic = true;

    const esc_seq_start = self.pos;
    try self.advance();

    switch (self.buf[0].code) {
        'n', 'r', 't', '\\' => |esc| {
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
            const span = Span{ self.pos, self.pos.next() };

            var chars: [2]u8 = undefined;
            inline for (0..2) |i| {
                chars[i] = @truncate(self.buf[0].code);
                if (!(isHexadecimalChar(chars[i]) or chars[i] == '_')) {
                    try self.logger.log(.non_hex_in_hex_esc, span, self.pos);
                    return error.InvalidCharacter;
                } else if (i == 0) try self.advance();
            }

            return parseUnsigned(u8, &chars, 16) catch unreachable;
        },
        'u' => {
            try self.advance(); // Advance past 'u'.
            const esc_char_start = self.pos.next();

            if (self.buf[0].code == '{' and !isNewLine(self.buf[1].?.code)) parse: {
                try self.advance(); // Advance past '{'.

                var idx: usize = 5;
                while (true) : (try self.advance()) {
                    if (self.buf[0].code == '}') break;

                    if (!(isHexadecimalChar(self.buf[0].code) or self.buf[0].code == '_') or
                        isNewLine(self.buf[1].?.code)) break :parse;

                    if (idx == 0) {
                        while (self.buf[0].code != '}') try self.advance();
                        try self.logger.log(.esc_too_large, .{ esc_seq_start, self.pos }, null);
                        return error.Overflow;
                    } else idx -|= 1;
                }

                return parseUnsigned(u21, self.script.src[esc_char_start.raw..self.pos.raw], 16) catch {
                    try self.logger.log(.esc_too_large, .{ esc_seq_start, self.pos }, null); // InvalidCharacter is unreachable.
                    return error.Overflow;
                };
            }

            if (isNewLine(self.buf[1].?.code)) {
                const fail_pos = self.pos.next();
                try self.advance();
                try self.fatal(.esc_no_end, .{ esc_seq_start, fail_pos }, fail_pos);
                return error.UnexpectedEOL;
            }

            try self.logger.log(.non_hex_in_hex_esc, .{ esc_char_start, self.pos }, self.pos);
            return error.InvalidCharacter;
        },
        else => |char| if (char == exit) return char,
    }

    try self.logger.log(.invalid_esc, .{ esc_seq_start, self.pos }, self.pos);
    return error.InvalidCharacter;
}

pub fn next(self: *Self) !void {
    if (self.finished()) return;

    const last_token = self.getLastToken();
    const initial_len = self.output.items.len;

    while (initial_len == self.output.items.len and !self.failed) : (try self.advance()) {
        if (self.word.len == 0) {
            var signed_number = (self.buf[0].code == '-' or self.buf[0].code == '+') and isNumberChar(self.buf[1].?.code);
            // if the last token was an expression, it's arithmetic. otherwise, it's a signed number.
            if (signed_number and last_token != null)
                signed_number = !switch (last_token.?.token) {
                    // end of function call or expression in parenthesis
                    .symbol => |symbol| symbol == ')',
                    .keyword, .doc_comment => false,
                    else => true,
                };

            if (isNumberChar(self.buf[0].code) or signed_number)
                return self.tokenizeNumber(signed_number);

            self.start = self.pos;
        }

        char: switch (self.buf[0].code) {
            '"', '`' => |exit| { // String parser.
                const start = self.pos;
                var last = if (!self.fmt_string) blk: {
                    try self.advance();
                    break :blk start.raw + 1;
                } else start.raw;

                const formatted = exit == '`';
                if (formatted) self.fmt_string = true;

                // use the engine allocator so they don't free after arena deinit
                const allocator = self.script.engine.allocator;
                var buffer = std.ArrayList(u8).init(allocator);
                var out = Token.String{};

                const real_start = last;
                while (true) : (try self.advance()) {
                    const end = self.buf[0].code == exit;
                    if (end or (formatted and self.buf[0].code == '{')) {
                        if (last == real_start) {
                            out.data.text = self.script.src[last..self.pos.raw];
                            out.data.managed = false;
                        } else {
                            try buffer.appendSlice(self.script.src[last..self.pos.raw]);
                            out.data.text = try buffer.toOwnedSlice();
                        }

                        if (formatted) {
                            out.formatting = .{ .end = end };
                            if (end) self.fmt_string = false;
                        }

                        break;
                    } else if (self.buf[0].code == '\\') {
                        try buffer.appendSlice(self.script.src[last..self.pos.raw]);
                        const c = self.parseEscapeSequence(exit) catch if (self.failed) return else 0xFFFD;
                        try unicode.writeCodepointToUtf8(c, buffer.writer());
                        last = self.buf[1].?.offset;
                    }
                }

                try self.addToken(.{ start, self.pos }, .{ .string = out });
            },
            '\'' => { // Character parser.
                const start = self.pos;
                var char: u21 = 0xFFFD;

                try self.advance();
                if (self.buf[0].code == '\'') {
                    try self.logger.log(.empty_char, .{ start, self.pos }, null);
                } else if (self.buf[0].code == '\\') {
                    char = self.parseEscapeSequence('\'') catch if (self.failed) return else 0xFFFD;
                    try self.advance();
                } else if (self.buf[1].?.code == '\'') {
                    char = self.buf[0].code;
                    try self.advance();
                }

                if (self.buf[0].code != '\'') {
                    while (self.buf[0].code != '\'') : (try self.advance())
                        if (isNewLine(self.buf[1].?.code))
                            return self.fatal(.char_no_end, .{ start, start }, null);

                    if (char != 0xFFFD) try self.logger.log(.char_too_large, .{ start, self.pos }, null);
                }

                try self.addToken(.{ start, self.pos }, .{ .char = char });
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
                        if (isNewLine(self.buf[1].?.code)) continue else try self.advance();
                        out.top_level = self.buf[0].code == '!';
                        doc = out.top_level or self.buf[0].code == '/';
                        if (isNewLine(self.buf[1].?.code)) continue else try self.advance();

                        start = self.pos;
                        while (!isNewLine(self.buf[1].?.code)) try self.advance();
                        span[1] = self.pos;
                    },
                    // Multi-line comment.
                    '*' => {
                        try self.advance();
                        out.top_level = self.buf[0].code == '!';
                        doc = out.top_level or self.buf[0].code == '*';
                        try self.advance();

                        start = self.pos;
                        while (!(self.buf[0].code == '*' and self.buf[1].?.code == '/')) : (try self.advance()) span[1] = self.pos;
                        try self.advance();
                    },
                    else => unreachable,
                }

                if (out.top_level and span[0].raw != 0)
                    try self.logger.log(.misplaced_top_level_doc, span, null);

                out.text = self.script.src[start.raw .. span[1].raw + 1];
                if (doc) try self.addToken(span, .{ .doc_comment = out });
            } else try self.addWord(true), // Symbol.
            // Symbol or end of format string expression.
            '}' => if (self.fmt_string) {
                try self.advance();
                continue :char '`';
            } else try self.addWord(true), // Symbol.
            else => if (!isSymbol(self.buf[0].code)) {
                self.word.len += self.buf[0].len;
            } else try self.addWord(true), // Symbol.
        }
    }
}
