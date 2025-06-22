const std = @import("std");
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;
const allocPrint = std.fmt.allocPrint;
const parseUnsigned = std.fmt.parseUnsigned;
const parseInt = std.fmt.parseInt;
const parseFloat = std.fmt.parseFloat;

const utils = @import("utils");

const unicode = @import("../unicode.zig");
const Rune = unicode.Rune;
const isGap = unicode.isGap;
const isNewLine = unicode.isNewLine;
const isNumberChar = unicode.isNumber;
const isHexadecimalChar = unicode.isHexadecimal;

const ast = @import("../ast.zig");
const Pos = ast.Pos;
const Span = ast.Span;
const BasicType = ast.BasicType;

const Script = @import("../Script.zig");

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
    basic_type: BasicType,
    identifier: []const u8,
    doc_comment: DocComment,

    pub const Number = struct {
        buf: []const u8 = "",
        float: bool = false,
    };

    pub const String = struct {
        data: ast.String = .{},
        fmt: ?enum { part, end } = null,
    };

    /// Current count: 29
    pub const Keyword = enum {
        // zig fmt: off
        as, // casting

        throw,    // throw error
        @"try",   // try function which throws errors
        @"catch", // catch error
        assert,   // throw error if condition not true

        @"if",     // define conditional if statement
        @"else",   // define conditional else statement
        @"switch", // define switch statement

        @"pub",   // publicize declaration
        @"const", // declare constant value
        @"let",   // declare runtime variable
        @"mut",   // make variable mutable
        @"type",  // declare type

        func,        // define function
        @"return",   // return from function
        @"defer",    // execute when function returns
        @"errdefer", // execute when function throws error

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
        // zig fmt: on

        pub const string_map = utils.EnumStringMap(Keyword, .fastest);
    };

    pub const DocComment = struct {
        text: []const u8 = "",
        top_level: bool = false,
    };

    pub fn format(
        self: *const Token,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self.*) {
            .null => try writer.writeAll("null"),
            .undefined => try writer.writeAll("undefined"),
            .bool => |v| try writer.print("bool: {}", .{v}),
            .char => |v| {
                try writer.writeAll("char: '");
                try unicode.writeCharUtf8(v, writer);
                try writer.writeByte('\'');
            },
            .number => |v| try writer.print("number: {s}", .{v.buf}),
            .string => |v| {
                try writer.print("string: \"{s}\"", .{v.data.text});
                if (v.data.managed) try writer.writeAll(" (managed)");
                if (v.fmt) |fmt| try writer.print(" (format {s})", .{@tagName(fmt)});
            },

            .symbol => |v| {
                try writer.writeAll("symbol: ");
                try writer.writeByte(v);
            },
            .keyword => |v| try writer.print("keyword: {s}", .{@tagName(v)}),
            .basic_type => |v| try writer.print("basic type: {s}", .{@tagName(v)}),
            .identifier => |v| try writer.print("identifier: {s}", .{v}),
            .doc_comment => |v| {
                try writer.print("doc comment: \"{s}\"", .{v.text});
                if (v.top_level) try writer.writeAll(" (top-level)");
            },
        }
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

const max_row = std.math.maxInt(u32); // 4294967295
const max_col = std.math.maxInt(u16); // 65535

// MESSAGES

const log = @import("../log.zig");

// zig fmt: off
const message = log.simpleMessage(&.{
    // numbers
    .{ .fatal, .mismatched_bases,       "base {} digit in base {} number", struct { u8, u8 } },
    .{ .fatal, .unexpected_char_number, "unexpected character in number",  void },

    // escape sequences
    .{ .err,   .invalid_esc,    "invalid escape sequence",      void },
    .{ .fatal, .esc_no_start,   "escape sequence has no start", void },
    .{ .fatal, .esc_no_end,     "escape sequence has no end",   void },
    .{ .err,   .esc_too_large,  "escape sequence is too large", void },
    .{ .err,   .surrogate_half, "escape sequence cannot represent UTF-16 surrogate half (U+D800 <= U+{X} <= U+DFFF)", struct { u16 } },
    .{ .err,   .non_hex_in_hex_esc,          "non-hex character in hex escape sequence",             void },
    .{ .err,   .underscore_in_short_hex_esc, "underscores not allowed in short hex escape sequence", void },

    // characters
    .{ .err,   .empty_char,     "character cannot be empty", void },
    .{ .fatal, .char_no_end,    "character has no end",      void },
    .{ .err,   .char_too_large, "character is too large",    void },

    // miscellaneous
    .{ .err, .misplaced_top_level_doc, "top-level doc comments must be at the start of the script", void },
    .{ .err, .identifier_too_long,     "identifier exceeds maximum length ({} bytes > 64 bytes)",   struct { usize } },
}, &.{
    .{ .fatal, .row_too_long,   "row {} exceeded byte length limit ({} > 65535)", struct { u32, usize } },
    .{ .fatal, .too_many_rows,  "exceeded maximum row count of 4294967295", void },
    .{ .fatal, .unexpected_eof, "unexpected end-of-file", void },
});
// zig fmt: on

pub const Message = message[0];
pub const Error = Allocator.Error || message[1];
const Logger = log.Logger(Message, message[1], message[2]);

// STRUCTURE

script: *Script,

output: std.ArrayListUnmanaged(LocatedToken) = .{},

pos: Pos = .{},
last: Pos = .{},
start: Pos = .{},

cursor: usize = 0,
buf: struct { Rune, ?Rune } = .{ undefined, null },
row_start: Pos = .{},
word: []const u8 = "",
basic: bool = true,
fmt_string: bool = false,

logger: Logger,

const Self = @This();

// INIT / DEINIT

pub fn init(script: *Script) !*Self {
    var lexer = try script.engine.allocator.create(Self);
    lexer.* = .{ .script = script, .logger = .{ .script = script } };

    if (Rune.decodeCursor(script.src, &lexer.cursor)) |rune| lexer.buf[0] = rune;
    lexer.buf[1] = Rune.decodeCursor(script.src, &lexer.cursor);

    return lexer;
}

// MISCELLANEOUS TOOLS

pub inline fn getLastToken(self: *Self) ?LocatedToken {
    if (self.script.tokens.items.len == 0) return null;
    return self.script.tokens.items[self.script.tokens.items.len - 1];
}

pub inline fn finished(self: Self) bool {
    return (self.pos.raw >= self.script.src.len or self.script.failed);
}

// READER TOOLS

fn read(self: *Self, comptime pos: comptime_int) if (pos == 0) u21 else Error!u21 {
    return switch (pos) {
        0 => self.buf[0].code,
        1 => if (self.buf[1]) |ahead|
            ahead.code
        else {
            try self.logger.log(.unexpected_eof, .{ self.pos, .{} }, null);
            unreachable;
        },
        else => {
            var cursor = self.cursor;
            var ahead: Rune = undefined;
            inline for (0..pos) |_|
                ahead = Rune.decodeCursor(self.script.src, &cursor) orelse
                    try self.logger.log(.unexpected_eof, .{ self.pos, .{} }, null);

            return ahead;
        },
    };
}

fn nextPos(self: *Self) Pos {
    return .{
        .raw = self.pos.raw + self.buf[0].len,
        .row = self.pos.row,
        .col = self.pos.col + 1,
    };
}

// OUTPUT TOOLS

inline fn addToken(self: *Self, span: Span, token: Token) Error!void {
    try self.script.tokens.append(self.script.allocator(), .{ .span = span, .token = token });
}

fn addWord(self: *Self, comptime symbol: bool) Error!void {
    if (self.word.len > 0) {
        defer self.word.len = 0;
        self.word.ptr = self.script.src.ptr + self.start.raw;

        const span = Span{ self.start, self.last };
        const token: ?Token = blk: {
            if (self.word.len > 64) {
                try self.logger.log(.{ .identifier_too_long = .{self.word.len} }, span, null);
                break :blk .{ .identifier = "" };
            }

            if (std.mem.eql(u8, self.word, "null")) break :blk .null;
            if (std.mem.eql(u8, self.word, "undefined")) break :blk .undefined;
            if (std.mem.eql(u8, self.word, "true")) break :blk .{ .bool = true };
            if (std.mem.eql(u8, self.word, "false")) break :blk .{ .bool = false };
            if (Token.Keyword.string_map.get(self.word)) |kw| break :blk .{ .keyword = kw };
            if (BasicType.string_map.get(self.word)) |t| break :blk .{ .basic_type = t };
            break :blk .{ .identifier = self.word };
        };

        if (token) |tok| try self.addToken(span, tok);
    }

    if (symbol) try self.addToken(.{ self.pos, self.pos }, .{ .symbol = @truncate(self.read(0)) });
}

// ADVANCE TOOLS

inline fn advanceRead(self: *Self) Error!void {
    defer self.buf[1] = Rune.decodeCursor(self.script.src, &self.cursor);
    self.buf[0] = self.buf[1] orelse {
        // no next char, we're done
        self.pos.raw = self.script.src.len;
        self.pos.col += 1;
        return self.logger.log(.unexpected_eof, .{ self.pos, .{} }, null);
    };
}

inline fn advanceCheck(self: Self, char: u21) bool {
    return isNewLine(char) or (self.basic and unicode.isSpace(char));
}

fn advance(self: *Self) Error!void {
    if (self.finished()) return self.logger.log(.unexpected_eof, .{ self.pos, .{} }, null);

    self.last = self.pos;
    try self.advanceRead();
    self.pos.raw = self.buf[0].offset;
    self.pos.col += 1;

    if (!self.advanceCheck(self.buf[0].code)) return;
    // wont do anything if word len is 0, always call it
    try @call(.always_inline, addWord, .{ self, false });

    while (self.advanceCheck(self.buf[0].code)) {
        const code1 = self.buf[0].code;
        const new_line = isNewLine(code1);

        if (new_line) {
            if (self.pos.row == max_row) {
                try self.logger.log(.too_many_rows, .{ self.pos, .{} }, null);
                return error.TooManyRows;
            }

            const len = (self.pos.raw - self.row_start.raw) - 1;
            if (len > max_col) {
                const msg = Message{ .row_too_long = .{ self.pos.row, len } };
                try self.logger.log(msg, .{ self.row_start, self.pos }, null);
                return error.RowTooLong;
            }
            self.script.rows.items[self.pos.row].len = @truncate(len);

            self.pos.row += 1;
            self.pos.col = 0;
        } else self.pos.col += 1;

        self.advanceRead() catch return;
        defer self.pos.raw = self.buf[0].offset;
        if (!new_line) continue;

        const code2 = self.buf[0].code;
        if ((code1 == '\r' and code2 == '\n') or
            (code1 == '\n' and code2 == '\r'))
        {
            // encountered CRLF or LFCR, skip another character
            // this allows us to treat these as one new line
            self.advanceRead() catch return;
        }

        const row = ast.RowInfo{ .raw = @truncate(self.pos.raw + 1), .len = 0 };
        try self.script.rows.append(self.script.allocator(), row);
        self.row_start = self.pos;
    }
}

// TOKENIZER METHODS

inline fn tokenizeNumber(self: *Self, signed: bool) Error!void {
    self.basic = false;
    defer self.basic = true;

    var span = Span{ self.pos, self.pos };
    var out = Token.Number{};
    var base: u8 = 10;

    if (signed) try self.advance();
    if (self.read(0) == '0') {
        base = switch (try self.read(1)) {
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
        switch (self.read(0)) {
            '0'...'1', '_' => {},
            '2'...'7' => if (base == 2) return self.logger.log(.{ .mismatched_bases = .{ 8, 2 } }, span, self.pos),
            '8'...'9' => if (base < 10) return self.logger.log(.{ .mismatched_bases = .{ 10, base } }, span, self.pos),
            'a'...'d', 'A'...'D', 'f', 'F' => if (base != 16) return self.logger.log(.{ .mismatched_bases = .{ 16, base } }, span, self.pos),
            'e', 'E' => if (base != 16) {
                if (base == 10 and out.float) {
                    if (!exponent_reached) {
                        exponent_reached = true;
                    } else return self.logger.log(.unexpected_char_number, span, self.pos);
                } else return self.logger.log(.{ .mismatched_bases = .{ 16, base } }, span, self.pos);
            },
            '+', '-' => if (exponent_reached and !exp_sign_reached) {
                exp_sign_reached = true;
            } else break,
            '.' => {
                if (out.float or base != 10) break;
                // if character after . in number is not a number
                // then it's not a float, instead a field access
                if (!isNumberChar(try self.read(1))) break;
                out.float = true;
            },
            else => return self.logger.log(.unexpected_char_number, span, self.pos),
        }

        span[1] = self.pos;
        end_reached = switch (try self.read(1)) {
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

    switch (self.read(0)) {
        'n' => return '\n',
        'r' => return '\r',
        't' => return '\t',
        '\\' => return '\\',
        '{' => if (exit == '`') return '{',
        'x' => {
            try self.advance(); // Advance past 'x'.
            const span = Span{ self.pos, self.nextPos() };

            var chars: [2]u8 = undefined;
            inline for (0..2) |i| {
                chars[i] = @truncate(self.read(0));
                if (!isHexadecimalChar(chars[i])) {
                    const msg: Message = if (chars[i] == '_')
                        .underscore_in_short_hex_esc
                    else
                        .non_hex_in_hex_esc;
                    try self.logger.log(msg, span, self.pos);
                    return error.InvalidCharacter;
                } else if (i == 0) try self.advance();
            }

            return parseUnsigned(u8, &chars, 16) catch unreachable;
        },
        'u' => {
            try self.advance(); // Advance past 'u'.
            const esc_char_start = self.nextPos();
            var non_hex_pos = Pos{};

            if (self.read(0) != '{') {
                try self.logger.log(.esc_no_start, .{ esc_seq_start, self.pos }, self.pos);
                return error.InvalidCharacter;
            }

            if (!isNewLine(try self.read(1))) parse: {
                try self.advance(); // Advance past '{'.

                var idx: usize = 5;
                while (self.read(0) != '}') : (try self.advance()) {
                    if (self.read(0) == '_') continue;

                    if (!isHexadecimalChar(self.read(0)) or isNewLine(try self.read(1))) {
                        non_hex_pos = self.pos;
                        while (self.read(0) != '}') try self.advance();
                        break :parse;
                    }

                    if (idx == 0) {
                        while (self.read(0) != '}') try self.advance();
                        try self.logger.log(.esc_too_large, .{ esc_seq_start, self.pos }, null);
                        return error.Overflow;
                    } else idx -= 1;
                }

                // could throw Overflow or InvalidCharacter, 6 max chars means it will always fit in u24, we ensure hex format
                const char = parseUnsigned(u24, self.script.src[esc_char_start.raw..self.pos.raw], 16) catch unreachable;

                if (char > 0x110000) {
                    try self.logger.log(.esc_too_large, .{ esc_seq_start, self.pos }, null);
                    return error.Overflow;
                } else if (char >= 0xD800 and char <= 0xDFFF) {
                    try self.logger.log(.{ .surrogate_half = .{@truncate(char)} }, .{ esc_seq_start, self.pos }, null);
                    return error.SurrogateHalf;
                } else return @truncate(char);
            }

            if (isNewLine(try self.read(1))) {
                const fail_pos = self.nextPos();
                try self.advance();
                try self.logger.log(.esc_no_end, .{ esc_seq_start, fail_pos }, fail_pos);
                return error.UnexpectedEOL;
            }

            try self.logger.log(.non_hex_in_hex_esc, .{ esc_char_start, self.last }, non_hex_pos);
            return error.InvalidCharacter;
        },
        else => |char| if (char == exit) return char,
    }

    try self.logger.log(.invalid_esc, .{ esc_seq_start, self.pos }, self.pos);
    return error.InvalidCharacter;
}

pub fn next(self: *Self) Error!void {
    if (self.finished()) return;

    const last_token = self.getLastToken();
    const initial_len = self.script.tokens.items.len;

    while (initial_len == self.script.tokens.items.len and !self.script.failed) : (try self.advance()) {
        if (self.word.len == 0) {
            var signed_number = (self.read(0) == '-' or self.read(0) == '+') and isNumberChar(try self.read(1));
            // if the last token was an expression, it's arithmetic. otherwise, it's a signed number.
            if (signed_number and last_token != null)
                signed_number = !switch (last_token.?.token) {
                    // end of function call or expression in parenthesis
                    .symbol => |symbol| symbol == ')',
                    .keyword, .doc_comment => false,
                    else => true,
                };

            if (isNumberChar(self.read(0)) or signed_number)
                return self.tokenizeNumber(signed_number);

            self.start = self.pos;
        }

        char: switch (self.read(0)) {
            '"', '`' => |exit| { // String parser.
                const start = self.pos;
                var last = if (!self.fmt_string) blk: {
                    try self.advance();
                    break :blk start.raw + 1;
                } else start.raw;

                const formatted = exit == '`';
                if (formatted) self.fmt_string = true;

                const allocator = self.script.allocator();
                var buffer = std.ArrayList(u8).init(allocator);
                var out = Token.String{};

                const real_start = last;
                while (true) : (try self.advance()) {
                    const end = self.read(0) == exit;
                    if (end or (formatted and self.read(0) == '{')) {
                        if (last == real_start) {
                            out.data.text = self.script.src[last..self.pos.raw];
                            out.data.managed = false;
                        } else {
                            try buffer.appendSlice(self.script.src[last..self.pos.raw]);
                            out.data.text = try buffer.toOwnedSlice();
                        }

                        if (formatted) {
                            out.fmt = if (end) .end else .part;
                            if (end) self.fmt_string = false;
                        }

                        break;
                    } else if (self.read(0) == '\\') {
                        try buffer.appendSlice(self.script.src[last..self.pos.raw]);
                        last = self.nextPos().raw;

                        const c = self.parseEscapeSequence(exit) catch if (self.script.failed) return else continue;
                        unicode.writeCharUtf8(c, buffer.writer()) catch |e| switch (e) {
                            // handled in parseEscapeSequence
                            error.CannotEncodeSurrogateHalf,
                            error.CharTooLarge,
                            => unreachable,
                            else => |err| return err,
                        };
                    }
                }

                try self.addToken(.{ start, self.pos }, .{ .string = out });
            },
            '\'' => { // Character parser.
                const start = self.pos;
                var char: u21 = 0xFFFD;

                try self.advance();
                if (self.read(0) == '\'') {
                    try self.logger.log(.empty_char, .{ start, self.pos }, null);
                } else if (self.read(0) == '\\') {
                    char = self.parseEscapeSequence('\'') catch if (self.script.failed) return else 0xFFFD;
                    try self.advance();
                } else if (try self.read(1) == '\'') {
                    char = self.read(0);
                    try self.advance();
                }

                if (self.read(0) != '\'') {
                    while (self.read(0) != '\'') : (try self.advance())
                        if (isNewLine(try self.read(1)))
                            return self.logger.log(.char_no_end, .{ start, start }, null);

                    if (char != 0xFFFD) try self.logger.log(.char_too_large, .{ start, self.pos }, null);
                }

                try self.addToken(.{ start, self.pos }, .{ .char = char });
            },
            // Symbol or comment.
            '/' => if (try self.read(1) == '/' or try self.read(1) == '*') {
                var span = Span{ self.pos, self.pos };
                var start = self.pos;
                try self.advance();
                var doc = false;
                var out = Token.DocComment{};

                switch (self.read(0)) {
                    // Single-line comment.
                    '/' => {
                        if (isNewLine(try self.read(1))) continue else try self.advance();
                        out.top_level = self.read(0) == '!';
                        doc = out.top_level or self.read(0) == '/';
                        if (isNewLine(try self.read(1))) continue else try self.advance();

                        start = self.pos;
                        while (!isNewLine(try self.read(1))) try self.advance();
                        span[1] = self.pos;
                    },
                    // Multi-line comment.
                    '*' => {
                        try self.advance();
                        out.top_level = self.read(0) == '!';
                        doc = out.top_level or self.read(0) == '*';
                        try self.advance();

                        start = self.pos;
                        while (!(self.read(0) == '*' and try self.read(1) == '/')) : (try self.advance()) span[1] = self.pos;
                        try self.advance();
                    },
                    else => unreachable,
                }

                if (out.top_level and span[0].raw != 0) {
                    try self.logger.log(.misplaced_top_level_doc, span, null);
                    continue;
                }

                out.text = self.script.src[start.raw .. span[1].raw + 1];
                if (doc) try self.addToken(span, .{ .doc_comment = out });
            } else try self.addWord(true), // Symbol.
            // Symbol or end of format string expression.
            '}' => if (self.fmt_string) {
                try self.advance();
                continue :char '`';
            } else try self.addWord(true), // Symbol.
            else => if (!isSymbol(self.read(0))) {
                self.word.len += self.buf[0].len;
            } else try self.addWord(true), // Symbol.
        }
    }
}
