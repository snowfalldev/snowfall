const std = @import("std");
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;
const allocPrint = std.fmt.allocPrint;
const parseUnsigned = std.fmt.parseUnsigned;
const parseInt = std.fmt.parseInt;
const parseFloat = std.fmt.parseFloat;

const Script = @import("../Script.zig");
const BuiltinType = @import("../vm/types.zig").BuiltinType;

const util = @import("../util.zig");

const unicode = @import("../unicode.zig");
const Rune = unicode.Rune;
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
        fmt: ?enum { part, end } = null,
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
            .builtin_type => |v| try writer.print("built-in type: {s}", .{@tagName(v)}),
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

// MESSAGES

const log = @import("../log.zig");

// zig fmt: off
const message = log.simpleMessage(&.{
    // numbers
    .{ .fatal, .mismatched_bases,       "base {} digit in base {} number", struct { u8, u8 } },
    .{ .fatal, .unexpected_char_number, "unexpected character in number",  void },

    // escape sequences
    .{ .err,   .invalid_esc,   "invalid escape sequence",      void },
    .{ .fatal, .esc_no_start,  "escape sequence has no start", void },
    .{ .fatal, .esc_no_end,    "escape sequence has no end",   void },
    .{ .err,   .esc_too_large, "escape sequence is too large", void },
    .{ .err,   .non_hex_in_hex_esc,          "non-hex character in hex escape sequence",             void },
    .{ .err,   .underscore_in_short_hex_esc, "underscores not allowed in short hex escape sequence", void },

    // characters
    .{ .err,   .empty_char,     "character cannot be empty", void },
    .{ .fatal, .char_no_end,    "character has no end",      void },
    .{ .err,   .char_too_large, "character is too large",    void },

    // miscellaneous
    .{ .err, .misplaced_top_level_doc, "top-level doc comments must be at the start of the script", void },
    .{ .err, .identifier_too_long,     "identifier exceeds maximum length ({} bytes > 64 bytes)",   struct { usize } },
});
// zig fmt: on

pub const Message = message[0];
const Logger = log.Logger(Message, message[1]);

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

// MISCELLANEOUS UTILITIES

pub inline fn getLastToken(self: *Self) ?LocatedToken {
    if (self.output.items.len == 0) return null;
    return self.output.items[self.output.items.len - 1];
}

pub inline fn finished(self: Self) bool {
    return (self.pos.raw >= self.script.src.len or self.script.failed);
}

// IMPLEMENTATION

inline fn addToken(self: *Self, span: Span, token: Token) !void {
    try self.output.append(self.script.allocator(), .{ .span = span, .token = token });
}

fn addWord(self: *Self, comptime symbol: bool) !void {
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
            if (self.word.len > 0 and !isNumberChar(self.word[0])) {
                if (Token.Keyword.string_map.get(self.word)) |kw| break :blk .{ .keyword = kw };
                if (BuiltinType.string_map.get(self.word)) |t| break :blk .{ .builtin_type = t };
                break :blk .{ .identifier = self.word };
            } else break :blk null;
        };

        if (token) |tok| try self.addToken(span, tok);
    }

    if (symbol) try self.addToken(.{ self.pos, self.pos }, .{ .symbol = @truncate(self.buf[0].code) });
}

inline fn advanceRead(self: *Self) !void {
    defer self.buf[1] = Rune.decodeCursor(self.script.src, &self.cursor);
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
    self.pos.raw = self.buf[0].offset;
    self.pos.col += 1;

    if (!self.advanceCheck(self.buf[0].code)) return;
    // wont do anything if word len is 0, always call it
    try @call(.always_inline, addWord, .{ self, false });

    while (self.advanceCheck(self.buf[0].code)) {
        const code1 = self.buf[0].code;
        const new_line = isNewLine(code1);

        if (new_line) {
            const len = (self.pos.raw - self.row_start.raw) - 1;
            if (len > 65535) { // len must fit into a u16
                const args = .{ self.pos.row + 1, self.script.name, len };
                // we don't use the logger here because trying to print the huge row is a bad idea
                log.scoped.err("row {} in script {s} exceeded byte length limit ({} > 65535)", args);
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

        self.row_start = self.pos;
        try self.script.rows.append(self.script.allocator(), .{ .raw = @truncate(self.pos.raw + 1), .len = 0 });
    }
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
                if (!isNumberChar(self.buf[1].?.code)) break;
                out.float = true;
            },
            else => return self.logger.log(.unexpected_char_number, span, self.pos),
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

inline fn nextPos(self: Self) Pos {
    return .{
        .raw = self.pos.raw + 1,
        .row = self.pos.row,
        .col = self.pos.col + 1,
    };
}

fn parseEscapeSequence(self: *Self, exit: u21) !u21 {
    self.basic = false;
    defer self.basic = true;

    const esc_seq_start = self.pos;
    try self.advance();

    switch (self.buf[0].code) {
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
                chars[i] = @truncate(self.buf[0].code);
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

            if (self.buf[0].code != '{') {
                try self.logger.log(.esc_no_start, .{ esc_seq_start, self.pos }, self.pos);
                return error.InvalidCharacter;
            }

            if (!isNewLine(self.buf[1].?.code)) parse: {
                try self.advance(); // Advance past '{'.

                var idx: usize = 5;
                while (self.buf[0].code != '}') : (try self.advance()) {
                    if (self.buf[0].code == '_') continue;

                    if (!isHexadecimalChar(self.buf[0].code) or isNewLine(self.buf[1].?.code)) {
                        non_hex_pos = self.pos;
                        while (self.buf[0].code != '}') try self.advance();
                        break :parse;
                    }

                    if (idx == 0) {
                        while (self.buf[0].code != '}') try self.advance();
                        try self.logger.log(.esc_too_large, .{ esc_seq_start, self.pos }, null);
                        return error.Overflow;
                    } else idx -= 1;
                }

                return parseUnsigned(u21, self.script.src[esc_char_start.raw..self.pos.raw], 16) catch {
                    try self.logger.log(.esc_too_large, .{ esc_seq_start, self.pos }, null); // InvalidCharacter is unreachable.
                    return error.Overflow;
                };
            }

            if (isNewLine(self.buf[1].?.code)) {
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

pub fn next(self: *Self) !void {
    if (self.finished()) return;

    const last_token = self.getLastToken();
    const initial_len = self.output.items.len;

    while (initial_len == self.output.items.len and !self.script.failed) : (try self.advance()) {
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

                const allocator = self.script.allocator();
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
                            out.fmt = if (end) .end else .part;
                            if (end) self.fmt_string = false;
                        }

                        break;
                    } else if (self.buf[0].code == '\\') {
                        try buffer.appendSlice(self.script.src[last..self.pos.raw]);
                        const c = self.parseEscapeSequence(exit) catch if (self.script.failed) return else 0xFFFD;
                        try unicode.writeCharUtf8(c, buffer.writer());
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
                    char = self.parseEscapeSequence('\'') catch if (self.script.failed) return else 0xFFFD;
                    try self.advance();
                } else if (self.buf[1].?.code == '\'') {
                    char = self.buf[0].code;
                    try self.advance();
                }

                if (self.buf[0].code != '\'') {
                    while (self.buf[0].code != '\'') : (try self.advance())
                        if (isNewLine(self.buf[1].?.code))
                            return self.logger.log(.char_no_end, .{ start, start }, null);

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
            else => if (!isSymbol(self.buf[0].code)) {
                self.word.len += self.buf[0].len;
            } else try self.addWord(true), // Symbol.
        }
    }
}
