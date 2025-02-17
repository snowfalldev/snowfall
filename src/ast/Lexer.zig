// A terrible lexer that does half the work a parser should do.
// Hopefully that'll mean the parser's easy to do.

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
const Logger = @import("../log.zig").Logger;
const NumberType = @import("../vm/value/number.zig").NumberType;
const BuiltinType = @import("../vm/type.zig").BuiltinType;

const util = @import("../util.zig");

const unicode = @import("../unicode.zig");
const isGap = unicode.isGap;
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

        loop,        // define infinite loop
        @"while",    // define while loop
        @"for",      // define for loop
        @"break",    // loop break
        @"continue", // loop continue

        @"async", // define function as async
        @"await", // await async function

        import, // import a script
        from, // cherry-pick imports

        pub const string_map = util.mkStringMap(Keyword);
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
};

pub const LocatedToken = struct { span: Span, token: Token };

pub const State = enum { string, char, number, none };

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

script: *Script,

arena: Arena,
allocator: Allocator,
output: std.ArrayList(LocatedToken),

pos: Pos = .{},
last: Pos = .{},
start: Pos = .{},

iter: code_point.Iterator,
buf: struct { CodePoint, ?CodePoint } = .{ undefined, null },
word: []const u8 = "",
basic: bool = true,
fmt_string: bool = false,

logger: Logger(.lexer),
failed: bool = false,

const Self = @This();

// INIT / DEINIT

/// Initializes a new lexer.
pub fn init(script: *Script) !Self {
    var lexer = Self{
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
    lexer.output = std.ArrayList(LocatedToken).init(lexer.allocator);
    lexer.logger = Logger(.lexer).init(lexer.allocator, script);

    return lexer;
}

/// De-initializes the lexer.
/// This should only be run after the output of the lexer is done being used.
pub inline fn deinit(self: Self) void {
    self.arena.deinit();
}

// MISCELLANEOUS UTILITIES

// Returns the last token the lexer outputted.
pub inline fn getLastToken(self: *Self) ?LocatedToken {
    if (self.output.items.len == 0) return null;
    return self.output.items[self.output.items.len - 1];
}

pub inline fn finished(self: Self) bool {
    return (self.pos.raw >= self.script.src.len or self.failed);
}

inline fn fatal(self: *Self, comptime fmt: []const u8, args: anytype, span: ?Span, hi: ?Pos) !void {
    try self.logger.err(fmt, args, span, hi);
    self.failed = true;
}

// IMPLEMENTATION

// Adds the current word to the output.
fn addWord(self: *Self, comptime symbol: bool) !void {
    if (self.word.len != 0) {
        self.word.ptr = self.script.src.ptr + self.start.raw;

        const tok: ?Token = blk: {
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

        if (tok) |token| try self.output.append(.{ .span = .{ self.start, self.last }, .token = token });

        self.word.len = 0;
    }

    if (symbol) try self.output.append(.{
        .span = .{ self.pos, self.pos },
        .token = .{ .symbol = @truncate(self.buf[0].code) },
    });
}

// Returns true if we've hit EOF.
inline fn advanceRead(self: *Self) !bool {
    self.buf[0] = self.buf[1] orelse {
        // no next char, we're done
        self.pos.raw = self.script.src.len;
        self.pos.col += 1;
        return true;
    };

    self.buf[1] = self.iter.next();
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
            self.pos.raw = self.script.src.len;
            self.pos.col += 1;
        }

        return;
    }

    self.last = self.pos;
    self.pos.col += 1;

    self.buf[1] = self.iter.next();

    if (isGap(self.buf[0].code)) {
        if (self.basic) try self.addWord(false);

        while (isGap(self.buf[0].code)) {
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
    if (self.word.len == 0) self.start = self.pos;
}

// Validates and tokenizes a possible number.
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
    var type_char: ?CodePoint = null;

    if (signed) {
        state = NumberType.i64;
        try self.advance();
    }

    while (!self.finished()) : (try self.advance()) {
        num: switch (self.buf[0].code) {
            '0'...'9', '_' => {},
            'f' => continue :num if (out.base == 10) 'i' else 'F',
            'n', 'i', 'u' => {
                type_char = self.buf[0];
                break;
            },
            'a'...'d', 'A'...'D', 'F' => if (out.base != 16)
                return self.fatal("hex char in base {} number", .{out.base}, span, self.pos),
            'e', 'E' => switch (out.base) {
                16 => {},
                10 => if (state == .f64) {
                    if (!exponent_reached) {
                        exponent_reached = true;
                    } else return self.fatal("hex char in float exponent", .{}, span, self.pos);
                } else return self.fatal("hex char in base 10 number", .{}, span, self.pos),
                else => return self.fatal("hex char in base {} number", .{out.base}, span, self.pos),
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
            else => return self.fatal("unexpected character in number", .{}, span, self.pos),
        }

        span[1] = self.pos;

        if (exponent_reached and (self.buf[1].?.code == '+' or self.buf[1].?.code == '-')) continue;

        if (isSymbol(self.buf[1].?.code, false) or isGap(self.buf[1].?.code)) {
            try self.advance();
            break;
        }
    }

    out.buf = self.script.src[after_base .. span[1].raw + 1];

    if (type_char) |char| get_type: {
        const start = self.pos;

        while (!self.finished()) : (try self.advance()) {
            if (isSymbol(self.buf[1].?.code, true) or isGap(self.buf[1].?.code)) {
                span[1] = self.pos;
                try self.advance();
                break;
            }
        }

        const type_string = self.script.src[start.raw .. span[1].raw + 1];

        if (type_string.len > 1) {
            out.typ = NumberType.string_map.get(type_string);
            if (out.typ == null) {
                try self.logger.err("unknown type", .{}, .{ start, span[1] }, null);
                break :get_type;
            }
        } else out.typ = switch (char.code) {
            'n' => .bigint,
            'i' => .i64,
            'u' => .u64,
            'f' => .f64,
            else => unreachable,
        };

        if ((char.code == 'i' or char.code == 'n') and state == .f64) {
            try self.logger.err("floating-point numbers cannot become signed integers", .{}, span, start);
        } else if (char.code == 'u') {
            if (state == .i64 and out.buf[0] == '-') {
                try self.logger.err("unsigned integers cannot be negative", .{}, span, start);
            } else if (state == .f64) {
                try self.logger.err("floating-point numbers cannot become unsigned integers", .{}, span, start);
            }
        }
    }

    try self.output.append(.{ .span = span, .token = .{ .number = out } });
}

fn parseEscapeSequence(self: *Self, exit: u8) !u21 {
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

            if (self.pos.raw + 1 + 2 >= self.script.src.len)
                return error.UnexpectedEOF;

            const hex_start = self.pos;
            const chars: [2]u8 = .{
                @truncate(self.buf[0].code),
                @truncate(self.buf[1].?.code),
            };
            try self.advance();

            return parseUnsigned(u8, &chars, 16) catch {
                try self.logger.err("bad hexadecimal number", .{}, .{ hex_start, self.pos }, null); // Overflow is unreachable.
                return 0xFFFD;
            };
        },
        'u' => {
            try self.advance(); // Advance past 'u'.

            var fail_pos = self.pos;

            if (self.buf[0].code == '{') parse: {
                try self.advance(); // Advance past '{'.

                const esc_char_start = self.pos;
                var idx: usize = 5;

                while (true) : (try self.advance()) {
                    if (self.buf[0].code == '}') break;

                    if (!(isHexadecimalChar(self.buf[0].code) or self.buf[0].code == '_')) {
                        fail_pos = self.pos;
                        break :parse;
                    } else if (isNewLine(self.buf[1].?.code)) {
                        fail_pos = self.pos.next();
                        try self.advance();
                        break :parse;
                    }

                    if (idx == 0) {
                        while (!self.finished() and self.buf[0].code != '}') try self.advance();
                        try self.logger.err("escape sequence is too large", .{}, .{ esc_seq_start, self.pos }, null);
                        return 0xFFFD;
                    } else idx -|= 1;
                }

                const char = parseUnsigned(u24, self.script.src[esc_char_start.raw..self.pos.raw], 16) catch unreachable;
                if (char > 0x10FFFF) {
                    try self.logger.err("escape sequence is too large", .{}, .{ esc_seq_start, self.pos }, null);
                    return 0xFFFD;
                } else return @truncate(char);
            }

            try self.logger.err("invalid escape sequence", .{}, .{ esc_seq_start, fail_pos }, fail_pos);
            return 0xFFFD;
        },
        else => |char| if (char == exit) return char,
    }

    try self.logger.err("invalid escape sequence", .{}, .{ esc_seq_start, self.pos }, self.pos);
    return 0xFFFD;
}

// Lex characters into the next token(s).
pub fn next(self: *Self) !void {
    if (self.finished()) return;

    const initial_len = self.output.items.len;
    const last_token = self.getLastToken();

    while (initial_len == self.output.items.len) : (try self.advance()) {
        if (self.word.len == 0) {
            var signed_number = (self.buf[0].code == '-' or self.buf[0].code == '+') and isNumberChar(self.buf[1].?.code);
            // if the last token was an expression, it's arithmetic. otherwise, it's a signed number.
            if (signed_number and last_token != null) {
                signed_number = switch (last_token.?.token) {
                    // end of function call or expression in parenthesis
                    .symbol => |symbol| symbol == ')',
                    .keyword, .doc_comment => false,
                    else => true,
                };
            }

            if (isNumberChar(self.buf[0].code) or signed_number)
                return self.tokenizeNumber(signed_number);
        }

        char: switch (self.buf[0].code) {
            '"', '`' => |e| { // String parser.
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
                    try self.logger.err("empty characters not allowed", .{}, .{ start, self.pos }, null);
                } else if (self.buf[0].code == '\\') {
                    char = try self.parseEscapeSequence('\'');
                    try self.advance();
                } else if (self.buf[1].?.code == '\'') {
                    char = self.buf[0].code;
                    try self.advance();
                }

                if (self.buf[0].code != '\'') {
                    while (!self.finished() and self.buf[0].code != '\'' and !isNewLine(self.buf[1].?.code)) try self.advance();

                    if (self.buf[0].code == '\'') {
                        try self.fatal("character is too large", .{}, .{ start, self.pos }, null);
                    } else {
                        try self.fatal("character has no end", .{}, .{ start, start }, null);
                    }

                    try self.logger.info("strings are defined with double quotes", .{}, null, null);
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
                    return self.fatal("top-level doc comment must be at the start of the file", .{}, span, null);

                out.text = self.script.src[start.raw .. span[1].raw + 1];

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

        if (self.failed) return;
    }
}
