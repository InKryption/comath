const std = @import("std");
const util = @import("util");

const Tokenizer = @This();
index: comptime_int,

pub const init: Tokenizer = .{
    .index = 0,
};

/// The set of characters which are used to separate meaningful tokens.
pub const whitespace_characters = [_]u8{ ' ', '\t', '\n' };

/// The set of characters which are valid for use as part of an operator.
pub const operator_characters = [_]u8{
    '!', '#', '$', '%', '&', '*',
    '+', '-', '/', '<', '=', '>',
    '?', '@', '~', '^', '|', ':',
};

/// The set of characters which can be used to start and compose an identifier.
pub const identifier_start_characters: [26 * 2 + 1]u8 =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ".* ++
    "abcdefghijklmnopqrstuvwxyz".* ++
    "_".* //
;

/// The set of characters which can be used to compose an identifier, though only a subset
/// can be used to start an identifier.
pub const identifier_characters: [identifier_start_characters.len + 10]u8 =
    identifier_start_characters ++
    "0123456789".* //
;

/// The set of characters which are valid after a period ('.').
pub const field_access_characters = operator_characters ++ identifier_characters;

pub const Token = union(enum) {
    /// no more tokens left
    eof,

    ident: []const u8,
    /// '.' field
    field: []const u8,
    number: []const u8,
    op_symbols: []const u8,

    /// ','
    comma,
    /// '('
    paren_open,
    /// ')'
    paren_close,
    /// '['
    bracket_open,
    /// ']'
    bracket_close,

    /// error while tokenizing
    err: Err,

    pub const Err = union(enum) {
        empty_field_access,
        unexpected_byte: u8,
    };

    pub inline fn eql(comptime a: Token, comptime b: Token) bool {
        comptime if (std.meta.activeTag(a) != b) return false;
        comptime return switch (a) {
            .eof,
            .comma,
            .paren_open,
            .paren_close,
            .bracket_open,
            .bracket_close,
            => true,

            inline //
            .ident,
            .field,
            .number,
            .op_symbols,
            => |str, tag| util.eqlComptime(u8, str, @field(b, @tagName(tag))),

            .err => |a_err| std.meta.activeTag(a_err) == b.err and switch (a_err) {
                .empty_field_access => true,
                .unexpected_byte => |a_byte| a_byte == b.err.unexpected_byte,
            },
        };
    }

    pub inline fn format(
        comptime self: Token,
        comptime fmt_str: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = options;
        if (fmt_str.len != 0) comptime {
            std.fmt.invalidFmtError(fmt_str, self);
        };

        switch (self) {
            inline //
            .ident,
            .field,
            .number,
            .op_symbols,
            => |str, tag| try writer.writeAll(comptime std.fmt.comptimePrint("{s}({s})", .{ @tagName(tag), str })),

            .eof,
            .comma,
            .paren_open,
            .paren_close,
            .bracket_open,
            .bracket_close,
            => try writer.writeAll(@tagName(self)),

            .err => |err| switch (err) {
                .empty_field_access => |_| try writer.writeAll(comptime std.fmt.comptimePrint("{s}({s})", .{ @tagName(self), @tagName(err) })),
                .unexpected_byte => |byte| try writer.writeAll(comptime std.fmt.comptimePrint("{s}({s}('{'}'))", .{ @tagName(self), @tagName(err), std.zig.fmtEscapes(@as(*const [1]u8, &byte)) })),
            },
        }
    }
};

pub inline fn next(
    comptime tokenizer: *Tokenizer,
    comptime buffer: []const u8,
) Token {
    comptime {
        const result = tokenizer.peekImpl(util.dedupe.scalarSlice(u8, buffer[0..].*));
        tokenizer.* = result.state;
        return result.token;
    }
}

pub inline fn peek(
    comptime tokenizer: Tokenizer,
    comptime buffer: []const u8,
) Token {
    const result = tokenizer.peekImpl(util.dedupe.scalarSlice(u8, buffer[0..].*));
    return result.token;
}

const PeekRes = struct {
    token: Token,
    state: Tokenizer,
};
fn peekImpl(
    comptime tokenizer_init: Tokenizer,
    comptime buffer: []const u8,
) PeekRes {
    if (!@inComptime()) comptime unreachable;

    const tokenizer = blk: {
        var tokenizer = tokenizer_init;
        switch ((buffer ++ &[_:0]u8{})[tokenizer.index]) {
            ' ', '\t', '\n' => {
                const whitespace_end = util.indexOfNonePosComptime(u8, buffer[0..].*, tokenizer.index + 1, whitespace_characters[0..].*) orelse buffer.len;
                tokenizer.index = whitespace_end;
            },
            else => {},
        }
        break :blk tokenizer;
    };

    switch ((buffer ++ &[_:0]u8{})[tokenizer.index]) {
        0 => |sentinel| {
            if (tokenizer_init.index != buffer.len) return .{
                .state = .{ .index = tokenizer.index + 1 },
                .token = .{ .err = .{ .unexpected_byte = sentinel } },
            };
            return .{
                .state = tokenizer,
                .token = .eof,
            };
        },

        ' ',
        '\t',
        '\n',
        => unreachable,

        ',', '(', ')', '[', ']' => |char| return .{
            .state = .{ .index = tokenizer.index + 1 },
            .token = switch (char) {
                ',' => .comma,
                '(' => .paren_open,
                ')' => .paren_close,
                '[' => .bracket_open,
                ']' => .bracket_close,
                else => unreachable,
            },
        },

        'a'...'z',
        'A'...'Z',
        '_',
        => {
            const start = tokenizer.index;
            const end = util.indexOfNonePosComptime(u8, buffer[0..].*, start + 1, identifier_characters) orelse buffer.len;
            const ident = util.dedupe.scalarSlice(u8, buffer[start..end].*);
            return .{
                .state = .{ .index = end },
                .token = .{ .ident = ident },
            };
        },
        '.' => {
            const start = util.indexOfNonePosComptime(u8, buffer[0..].*, tokenizer.index + 1, whitespace_characters[0..].*) orelse buffer.len;
            const end = util.indexOfNonePosComptime(u8, buffer[0..].*, @min(buffer.len, start + 1), field_access_characters) orelse buffer.len;
            const ident = util.dedupe.scalarSlice(u8, buffer[start..end].*);
            if (ident.len == 0 or
                !util.containsScalarComptime(u8, field_access_characters, ident[0]) //
            ) return .{
                .state = .{ .index = start },
                .token = .{ .err = .empty_field_access },
            };
            return .{
                .state = .{ .index = end },
                .token = .{ .field = ident },
            };
        },
        '0'...'9' => {
            const start = tokenizer.index;
            @setEvalBranchQuota(@min(std.math.maxInt(u32), (buffer.len - start) * 100));
            var zig_tokenizer = std.zig.Tokenizer.init(buffer[start..] ++ &[_:0]u8{});
            const zig_tok = zig_tokenizer.next();

            if (zig_tok.loc.start != 0) unreachable;
            const literal_src = util.dedupe.scalarSlice(u8, buffer[start..][zig_tok.loc.start..zig_tok.loc.end].*);

            return .{
                .state = .{ .index = start + literal_src.len },
                .token = .{ .number = literal_src },
            };
        },
        else => |first_byte| {
            const start = tokenizer.index;
            const end = util.indexOfNonePosComptime(u8, buffer[0..].*, start, operator_characters[0..].*) orelse buffer.len;
            if (start == end) return .{
                .state = .{ .index = end + 1 },
                .token = .{ .err = .{ .unexpected_byte = first_byte } },
            };
            return .{
                .state = .{ .index = end },
                .token = .{ .op_symbols = util.dedupe.scalarSlice(u8, buffer[start..end].*) },
            };
        },
    }
    return null;
}

fn testTokenizer(
    comptime buffer: []const u8,
    comptime expected: []const Token,
) !void {
    comptime var tokenizer: Tokenizer = .init;
    inline for (expected) |expected_tok| {
        const actual_tok = tokenizer.next(buffer);
        if (actual_tok.eql(expected_tok)) continue;
        std.log.err("Expected '{}', found '{}'", .{ expected_tok, actual_tok });
        return error.TestExpectedEqual;
    }
    const expected_tok: Token = .eof;
    const actual_tok = tokenizer.next(buffer);
    if (!expected_tok.eql(actual_tok)) {
        std.log.err("Expected '{}', found '{}'", .{ expected_tok, actual_tok });
        return error.TestExpectedEqual;
    }
}

test Tokenizer {
    try testTokenizer(
        \\~      3.0    +   3       -
        \\-      !      (   0b1a1   ]
        \\*      )      /   [       ]
        \\^      ]      |   a_b_C   %
        \\.foo   .b@r   .?
    ,
        &.{
            .{ .op_symbols = "~" }, .{ .number = "3.0" },     .{ .op_symbols = "+" }, .{ .number = "3" },      .{ .op_symbols = "-" },
            .{ .op_symbols = "-" }, .{ .op_symbols = "!" },   .{ .paren_open = {} },  .{ .number = "0b1a1" },  .{ .bracket_close = {} },
            .{ .op_symbols = "*" }, .{ .paren_close = {} },   .{ .op_symbols = "/" }, .{ .bracket_open = {} }, .{ .bracket_close = {} },
            .{ .op_symbols = "^" }, .{ .bracket_close = {} }, .{ .op_symbols = "|" }, .{ .ident = "a_b_C" },   .{ .op_symbols = "%" },
            .{ .field = "foo" },    .{ .field = "b@r" },      .{ .field = "?" },
        },
    );

    try testTokenizer("(x - 2) * (3 + ~y)", &.{
        .paren_open,
        .{ .ident = "x" },
        .{ .op_symbols = "-" },
        .{ .number = "2" },
        .paren_close,
        .{ .op_symbols = "*" },
        .paren_open,
        .{ .number = "3" },
        .{ .op_symbols = "+" },
        .{ .op_symbols = "~" },
        .{ .ident = "y" },
        .paren_close,
    });

    try testTokenizer("x+-3", &.{
        .{ .ident = "x" },
        .{ .op_symbols = "+-" },
        .{ .number = "3" },
    });
    try testTokenizer("x+ -3", &.{
        .{ .ident = "x" },
        .{ .op_symbols = "+" },
        .{ .op_symbols = "-" },
        .{ .number = "3" },
    });
    try testTokenizer("a.+ + 36", &.{
        .{ .ident = "a" },
        .{ .field = "+" },
        .{ .op_symbols = "+" },
        .{ .number = "36" },
    });
    try testTokenizer("a.++ 36", &.{
        .{ .ident = "a" },
        .{ .field = "++" },
        .{ .number = "36" },
    });
    try testTokenizer("a.++36", &.{
        .{ .ident = "a" },
        .{ .field = "++36" },
    });
    try testTokenizer("a. ++36", &.{
        .{ .ident = "a" },
        .{ .field = "++36" },
    });
    try testTokenizer("a.  (", &.{
        .{ .ident = "a" },
        .{ .err = .empty_field_access },
        .{ .paren_open = {} },
    });
    try testTokenizer("a.  )  + a", &.{
        .{ .ident = "a" },
        .{ .err = .empty_field_access },
        .{ .paren_close = {} },
        .{ .op_symbols = "+" },
        .{ .ident = "a" },
    });
    try testTokenizer("\x00", &.{
        .{ .err = .{ .unexpected_byte = '\x00' } },
    });
    try testTokenizer("\x00(+\xff", &.{
        .{ .err = .{ .unexpected_byte = '\x00' } },
        .{ .paren_open = {} },
        .{ .op_symbols = "+" },
        .{ .err = .{ .unexpected_byte = '\xff' } },
    });
}
