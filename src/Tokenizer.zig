const std = @import("std");
const assert = std.debug.assert;

const util = @import("util");

const Tokenizer = @This();
index: comptime_int = 0,

pub const operator_characters: []const u8 = &[_]u8{
    '!', '#', '$', '%', '&', '*',
    '+', '-', '/', '<', '=', '>',
    '?', '@', '~', '^', '|', ':',
};
pub const identifier_start_characters: []const u8 =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
    "abcdefghijklmnopqrstuvwxyz" ++
    "_";
pub const identifier_characters: []const u8 = identifier_start_characters ++ "0123456789";

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
    comptime tokenizer: Tokenizer,
    comptime buffer: []const u8,
) PeekRes {
    switch ((buffer ++ &[_:0]u8{})[tokenizer.index]) {
        0 => return .{
            .state = tokenizer,
            .token = .eof,
        },
        ' ', '\t', '\n', '\r' => {
            const whitespace_end = util.indexOfNonePosComptime(u8, buffer, tokenizer.index + 1, &.{ ' ', '\t', '\n', '\r' }) orelse buffer.len;
            var fwd_state = tokenizer;
            fwd_state.index = whitespace_end;
            return fwd_state.peekImpl(buffer);
        },
        ',' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .comma },
        '(' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .paren_open },
        ')' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .paren_close },
        '[' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .bracket_open },
        ']' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .bracket_close },
        'a'...'z',
        'A'...'Z',
        '_',
        => {
            const start = tokenizer.index;
            const end = util.indexOfNonePosComptime(u8, buffer, start + 1, identifier_characters) orelse buffer.len;
            const ident = util.dedupe.scalarSlice(u8, buffer[start..end].*);
            return .{
                .state = .{ .index = end },
                .token = .{ .ident = ident },
            };
        },
        '.' => {
            const start = tokenizer.index + 1;
            const end = util.indexOfNonePosComptime(u8, buffer, start + 1, identifier_characters ++ operator_characters) orelse buffer.len;
            const ident = util.dedupe.scalarSlice(u8, buffer[start..end].*);
            if (ident.len == 0) @compileError("Expected identifier following period");
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

            assert(zig_tok.loc.start == 0);
            const literal_src = util.dedupe.scalarSlice(u8, buffer[start..][zig_tok.loc.start..zig_tok.loc.end].*);

            return .{
                .state = .{ .index = start + literal_src.len },
                .token = .{ .number = literal_src },
            };
        },
        else => |first_byte| {
            const start = tokenizer.index;
            const end = util.indexOfNonePosComptime(u8, buffer, start + 1, operator_characters) orelse buffer.len;
            if (start == end) @compileError("Unexpected byte '" ++ &[_]u8{first_byte} ++ "'");
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
    comptime var tokenizer = Tokenizer{};
    inline for (expected) |expected_tok| {
        const actual_tok = tokenizer.next(buffer);
        try util.testing.expectEqualDeep(expected_tok, actual_tok);
    }
    try util.testing.expectEqualDeep(.eof, tokenizer.next(buffer));
    try util.testing.expectEqualDeep(.eof, tokenizer.next(buffer));
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
}
