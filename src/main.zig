const std = @import("std");
const assert = std.debug.assert;

const Seeked = enum {
    none,
    paren,
    bracket,
};
const EvalImplUpdate = struct {
    tokenizer: Tokenizer,
};
fn evalImpl(
    comptime T: type,
    comptime expr: []const u8,
    inputs: anytype,
) !T {
    _ = inputs;
    _ = expr;
    comptime var tokenizer = Tokenizer{};

    inline while (tokenizer.next()) |tok| {
        switch (tok) {
            .ident => {},
            .field => {},
            .integer => {},
            .char => {},
            .float => {},
            .op => {},
            .comma => {},
            .paren_open => {},
            .paren_close => {},
            .bracket_open => {},
            .bracket_close => {},
        }
    }
}

inline fn parseExpr(comptime expr: []const u8) ExprUnion {
    comptime return parseExprImpl(dedupeSlice(u8, expr));
}

test parseExpr {
    @compileLog(parseExpr("(3 + a)").group);
}

fn binOp(
    comptime lhs: ExprUnion,
    comptime op: Operator,
    comptime rhs: ExprUnion,
) ExprUnion {
    return .{ .bin_op = .{
        .lhs = &lhs,
        .op = op,
        .rhs = &rhs,
    } };
}
fn unOp(comptime op: Operator, comptime val: ExprUnion) ExprUnion {
    return .{ .un_op = .{
        .op = op,
        .val = &val,
    } };
}

const NestType = enum { paren, bracket };

fn parseExprImpl(
    comptime expr: []const u8,
) ExprUnion {
    if (!@inComptime()) comptime unreachable;
    comptime {
        var stack: BoundedPackedStack(128, NestType) = .{};
        var result: ExprUnion = .null;
        var tokenizer = Tokenizer{};
        while (tokenizer.next(expr)) |tok| {
            const res_copy = result;
            switch (tok) {
                inline //
                .ident,
                .integer,
                .char,
                .float,
                => |val, tag| {
                    const new_node = @unionInit(ExprUnion, @tagName(tag), val);
                    switch (res_copy) {
                        .null => {
                            result = new_node;
                            continue;
                        },
                        .bin_op => |bin_op| switch (bin_op.rhs.*) {
                            .null => {
                                result = binOp(bin_op.lhs.*, bin_op.op, new_node);
                                continue;
                            },
                            else => {},
                        },
                        .un_op => |un_op| switch (un_op.val.*) {
                            .null => {
                                result = unOp(un_op.op, new_node);
                                continue;
                            },
                            else => {},
                        },

                        .ident,
                        .float,
                        .integer,
                        .char,
                        .group,
                        .field_access,
                        => {},
                    }
                    const fmt_str = switch (tag) {
                        .ident => "s",
                        .char => "u",
                        .integer, .float => "d",
                    };
                    @compileError(std.fmt.comptimePrint("Unexpected token '{" ++ fmt_str ++ "}'", .{val}));
                },
                .field => |field| switch (res_copy) {
                    .null => @compileError("Unexpected token '." ++ field ++ "'"),
                    .integer,
                    .char,
                    .float,
                    .ident,
                    .field_access,
                    .group,
                    => result = .{ .field_access = .{
                        .accessed = &res_copy,
                        .accessor = field,
                    } },
                    .bin_op => |bin_op| result = binOp(bin_op.lhs.*, bin_op.op, .{ .field_access = .{
                        .accessed = &(bin_op.rhs.*),
                        .accessor = field,
                    } }),
                    .un_op => |un_op| result = unOp(un_op.op, .{ .field_access = .{
                        .accessed = &(un_op.val.*),
                        .accessor = field,
                    } }),
                },

                .bin_op, .un_op => |op| switch (res_copy) {
                    .null => result = unOp(op, .null),
                    .bin_op => |bin_op| switch (bin_op.rhs.*) {
                        .null => result = binOp(bin_op.lhs.*, bin_op.op, .{ .un_op = unOp(op, .null) }),
                        else => @compileError("TODO: " ++ @tagName(res_copy)),
                    },
                    .ident,
                    .integer,
                    .char,
                    .float,
                    .group,
                    .field_access,
                    .un_op,
                    => result = binOp(res_copy, op, .null),
                },
                .paren_open => {
                    stack.push(.paren) catch @compileError("Nesting is too deep");
                    switch (result) {
                        .null => {},
                        .ident,
                        .field_access,
                        .integer,
                        .char,
                        .float,
                        .group,
                        .bin_op,
                        .un_op,
                        => @compileError("TODO: " ++ @tagName(res_copy)),
                    }
                },
                .paren_close => {
                    const prev = stack.pop() orelse @compileError("Unexpected closing parentheses");
                    if (prev != .paren) @compileError("Unexpected closing parentheses");
                    result = .{ .group = &res_copy };
                },
                .bracket_open => @compileError("TODO: " ++ @tagName(res_copy)),
                .bracket_close => @compileError("TODO: " ++ @tagName(res_copy)),
            }
        }
        return result;
    }
}

const ExprUnion = union(enum) {
    null,
    ident: []const u8,
    field_access: struct {
        accessed: *const ExprUnion,
        accessor: []const u8,
    },
    integer: comptime_int,
    char: comptime_int,
    float: []const u8,
    group: *const ExprUnion,
    bin_op: struct {
        lhs: *const ExprUnion,
        op: Operator,
        rhs: *const ExprUnion,
    },
    un_op: struct {
        op: Operator,
        val: *const ExprUnion,
    },
};

const Operator = enum {
    @"~",
    @"^",
    @"|",
    @"&",

    @"+",
    @"-",
    @"*",
    @"/",
    @"%",

    @"!",
    @"==",
    @"!=",
    @"<",
    @">",
    @"<=",
    @">=",
};

const Token = union(enum) {
    ident: []const u8,
    /// '.' field
    field: []const u8,
    integer: comptime_int,
    char: comptime_int,
    float: []const u8,
    bin_op: Operator,
    un_op: Operator,

    /// '('
    paren_open,
    /// ')'
    paren_close,
    /// '['
    bracket_open,
    /// ']'
    bracket_close,
};

const Tokenizer = struct {
    index: comptime_int = 0,
    can_be_unary: bool = true,

    inline fn next(comptime state: *Tokenizer, comptime buffer: []const u8) ?Token {
        comptime return state.nextImpl(buffer);
    }
    fn nextImpl(comptime tokenizer: *Tokenizer, comptime buffer: []const u8) ?Token {
        const peek_res = tokenizer.peekImpl(buffer) orelse return null;
        tokenizer.* = peek_res.state;
        return peek_res.token;
    }

    inline fn peek(comptime state: Tokenizer, comptime buffer: []const u8) ?Token {
        const result = state.peekImpl(dedupeSlice(u8, buffer)) orelse return null;
        return result.token;
    }
    const PeekRes = struct {
        token: Token,
        state: Tokenizer,
    };

    fn peekImpl(comptime state: Tokenizer, comptime buffer: []const u8) ?PeekRes {
        var idx = state.index;
        while (idx < buffer.len) switch (buffer[idx]) {
            ' ', '\t', '\n', '\r' => {
                idx += 1;
                continue;
            },
            '(' => return .{ .state = .{ .index = idx + 1, .can_be_unary = true }, .token = .paren_open },
            ')' => return .{ .state = .{ .index = idx + 1, .can_be_unary = false }, .token = .paren_close },
            '[' => return .{ .state = .{ .index = idx + 1, .can_be_unary = true }, .token = .bracket_open },
            ']' => return .{ .state = .{ .index = idx + 1, .can_be_unary = false }, .token = .bracket_close },
            'a'...'z',
            'A'...'Z',
            '_',
            => {
                const start = idx;
                const end = endOfIdent(buffer, start);
                const ident = buffer[start..end];
                return .{
                    .state = .{ .index = idx + ident.len, .can_be_unary = false },
                    .token = .{ .ident = ident },
                };
            },
            '.' => {
                idx += 1;
                const start = idx;
                const end = endOfIdent(buffer, start);
                const ident = buffer[start..end];
                if (ident.len == 0) @compileError("Expected identifier following period");
                return .{
                    .state = .{ .index = idx + ident.len, .can_be_unary = false },
                    .token = .{ .field = ident },
                };
            },
            '0'...'9', '\'' => {
                @setEvalBranchQuota(@min(std.math.maxInt(u32), buffer.len - idx));
                var zig_tokenizer = std.zig.Tokenizer.init(buffer ++ &[_:0]u8{});
                zig_tokenizer.index = idx;
                const tok = zig_tokenizer.next();

                const literal_src = buffer[tok.loc.start..tok.loc.end];
                assert(literal_src.len != 0);
                idx += literal_src.len;

                @setEvalBranchQuota(@min(std.math.maxInt(u32), buffer.len - idx));
                const literal_tok: Token = switch (tok.tag) {
                    .char_literal => Token{ .char = parseCharLiteral(literal_src[0..].*) },
                    .number_literal => switch (std.zig.parseNumberLiteral(literal_src)) {
                        .int => |int| Token{ .integer = int },
                        .big_int => |base| Token{ .integer = blk: {
                            const digits = literal_src[if (base == .decimal) 0 else 2..].*;
                            break :blk parseComptimeIntDigits(@intFromEnum(base), digits) catch |err| @compileError(
                                "Encountered '" ++ @errorName(err) ++ "' while parsing '" ++ literal_src ++ "'",
                            );
                        } },
                        .float => Token{ .float = literal_src },
                        .failure => |failure| handleNumberParseFailure(literal_src, failure),
                    },
                    else => @compileError( //
                        "Unexpected token '" ++ literal_src ++ //
                        "' with tag '" ++ @tagName(tok.tag) ++ "'" //
                    ),
                };
                return .{
                    .state = .{ .index = idx, .can_be_unary = false },
                    .token = literal_tok,
                };
            },
            else => |c| {
                if (!containsComptime(u8, enumTagNameCharSet(Operator), c)) @compileError(
                    std.fmt.comptimePrint("Unexpected character '{'}'", .{std.zig.fmtEscapes(&.{c})}),
                );
                const start = idx;
                @setEvalBranchQuota(@min(std.math.maxInt(u32), (buffer.len - idx) * 100));
                const end = indexOfNonePosComptime(
                    u8,
                    buffer,
                    start,
                    enumTagNameCharSet(Operator),
                ) orelse buffer.len;
                const str = buffer[start..end];
                idx += str.len;
                if (!@hasField(Operator, str)) @compileError("Unexpected token '" ++ str ++ "'");
                const op = @field(Operator, str);
                return .{
                    .state = .{ .index = idx, .can_be_unary = true },
                    .token = @unionInit(Token, if (state.can_be_unary) "un_op" else "bin_op", op),
                };
            },
        };
        return null;
    }
    inline fn endOfIdent(
        comptime buffer: []const u8,
        comptime start: comptime_int,
    ) comptime_int {
        return indexOfNonePosComptime(u8, buffer, start + 1, //
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++ //
            "abcdefghijklmnopqrstuvwxyz" ++ //
            "0123456789" ++ "_" //
        ) orelse buffer.len;
    }
};

fn testTokenizer(comptime buffer: []const u8, comptime expected: []const ?Token) !void {
    comptime var tokenizer = Tokenizer{};
    inline for (expected) |tok| {
        try std.testing.expectEqualDeep(tok, tokenizer.next(buffer));
    }
}

test Tokenizer {
    try testTokenizer(
        \\~   3.0     +    3
        \\-   !       (    'a'
        \\)   *       ]    /
        \\[   ==      !=   <
        \\>   <=      >=   ^
        \\|   a_b_C   &
    , &.{
        .{ .un_op = .@"~" },     .{ .float = "3.0" },   .{ .bin_op = .@"+" },     .{ .integer = 3 },
        .{ .bin_op = .@"-" },    .{ .un_op = .@"!" },   .{ .paren_open = {} },    .{ .char = 'a' },
        .{ .paren_close = {} },  .{ .bin_op = .@"*" },  .{ .bracket_close = {} }, .{ .bin_op = .@"/" },
        .{ .bracket_open = {} }, .{ .un_op = .@"==" },  .{ .un_op = .@"!=" },     .{ .un_op = .@"<" },
        .{ .un_op = .@">" },     .{ .un_op = .@"<=" },  .{ .un_op = .@">=" },     .{ .un_op = .@"^" },
        .{ .un_op = .@"|" },     .{ .ident = "a_b_C" }, .{ .bin_op = .@"&" },     null,
    });

    try testTokenizer("(x - 2) * (3 + ~y)", &.{
        .paren_open,
        .{ .ident = "x" },
        .{ .bin_op = .@"-" },
        .{ .integer = 2 },
        .paren_close,
        .{ .bin_op = .@"*" },
        .paren_open,
        .{ .integer = 3 },
        .{ .bin_op = .@"+" },
        .{ .un_op = .@"~" },
        .{ .ident = "y" },
        .paren_close,
    });
}

inline fn handleStringParseFailure(char_src: []const u8, failure: std.zig.string_literal.Error) noreturn {
    switch (failure) {
        .invalid_escape_character => |idx| @compileError(std.fmt.comptimePrint(
            "Unrecognized escape character '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .expected_hex_digit => |idx| @compileError(std.fmt.comptimePrint(
            "Expected hex digit, got '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .empty_unicode_escape_sequence => @compileError("Found empty unicode escape sequence in " ++ char_src),
        .expected_hex_digit_or_rbrace => |idx| @compileError(std.fmt.comptimePrint(
            "Expected hex digit or rbrace, found '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .invalid_unicode_codepoint => @compileError("Invalid unicode codepoint in " ++ char_src),
        .expected_lbrace => |idx| @compileError(std.fmt.comptimePrint(
            "Expected lbrace, found '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .expected_rbrace => |idx| @compileError(std.fmt.comptimePrint(
            "Expected rbrace, found '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .expected_single_quote => @compileError("Unescaped single quote in " ++ char_src),
        .invalid_character => |idx| @compileError(std.fmt.comptimePrint(
            "Invalid character '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
    }
}

inline fn handleNumberParseFailure(num_src: []const u8, failure: std.zig.number_literal.Error) noreturn {
    switch (failure) {
        .leading_zero => @compileError("Invalid leading zeroes in '" ++ num_src ++ "'"),
        .digit_after_base => @compileError("Expected digit after base in '" ++ num_src ++ "'"),
        .upper_case_base, .invalid_float_base => @compileError("Invalid base in '" ++ num_src ++ "'"),
        .repeated_underscore, .invalid_underscore_after_special => @compileError("Invalid underscore in '" ++ num_src ++ "'"),
        .invalid_digit => |info| @compileError(std.fmt.comptimePrint(
            "Invalid digit '{c}' in '{s}' with base '{s}'",
            .{ num_src[info.i], num_src, @tagName(info.base) },
        )),
        .invalid_digit_exponent => |exp_idx| @compileError(std.fmt.comptimePrint(
            "Invalid exponent '{c}' in '{s}'",
            .{ num_src[exp_idx], num_src },
        )),
        .duplicate_period => @compileError("Duplicate periods in '" ++ num_src ++ "'"),
        .duplicate_exponent => @compileError("Duplicate exponents in '" ++ num_src ++ "'"),
        .exponent_after_underscore => @compileError("Exponent after underscore in '" ++ num_src ++ "'"),
        .special_after_underscore => |spec_idx| @compileError(std.fmt.comptimePrint(
            "Invalid '{c}' after underscore in '{s}'",
            .{ num_src[spec_idx], num_src },
        )),
        .trailing_special,
        .trailing_underscore,
        .invalid_character,
        .invalid_exponent_sign,
        => |err_idx| @compileError(std.fmt.comptimePrint(
            "Invalid '{c}' in '{s}'",
            .{ num_src[err_idx], num_src },
        )),
    }
}

inline fn parseCharLiteral(comptime char_src: anytype) comptime_int {
    assert(@TypeOf(char_src) == [char_src.len]u8);
    assert('\'' == char_src[0]);
    assert('\'' == char_src[char_src.len - 1]);
    if (char_src.len < 3) @compileError("Invalid empty character '" ++ &char_src ++ "'");
    return switch (std.zig.parseCharLiteral(&char_src)) {
        .success => |codepoint| codepoint,
        .failure => |failure| handleStringParseFailure(&char_src, failure),
    };
}

inline fn parseComptimeIntDigits(
    comptime base: u8,
    comptime digits: anytype,
) error{InvalidCharacter}!comptime_int {
    assert(@TypeOf(digits) == [digits.len]u8);
    var x = 0;
    for (digits) |c| {
        if (c == '_') continue;
        const digit_val = try std.fmt.charToDigit(c, base);
        x *= base;
        x += digit_val;
    }
    return x;
}

const containsComptime = struct {
    inline fn containsComptime(
        comptime T: type,
        comptime haystack: []const T,
        comptime needle: T,
    ) bool {
        comptime return containsComptimeImpl(
            T,
            dedupeSlice(T, haystack),
            needle,
        );
    }
    fn containsComptimeImpl(
        comptime T: type,
        comptime haystack: []const T,
        comptime needle: T,
    ) bool {
        comptime {
            const needle_vec: @Vector(haystack.len, T) = @splat(needle);
            return @reduce(.Or, haystack[0..].* == needle_vec);
        }
    }
}.containsComptime;

const enumTagNameCharSet = struct {
    inline fn enumTagNameCharSet(comptime E: type) *const [enumTagNameCharSetImpl(E).len]u8 {
        comptime return enumTagNameCharSetImpl(E)[0..];
    }
    fn enumTagNameCharSetImpl(comptime E: type) []const u8 {
        comptime {
            var chars: []const u8 = "";
            var set = std.bit_set.IntegerBitSet(std.math.maxInt(u8)).initEmpty();

            for (@typeInfo(E).Enum.fields) |field| {
                for (field.name) |c| {
                    if (set.isSet(c)) continue;
                    set.set(c);
                    chars = chars ++ &[_]u8{c};
                }
            }

            return chars;
        }
    }
}.enumTagNameCharSet;

inline fn indexOfNonePosComptime(
    comptime T: type,
    comptime haystack: []const T,
    comptime start: comptime_int,
    comptime excluded: []const T,
) ?comptime_int {
    const offs = indexOfNoneComptime(T, haystack[start..], excluded) orelse
        return null;
    return start + offs;
}
const indexOfNoneComptime = struct {
    fn indexOfNoneComptime(
        comptime T: type,
        comptime haystack: []const T,
        comptime excluded: []const T,
    ) ?comptime_int {
        const dd_hs = dedupeSlice(T, haystack);
        const dd_ex = dedupeSlice(T, excluded);
        return indexOfNoneComptimeImpl(T, dd_hs, dd_ex);
    }
    fn indexOfNoneComptimeImpl(
        comptime T: type,
        comptime haystack: []const T,
        comptime excluded: []const T,
    ) ?comptime_int {
        assert(excluded.len != 0);
        if (haystack.len == 0) return null;

        const arr = haystack[0..].*;
        const vec: @Vector(arr.len, T) = arr;

        var trues: @Vector(arr.len, bool) = .{true} ** arr.len;
        @setEvalBranchQuota(@min(std.math.maxInt(u32), excluded.len + 1));
        for (excluded) |ex| {
            const ex_vec: @Vector(arr.len, T) = @splat(ex);
            const prev: @Vector(arr.len, u1) = @bitCast(trues);
            const current: @Vector(arr.len, u1) = @bitCast(ex_vec != vec);
            trues = @bitCast(prev & current);
        }

        return std.simd.firstTrue(trues) orelse return null;
    }
}.indexOfNoneComptime;

const dedupeSlice = struct {
    inline fn dedupeSlice(comptime T: type, comptime slice: []const T) *const [slice.len]T {
        comptime return dedupeSliceImpl(slice[0..].*);
    }
    fn dedupeSliceImpl(comptime array: anytype) *const [array.len]@typeInfo(@TypeOf(array)).Array.child {
        comptime {
            const Array = @TypeOf(array);
            assert(Array == [array.len]@typeInfo(Array).Array.child);
            return &array;
        }
    }
}.dedupeSlice;

fn BoundedPackedStack(
    comptime max_len: comptime_int,
    comptime T: type,
) type {
    return struct {
        array: PackedArray = PackedArray.initAllTo(std.math.maxInt(Int)),
        len: std.math.IntFittingRange(0, max_len) = 0,
        const Self = @This();
        const Int = std.meta.Int(.unsigned, @bitSizeOf(T));
        const PackedArray = std.PackedIntArray(Int, max_len);

        pub fn push(stack: *Self, val: T) error{Overflow}!void {
            if (stack.len == max_len) return error.Overflow;
            const int: Int = switch (@typeInfo(T)) {
                .Enum => @bitCast(@intFromEnum(val)),
                else => @bitCast(val),
            };
            stack.array.set(stack.len, int);
            stack.len += 1;
        }

        pub fn pop(stack: *Self) ?T {
            if (stack.len == 0) return null;
            stack.len -= 1;
            const int = stack.array.get(stack.len);
            return switch (@typeInfo(T)) {
                .Enum => |info| @enumFromInt(@as(info.tag_type, @bitCast(int))),
                else => @as(T, @bitCast(int)),
            };
        }
    };
}
