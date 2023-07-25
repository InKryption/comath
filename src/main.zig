const std = @import("std");
const assert = std.debug.assert;

pub fn eval(
    comptime expr: []const u8,
    ctx: anytype,
    inputs: anytype,
) !EvalImpl(parseExpr(expr), @TypeOf(ctx), @TypeOf(inputs)) {
    const root = parseExpr(expr);
    return evalImpl(root, ctx, inputs);
}

const testing = struct {
    fn expectEqual(expected: anytype, actual: anytype) !void {
        const T = @TypeOf(expected, actual);
        return std.testing.expectEqual(@as(T, expected), @as(T, actual));
    }
};

test eval {
    try testing.expectEqual(7, eval("a + 3", defaultCtx(void{}), .{ .a = 4 }));
    try testing.expectEqual(0, eval("a % 2", defaultCtx(void{}), .{ .a = 4 }));
    try testing.expectEqual(12, eval("(y + 2) * x", defaultCtx(void{}), .{ .y = 2, .x = 3 }));
    try testing.expectEqual(8, eval("y + 2 * x", defaultCtx(void{}), .{ .y = 2, .x = 3 }));
    // // TODO: figure out why this crashes the compiler
    // try testing.expectEqual(3, eval("x[y]", defaultCtx(void{}), .{
    //     .x = [3]u16{ 0, 3, 7 },
    //     .y = 1,
    // }));
    try testing.expectEqual(3, eval("a.b", defaultCtx(void{}), .{ .a = .{ .b = 3 } }));

    const PowCtx = struct {
        pub fn EvalBinOp(comptime Lhs: type, comptime op: BinaryOp, comptime Rhs: type) type {
            _ = op;
            return @TypeOf(@as(Lhs, 0), @as(Rhs, 0));
        }
        pub inline fn evalBinOp(_: @This(), lhs: anytype, comptime op: BinaryOp, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            return std.math.pow(@TypeOf(lhs, rhs), lhs, rhs);
        }
    };
    try testing.expectEqual(64, eval("a ^ 3", defaultCtx(PowCtx{}), .{ .a = @as(u64, 4) }));
}

pub inline fn defaultCtx(sub_ctx: anytype) DefaultCtx(@TypeOf(sub_ctx)) {
    return .{ .sub_ctx = sub_ctx };
}
pub fn DefaultCtx(
    comptime SubCtx: type,
) type {
    return struct {
        sub_ctx: SubCtx,
        const Self = @This();

        const meaningful_subctx = switch (@typeInfo(ImplicitDeref(SubCtx))) {
            .Struct, .Union, .Enum, .Opaque => true,
            else => false,
        };

        pub fn EvalBinOp(comptime Lhs: type, comptime op: BinaryOp, comptime Rhs: type) type {
            if (meaningful_subctx and @hasDecl(SubCtx, "EvalBinOp")) {
                const Res = SubCtx.EvalBinOp(Lhs, op, Rhs);
                if (Res != noreturn) return Res;
            }
            return @TypeOf(
                @as(Lhs, undefined),
                @as(Rhs, undefined),
            );
        }
        pub inline fn evalBinOp(ctx: Self, lhs: anytype, comptime op: BinaryOp, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);
            if (meaningful_subctx and @hasDecl(SubCtx, "EvalBinOp")) {
                const Res = SubCtx.EvalBinOp(Lhs, op, Rhs);
                if (Res != noreturn) return ctx.sub_ctx.evalBinOp(lhs, op, rhs);
            }
            return switch (op) {
                .@"^" => lhs ^ rhs,
                .@"|" => lhs | rhs,
                .@"&" => lhs & rhs,

                .@"+" => lhs + rhs,
                .@"-" => lhs - rhs,
                .@"*" => lhs * rhs,
                .@"/" => lhs / rhs,
                .@"%" => lhs % rhs,

                .@"==" => lhs == rhs,
                .@"!=" => lhs != rhs,
                .@"<" => lhs < rhs,
                .@">" => lhs > rhs,
                .@"<=" => lhs <= rhs,
                .@">=" => lhs >= rhs,
            };
        }

        pub fn EvalIndexAccess(
            comptime Lhs: type,
            comptime Rhs: type,
        ) type {
            if (meaningful_subctx and @hasDecl(SubCtx, "EvalIndexAccess")) {
                const Res = SubCtx.EvalIndexAccess(Lhs, Rhs);
                if (Res != noreturn) return Res;
            }
            return std.meta.Elem(Lhs);
        }
        pub inline fn evalIndexAccess(ctx: Self, lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);

            if (meaningful_subctx and @hasDecl(SubCtx, "EvalIndexAccess")) {
                const Res = SubCtx.EvalIndexAccess(Lhs, Rhs);
                if (Res != noreturn) return ctx.sub_ctx.evalIndexAccess(lhs, rhs);
            }
            return lhs[rhs];
        }

        pub fn EvalProperty(
            comptime Lhs: type,
            comptime field: []const u8,
        ) type {
            if (meaningful_subctx and @hasDecl(SubCtx, "EvalProperty")) {
                const Res = SubCtx.EvalProperty(Lhs, field);
                if (Res != noreturn) return Res;
            }
            return std.meta.FieldType(Lhs, @field(std.meta.FieldEnum(Lhs), field));
        }
        pub inline fn evalProperty(ctx: Self, lhs: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(lhs), field) {
            const Lhs = @TypeOf(lhs);
            if (meaningful_subctx and @hasDecl(SubCtx, "EvalProperty")) {
                const Res = SubCtx.EvalProperty(Lhs, field);
                if (Res != noreturn) return ctx.sub_ctx.evalProperty(lhs, field);
            }
            return @field(lhs, field);
        }
    };
}

/// Represents a non-integer value, represented by the source code
/// Can be interpreted as needed, by default it is interpreted as a
/// floating point value
pub const Number = struct {
    src: []const u8,

    pub fn asFloat(comptime number: Number) comptime_float {
        comptime return std.fmt.parseFloat(f128, number.src) catch |err| @compileError(@errorName(err));
    }
};

/// Represents a character literal value, distinguished from `comptime_int`.
pub const Char = enum(comptime_int) {
    _,
};

pub const UnaryOp = enum {
    @"~",
    @"-",
    @"!",
};
pub const BinaryOp = enum {
    @"^",
    @"|",
    @"&",

    @"+",
    @"-",
    @"*",
    @"/",
    @"%",

    @"==",
    @"!=",
    @"<",
    @">",
    @"<=",
    @">=",

    const precedence = std.enums.directEnumArray(BinaryOp, comptime_int, 0, .{
        .@"^" = 5,
        .@"|" = 4,
        .@"&" = 4,

        .@"+" = 2,
        .@"-" = 2,
        .@"*" = 3,
        .@"/" = 3,
        .@"%" = 3,

        .@"==" = -1,
        .@"!=" = -1,
        .@"<" = -1,
        .@">" = -1,
        .@"<=" = -1,
        .@">=" = -1,
    });
};

fn EvalImpl(
    comptime expr: ExprNode,
    comptime Ctx: type,
    comptime Inputs: type,
) type {
    const InputTag = std.meta.FieldEnum(Inputs);
    return switch (expr) {
        .null => @compileError("Incomplete AST (encountered null expression)"),
        .ident => |ident| std.meta.FieldType(Inputs, @field(InputTag, ident)),
        .integer => comptime_int,
        .char => Char,
        .float => Number,
        .group => |group| EvalImpl(group.*, Ctx, Inputs),
        .field_access => |fa| blk: {
            const Lhs = EvalImpl(fa.accessed.*, Ctx, Inputs);
            break :blk Ctx.EvalProperty(Lhs, dedupeSlice(u8, fa.accessor));
        },
        .index_access => |ia| blk: {
            const Lhs = EvalImpl(ia.accessed.*, Ctx, Inputs);
            const Rhs = EvalImpl(ia.accessor.*, Ctx, Inputs);
            break :blk Ctx.EvalIndexAccess(Lhs, Rhs);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs.*, Ctx, Inputs);
            const Rhs = EvalImpl(bin.rhs.*, Ctx, Inputs);
            break :blk Ctx.EvalBinOp(Lhs, bin.op, Rhs);
        },
        .un_op => |un| {
            _ = un;
            if (true) @compileError("TODO");
        },
    };
}
inline fn evalImpl(
    comptime expr: ExprNode,
    ctx: anytype,
    inputs: anytype,
) !EvalImpl(expr, @TypeOf(ctx), @TypeOf(inputs)) {
    const Ctx = @TypeOf(ctx);
    const Inputs = @TypeOf(inputs);
    return switch (expr) {
        .null => @compileError("Incomplete AST (encountered null expression)"),
        .ident => |ident| @field(inputs, ident),
        .integer => |int| int,
        .char => |char| char,
        .float => |num| num,
        .group => |group| evalImpl(group.*, ctx, inputs),
        .field_access => |fa| blk: {
            const Lhs = EvalImpl(fa.accessed.*, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(fa.accessed.*, ctx, inputs);
            break :blk ctx.evalProperty(lhs, dedupeSlice(u8, fa.accessor));
        },
        .index_access => |ia| blk: {
            const Lhs = EvalImpl(ia.accessed.*, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(ia.accessed.*, ctx, inputs);

            const Rhs = EvalImpl(ia.accessor.*, Ctx, Inputs);
            const rhs: Rhs = try evalImpl(ia.accessor.*, ctx, inputs);

            break :blk ctx.evalIndexAccess(lhs, rhs);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs.*, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(bin.lhs.*, ctx, inputs);

            const Rhs = EvalImpl(bin.rhs.*, Ctx, Inputs);
            const rhs: Rhs = try evalImpl(bin.rhs.*, ctx, inputs);

            break :blk ctx.evalBinOp(lhs, bin.op, rhs);
        },
        .un_op => |un| {
            _ = un;
            if (true) @compileError("TODO");
        },
    };
}

inline fn parseExpr(comptime expr: []const u8) ExprNode {
    return parseExprImpl(
        dedupeSlice(u8, expr),
        .none,
        .{},
    ).result.dedupe().*;
}

test parseExpr {
    const group = ExprNode.makeGroup;
    const binOp = ExprNode.makeBinOp;
    const unOp = ExprNode.makeUnOp;
    const fieldAccess = ExprNode.makeFieldAccess;
    const indexAccess = ExprNode.makeIndexAccess;
    const int = ExprNode.makeInt;
    const float = ExprNode.makeFloat;
    const char = ExprNode.makeChar;
    const ident = ExprNode.makeIdent;

    try std.testing.expectEqualDeep(parseExpr("423_324"), int(423_324));
    try std.testing.expectEqualDeep(parseExpr("-423_324"), unOp(&.{.@"-"}, int(423_324)));
    try std.testing.expectEqualDeep(parseExpr("~-423_324"), unOp(&.{ .@"~", .@"-" }, int(423_324)));
    try std.testing.expectEqualDeep(parseExpr("~(-423_324)"), unOp(&.{.@"~"}, group(unOp(&.{.@"-"}, int(423_324)))));
    try std.testing.expectEqualDeep(parseExpr("a.b"), fieldAccess(ident("a"), "b"));
    try std.testing.expectEqualDeep(parseExpr("a[b]"), indexAccess(ident("a"), ident("b")));
    try std.testing.expectEqualDeep(parseExpr("!('\u{A0}' + a ^ (3 / y.z))"), unOp(&.{.@"!"}, group(binOp(
        char('\u{A0}'),
        .@"+",
        binOp(
            ident("a"),
            .@"^",
            group(binOp(int(3), .@"/", fieldAccess(ident("y"), "z"))),
        ),
    ))));
    try std.testing.expectEqualDeep(parseExpr("3 + -2"), binOp(
        int(3),
        .@"+",
        unOp(&.{.@"-"}, int(2)),
    ));
    try std.testing.expectEqualDeep(parseExpr("(y + 2) * x"), binOp(
        group(binOp(ident("y"), .@"+", int(2))),
        .@"*",
        ident("x"),
    ));
    try std.testing.expectEqualDeep(parseExpr("y + 2 * x"), binOp(
        ident("y"),
        .@"+",
        binOp(int(2), .@"*", ident("x")),
    ));
    try std.testing.expectEqualDeep(parseExpr("2.0 * y ^ 3"), binOp(
        float("2.0"),
        .@"*",
        binOp(ident("y"), .@"^", int(3)),
    ));
}

const NestType = enum(comptime_int) { none, paren, bracket };
const ParseExprImplInnerUpdate = struct {
    tokenizer: Tokenizer,
    result: ExprNode,
};
fn parseExprImpl(
    comptime expr: []const u8,
    comptime nest_type: NestType,
    comptime tokenizer_init: Tokenizer,
) ParseExprImplInnerUpdate {
    comptime {
        var result: ExprNode = .null;
        var tokenizer = tokenizer_init;
        while (tokenizer.next(expr)) |tok| {
            const res_copy = result;
            switch (tok) {
                inline //
                .ident,
                .integer,
                => |val, tag| result = res_copy.concatExpr(@unionInit(ExprNode, @tagName(tag), val)),
                .char => |val| result = res_copy.concatExpr(ExprNode.makeChar(val)),
                .float => |val| result = res_copy.concatExpr(ExprNode.makeFloat(val)),
                .field => |field| result = res_copy.concatFieldAccess(field),
                .op => |op| result = res_copy.concatOp(op),

                .paren_open => {
                    const update = parseExprImpl(expr, .paren, tokenizer);
                    tokenizer = update.tokenizer;
                    result = res_copy.concatExpr(ExprNode.makeGroup(update.result));
                },
                .paren_close => {
                    if (nest_type != .paren) @compileError("Unexpected closing parentheses");
                    break;
                },

                .bracket_open => {
                    const update = parseExprImpl(expr, .bracket, tokenizer);
                    tokenizer = update.tokenizer;
                    result = ExprNode.makeIndexAccess(res_copy, update.result);
                },
                .bracket_close => {
                    if (nest_type != .bracket) @compileError("Unexpected closing bracket");
                    break;
                },
            }
        }
        return .{
            .tokenizer = tokenizer,
            .result = result,
        };
    }
}

const ExprNode = union(enum) {
    null,
    ident: []const u8,
    integer: comptime_int,
    char: Char,
    float: Number,
    group: *const ExprNode,
    field_access: FieldAccess,
    index_access: IndexAccess,
    bin_op: BinOp,
    un_op: UnOp,

    fn makeIdent(comptime ident: []const u8) ExprNode {
        return .{ .ident = dedupeSlice(u8, ident) };
    }
    fn makeFieldAccess(comptime accessed: ExprNode, comptime accessor: []const u8) ExprNode {
        return .{ .field_access = .{
            .accessed = &accessed,
            .accessor = accessor,
        } };
    }
    fn makeIndexAccess(comptime accessed: ExprNode, comptime accessor: ExprNode) ExprNode {
        return .{ .index_access = .{
            .accessed = &accessed,
            .accessor = &accessor,
        } };
    }
    fn makeInt(comptime val: comptime_int) ExprNode {
        return .{ .integer = val };
    }
    fn makeChar(comptime val: comptime_int) ExprNode {
        return .{ .char = @enumFromInt(val) };
    }
    fn makeFloat(comptime val: anytype) ExprNode {
        comptime {
            if (std.meta.trait.isZigString(@TypeOf(val))) {
                return .{ .float = .{ .src = dedupeSlice(u8, val) } };
            }
            switch (@typeInfo(@TypeOf(val))) {
                .ComptimeFloat, .Float => {},
                else => unreachable,
            }
            return .{
                .float = .{ .src = dedupeSlice(u8, std.fmt.comptimePrint("{}", .{val})) },
            };
        }
    }
    fn makeGroup(comptime expr: ExprNode) ExprNode {
        return .{ .group = &expr };
    }
    fn makeBinOp(
        comptime lhs: ExprNode,
        comptime op: BinaryOp,
        comptime rhs: ExprNode,
    ) ExprNode {
        return .{ .bin_op = .{
            .lhs = &lhs,
            .op = op,
            .rhs = &rhs,
        } };
    }
    fn makeUnOp(comptime op: []const UnaryOp, comptime val: ExprNode) ExprNode {
        return .{ .un_op = .{
            .op = dedupeSlice(UnaryOp, op),
            .val = &val,
        } };
    }

    inline fn concatOp(comptime base: ExprNode, comptime op: Operator) ExprNode {
        return switch (base) {
            .null => makeUnOp(&.{std.enums.nameCast(UnaryOp, op)}, .null),

            .ident,
            .field_access,
            .index_access,
            .integer,
            .char,
            .float,
            .group,
            => makeBinOp(base, std.enums.nameCast(BinaryOp, op), .null),

            .bin_op => |bin| switch (bin.rhs.*) {
                .null => makeBinOp(bin.lhs.*, bin.op, makeUnOp(&.{std.enums.nameCast(UnaryOp, op)}, .null)),

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => blk: {
                    const old_prec = BinaryOp.precedence[@intFromEnum(bin.op)];
                    const new_prec = BinaryOp.precedence[@intFromEnum(op)];
                    if (old_prec < 0 and new_prec < 0) {
                        @compileError(@tagName(bin.op) ++ " cannot be chained with " ++ @tagName(op));
                    }
                    if (old_prec < new_prec) {
                        break :blk makeBinOp(bin.lhs.*, bin.op, makeBinOp(bin.rhs.*, std.enums.nameCast(BinaryOp, op), .null));
                    }
                    break :blk makeBinOp(base, std.enums.nameCast(BinaryOp, op), .null);
                },
                .bin_op, .un_op => makeBinOp(bin.lhs.*, bin.op, bin.rhs.concatOp(op)),
            },
            .un_op => |un| switch (un.val.*) {
                .null => makeUnOp(un.op ++ &[_]UnaryOp{std.enums.nameCast(UnaryOp, op)}, .null),

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => makeBinOp(base, op, .null),

                .bin_op => unreachable,
                .un_op => unreachable,
            },
        };
    }
    inline fn concatExpr(comptime base: ExprNode, comptime new: ExprNode) ExprNode {
        return switch (base) {
            .null => new,

            .ident,
            .field_access,
            .index_access,
            .integer,
            .char,
            .float,
            .group,
            => @compileError(std.fmt.comptimePrint("Unexpected token '{}'", .{new.fmt()})),

            .bin_op => |bin| switch (bin.rhs.*) {
                .null => makeBinOp(bin.lhs.*, bin.op, new),

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => @compileError(std.fmt.comptimePrint("Unexpected token '{}'", .{new.fmt()})),
                .bin_op,
                .un_op,
                => makeBinOp(bin.lhs.*, bin.op, bin.rhs.concatExpr(new)),
            },
            .un_op => |un| switch (un.val.*) {
                .null => makeUnOp(un.op, new),

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => @compileError(std.fmt.comptimePrint("Unexpected token '{}'", .{new.fmt()})),

                .bin_op => unreachable,
                .un_op => unreachable,
            },
        };
    }
    inline fn concatFieldAccess(comptime base: ExprNode, comptime field: []const u8) ExprNode {
        return switch (base) {
            .null => @compileError("Unexpected token '." ++ field ++ "'"),
            .integer,
            .char,
            .float,
            .ident,
            .field_access,
            .index_access,
            .group,
            => makeFieldAccess(base, field),
            .bin_op => |bin_op| makeBinOp(bin_op.lhs.*, bin_op.op, bin_op.rhs.concatFieldAccess(field)),
            .un_op => |un_op| makeUnOp(un_op.op, un_op.val.concatFieldAccess(field)),
        };
    }

    const dedupe = struct {
        fn dedupe(comptime expr: ExprNode) *const ExprNode {
            // comptime return dedupeImpl(expr, switch (expr) {
            //     inline else => |val| val,
            // });
            return &expr;
        }
        fn dedupeImpl(
            comptime tag: std.meta.FieldEnum(ExprNode),
            comptime value: std.meta.FieldType(ExprNode, tag),
        ) *const ExprNode {
            const res = switch (comptime tag) {
                .null,
                .char,
                => value,
                .ident => makeIdent(value),
                .float => makeFloat(value),
                .integer => makeInt(value),
                inline //
                .field_access,
                .index_access,
                .group,
                .bin_op,
                .un_op,
                => @unionInit(ExprNode, @tagName(tag), value.dedupe()),
            };
            return dedupeImpl2(res);
        }
        fn dedupeImpl2(
            comptime expr: ExprNode,
        ) *const ExprNode {
            return &expr;
        }
    }.dedupe;

    const FieldAccess = struct {
        accessed: *const ExprNode,
        accessor: []const u8,

        inline fn dedupe(comptime fa: FieldAccess) FieldAccess {
            return .{
                .accessed = fa.accessed.dedupe(),
                .accessor = dedupeSlice(u8, fa.accessor),
            };
        }
    };
    const IndexAccess = struct {
        accessed: *const ExprNode,
        accessor: *const ExprNode,

        inline fn dedupe(comptime ia: IndexAccess) IndexAccess {
            return .{
                .accessed = ia.accessed.dedupe(),
                .accessor = ia.accessor.dedupe(),
            };
        }
    };
    const BinOp = struct {
        lhs: *const ExprNode,
        op: BinaryOp,
        rhs: *const ExprNode,

        inline fn dedupe(comptime bin: BinOp) BinOp {
            return .{
                .lhs = bin.lhs.dedupe(),
                .op = bin.op,
                .rhs = bin.rhs.dedupe(),
            };
        }
    };
    const UnOp = struct {
        op: []const UnaryOp,
        val: *const ExprNode,

        inline fn dedupe(comptime un: UnOp) UnOp {
            return .{
                .op = dedupeSlice(UnaryOp, un.op),
                .val = un.val.dedupe(),
            };
        }
    };

    inline fn fmt(comptime expr: ExprNode) Fmt {
        return .{ .expr = expr };
    }
    const Fmt = struct {
        expr: ExprNode,

        pub fn format(
            comptime formatter: Fmt,
            comptime fmt_str: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            _ = options;
            if (fmt_str.len != 0) std.fmt.invalidFmtError(fmt_str, formatter);
            switch (formatter.expr) {
                .null => try writer.writeAll("null"),
                .ident => |ident| try writer.writeAll(ident),
                .field_access => |field| try writer.print("{}.{s}", .{ field.accessed.fmt(), field.accessor }),
                .index_access => |index| try writer.print("{}[{}]", .{ index.accessed.fmt(), index.accessor.fmt() }),
                .integer => |int| try writer.print("{d}", .{int}),
                .char => |char| try writer.print("'{u}'", .{char}),
                .float => |str| try writer.writeAll(str),
                .group => |group| try writer.print("({})", .{group.fmt()}),
                .bin_op => |bin_op| try writer.print("{} {s} {}", .{ bin_op.lhs.fmt(), @tagName(bin_op.op), bin_op.rhs.fmt() }),
                .un_op => |un_op| {
                    comptime var prefix: []const u8 = "";
                    comptime for (un_op.op) |op| {
                        prefix = prefix ++ @tagName(op);
                    };
                    try writer.print("{s}{}", .{ prefix, un_op.val.fmt() });
                },
            }
        }
    };
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

    const precedence = std.enums.directEnumArray(Operator, ?comptime_int, 0, .{
        .@"~" = null,
        .@"^" = 5,
        .@"|" = 4,
        .@"&" = 4,

        .@"+" = 2,
        .@"-" = 2,
        .@"*" = 3,
        .@"/" = 3,
        .@"%" = 3,

        .@"!" = null,
        .@"==" = -1,
        .@"!=" = -1,
        .@"<" = -1,
        .@">" = -1,
        .@"<=" = -1,
        .@">=" = -1,
    });
};

const Token = union(enum) {
    ident: []const u8,
    /// '.' field
    field: []const u8,
    integer: comptime_int,
    char: comptime_int,
    float: []const u8,
    op: Operator,

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
            '(' => return .{ .state = .{ .index = idx + 1 }, .token = .paren_open },
            ')' => return .{ .state = .{ .index = idx + 1 }, .token = .paren_close },
            '[' => return .{ .state = .{ .index = idx + 1 }, .token = .bracket_open },
            ']' => return .{ .state = .{ .index = idx + 1 }, .token = .bracket_close },
            'a'...'z',
            'A'...'Z',
            '_',
            => {
                const start = idx;
                const end = endOfIdent(buffer, start);
                const ident = buffer[start..end];
                return .{
                    .state = .{ .index = idx + ident.len },
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
                    .state = .{ .index = idx + ident.len },
                    .token = .{ .field = ident },
                };
            },
            '0'...'9', '\'' => {
                @setEvalBranchQuota(@min(std.math.maxInt(u32), (buffer.len - idx) * 100));
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
                    .state = .{ .index = idx },
                    .token = literal_tok,
                };
            },

            '~', '^', '|', '&', '+', '-', '*', '/', '%' => {
                const start = idx;
                idx += 1;
                const end = idx;
                const op = @field(Operator, buffer[start..end]);
                return .{
                    .state = .{ .index = idx },
                    .token = .{ .op = op },
                };
            },

            // "!", "!="
            '!' => {
                const start = idx;
                idx += 1;
                const end = if (idx != buffer.len and buffer[idx] == '=') blk: {
                    idx += 1;
                    break :blk idx;
                } else idx;
                const op = @field(Operator, buffer[start..end]);
                return .{
                    .state = .{ .index = idx },
                    .token = .{ .op = op },
                };
            },

            // "==", "<", "<=", ">=", ">"
            '=', '<', '>' => {
                const start = idx;
                idx += 1;
                const end = if (idx != buffer.len and buffer[idx] == '=') blk: {
                    idx += 1;
                    break :blk idx;
                } else idx;
                const str = buffer[start..end];
                if (!@hasField(Operator, str)) @compileError(
                    "Unexpected token '" ++ str ++ "'",
                );
                const op = @field(Operator, str);
                return .{
                    .state = .{ .index = idx },
                    .token = .{ .op = op },
                };
            },
            else => |c| @compileError("Unexpected character '" ++ &.{c} ++ "'"),
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
        .{ .op = .@"~" },        .{ .float = "3.0" },   .{ .op = .@"+" },         .{ .integer = 3 },
        .{ .op = .@"-" },        .{ .op = .@"!" },      .{ .paren_open = {} },    .{ .char = 'a' },
        .{ .paren_close = {} },  .{ .op = .@"*" },      .{ .bracket_close = {} }, .{ .op = .@"/" },
        .{ .bracket_open = {} }, .{ .op = .@"==" },     .{ .op = .@"!=" },        .{ .op = .@"<" },
        .{ .op = .@">" },        .{ .op = .@"<=" },     .{ .op = .@">=" },        .{ .op = .@"^" },
        .{ .op = .@"|" },        .{ .ident = "a_b_C" }, .{ .op = .@"&" },         null,
    });

    try testTokenizer("(x - 2) * (3 + ~y)", &.{
        .paren_open,
        .{ .ident = "x" },
        .{ .op = .@"-" },
        .{ .integer = 2 },
        .paren_close,
        .{ .op = .@"*" },
        .paren_open,
        .{ .integer = 3 },
        .{ .op = .@"+" },
        .{ .op = .@"~" },
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

fn ImplicitDeref(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Pointer => |info| switch (info.size) {
            .One => info.child,
            else => T,
        },
        else => T,
    };
}
