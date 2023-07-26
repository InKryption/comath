const std = @import("std");
const assert = std.debug.assert;

pub inline fn eval(
    comptime expr: []const u8,
    ctx: anytype,
    inputs: anytype,
) !EvalImpl(parseExpr(expr, @TypeOf(ctx).precedence), @TypeOf(ctx), @TypeOf(inputs)) {
    const root = comptime parseExpr(expr, @TypeOf(ctx).precedence);
    return evalImpl(root, ctx, inputs);
}

const testing = struct {
    fn expectEqual(expected: anytype, actual: anytype) !void {
        const T = @TypeOf(expected, actual);
        return std.testing.expectEqual(@as(T, expected), @as(T, actual));
    }
};

test eval {
    try testing.expectEqual(3, eval("x[y]", defaultCtx(void{}), .{
        .x = [3]u16{ 0, 3, 7 },
        .y = 1,
    }));
    try testing.expectEqual(.{ 1, 2, 3 }, eval("x + y", defaultCtx(void{}), .{
        .x = std.simd.iota(u8, 3),
        .y = @as(@Vector(3, u8), @splat(1)),
    }));
    try testing.expectEqual(-4, eval("-4", defaultCtx(void{}), .{}));
    try testing.expectEqual(7, eval("a + 3", defaultCtx(void{}), .{ .a = 4 }));
    try testing.expectEqual(0, eval("a % 2", defaultCtx(void{}), .{ .a = 4 }));
    try testing.expectEqual(12, eval("(y + 2) * x", defaultCtx(void{}), .{ .y = 2, .x = 3 }));
    try testing.expectEqual(8, eval("y + 2 * x", defaultCtx(void{}), .{ .y = 2, .x = 3 }));
    try testing.expectEqual(3, eval("a.b", defaultCtx(void{}), .{ .a = .{ .b = 3 } }));

    const PowCtx = struct {
        pub fn EvalBinOp(comptime Lhs: type, comptime op: Operator, comptime Rhs: type) type {
            _ = op;
            return @TypeOf(@as(Lhs, 0), @as(Rhs, 0));
        }
        pub inline fn evalBinOp(_: @This(), lhs: anytype, comptime op: Operator, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
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

        pub const precedence = Operator.PrecedenceValues{
            // non-binary operators
            .@"~" = null,
            .@"!" = null,

            // bit operators have higher precedence than the arithmetic operators
            .@"^" = 4,
            .@"|" = 4,
            .@"&" = 4,

            // typical arithmetic precedence
            .@"*" = 3,
            .@"/" = 3,
            .@"%" = 3,
            .@"+" = 2,
            .@"-" = 2,

            // make comparison operators non-chainable
            .@"==" = -1,
            .@"!=" = -1,
            .@"<" = -1,
            .@">" = -1,
            .@"<=" = -1,
            .@">=" = -1,
        };

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

        pub fn EvalUnOp(comptime op: Operator, comptime Val: type) type {
            if (meaningful_subctx and @hasDecl(SubCtx, "EvalUnOp")) {
                const Res = SubCtx.EvalUnOp(op, Val);
                if (Res != noreturn) return Res;
            }
            return Val;
        }
        pub fn evalUnOp(ctx: Self, comptime op: Operator, val: anytype) !EvalUnOp(op, @TypeOf(val)) {
            const Val = @TypeOf(val);
            if (meaningful_subctx and @hasDecl(SubCtx, "EvalUnOp")) {
                const Res = SubCtx.EvalUnOp(op, Val);
                if (Res != noreturn) return ctx.sub_ctx.evalUnOp(op, val);
            }
            return switch (op) {
                .@"~" => ~val,
                .@"-" => -val,
                .@"!" => !val,
                else => comptime unreachable,
            };
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: Operator, comptime Rhs: type) type {
            if (meaningful_subctx and @hasDecl(SubCtx, "EvalBinOp")) {
                const Res = SubCtx.EvalBinOp(Lhs, op, Rhs);
                if (Res != noreturn) return Res;
            }
            return @TypeOf(
                @as(Lhs, undefined),
                @as(Rhs, undefined),
            );
        }
        pub inline fn evalBinOp(ctx: Self, lhs: anytype, comptime op: Operator, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);
            if (meaningful_subctx and @hasDecl(SubCtx, "EvalBinOp")) {
                const Res = SubCtx.EvalBinOp(Lhs, op, Rhs);
                if (Res != noreturn) return ctx.sub_ctx.evalBinOp(lhs, op, rhs);
            }
            return switch (op) {
                .@"~" => comptime unreachable,
                .@"!" => comptime unreachable,

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

pub const Operator = enum {
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

    pub const PrecedenceValues = std.enums.EnumFieldStruct(Operator, ?comptime_int, null);
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
            const Lhs = EvalImpl(ia.accessed, Ctx, Inputs);
            const Rhs = EvalImpl(ia.accessor, Ctx, Inputs);
            break :blk Ctx.EvalIndexAccess(Lhs, Rhs);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs.*, Ctx, Inputs);
            const Rhs = EvalImpl(bin.rhs.*, Ctx, Inputs);
            break :blk Ctx.EvalBinOp(Lhs, bin.op, Rhs);
        },
        .un_op => |un| blk: {
            const Val = EvalImpl(un.val.*, Ctx, Inputs);
            break :blk Ctx.EvalUnOp(un.op, Val);
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
    return switch (comptime expr) {
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
            const Lhs = EvalImpl(ia.accessed, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(ia.accessed, ctx, inputs);

            const Rhs = EvalImpl(ia.accessor, Ctx, Inputs);
            const rhs: Rhs = try evalImpl(ia.accessor, ctx, inputs);

            break :blk ctx.evalIndexAccess(lhs, rhs);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs.*, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(bin.lhs.*, ctx, inputs);

            const Rhs = EvalImpl(bin.rhs.*, Ctx, Inputs);
            const rhs: Rhs = try evalImpl(bin.rhs.*, ctx, inputs);

            break :blk ctx.evalBinOp(lhs, bin.op, rhs);
        },
        .un_op => |un| blk: {
            const Val = EvalImpl(un.val.*, Ctx, Inputs);
            const val: Val = try evalImpl(un.val.*, ctx, inputs);
            break :blk ctx.evalUnOp(un.op, val);
        },
    };
}

fn parseExpr(
    comptime expr: []const u8,
    comptime precedence: Operator.PrecedenceValues,
) ExprNode {
    comptime {
        const deduped_expr = dedupeSlice(u8, expr);
        const res = parseExprImpl(deduped_expr, precedence, .none, .{});
        return res.result;
    }
}

test parseExpr {
    const helper = struct {
        inline fn int(comptime val: comptime_int) ExprNode {
            return .{ .integer = val };
        }
        inline fn float(comptime src: []const u8) ExprNode {
            return .{ .float = Number{ .src = src } };
        }
        inline fn ident(comptime name: []const u8) ExprNode {
            return .{ .ident = name };
        }
        inline fn char(comptime val: comptime_int) ExprNode {
            return .{ .char = @enumFromInt(val) };
        }
        inline fn group(comptime expr: ExprNode) ExprNode {
            return .{ .group = &expr };
        }
        inline fn binOp(comptime lhs: ExprNode, comptime op: Operator, comptime rhs: ExprNode) ExprNode {
            return .{ .bin_op = .{
                .lhs = &lhs,
                .op = op,
                .rhs = &rhs,
            } };
        }
        inline fn unOp(comptime op: Operator, comptime expr: ExprNode) ExprNode {
            return .{ .un_op = .{
                .op = op,
                .val = &expr,
            } };
        }
        inline fn fieldAccess(comptime expr: ExprNode, comptime field: []const u8) ExprNode {
            return .{ .field_access = .{
                .accessed = &expr,
                .accessor = field,
            } };
        }
        inline fn indexAccess(comptime lhs: ExprNode, comptime idx: ExprNode) ExprNode {
            return .{ .index_access = &.{
                .accessed = lhs,
                .accessor = idx,
            } };
        }
    };

    try std.testing.expectEqualDeep(parseExpr("423_324", DefaultCtx(void).precedence), helper.int(423_324));
    try std.testing.expectEqualDeep(parseExpr("-423_324", DefaultCtx(void).precedence), helper.unOp(.@"-", helper.int(423_324)));
    try std.testing.expectEqualDeep(parseExpr("~-423_324", DefaultCtx(void).precedence), helper.unOp(.@"~", helper.unOp(.@"-", helper.int(423_324))));
    try std.testing.expectEqualDeep(parseExpr("~(-423_324)", DefaultCtx(void).precedence), helper.unOp(.@"~", helper.group(helper.unOp(.@"-", helper.int(423_324)))));
    try std.testing.expectEqualDeep(parseExpr("a.b", DefaultCtx(void).precedence), helper.fieldAccess(helper.ident("a"), "b"));
    try std.testing.expectEqualDeep(parseExpr("a[b]", DefaultCtx(void).precedence), helper.indexAccess(helper.ident("a"), helper.ident("b")));
    try std.testing.expectEqualDeep(parseExpr("!('\u{A0}' + a ^ (3 / y.z))", DefaultCtx(void).precedence), helper.unOp(.@"!", helper.group(helper.binOp(
        helper.char('\u{A0}'),
        .@"+",
        helper.binOp(
            helper.ident("a"),
            .@"^",
            helper.group(helper.binOp(helper.int(3), .@"/", helper.fieldAccess(helper.ident("y"), "z"))),
        ),
    ))));
    try std.testing.expectEqualDeep(parseExpr("3 + -2", DefaultCtx(void).precedence), helper.binOp(
        helper.int(3),
        .@"+",
        helper.unOp(.@"-", helper.int(2)),
    ));
    try std.testing.expectEqualDeep(parseExpr("(y + 2) * x", DefaultCtx(void).precedence), helper.binOp(
        helper.group(helper.binOp(helper.ident("y"), .@"+", helper.int(2))),
        .@"*",
        helper.ident("x"),
    ));
    try std.testing.expectEqualDeep(parseExpr("y + 2 * x", DefaultCtx(void).precedence), helper.binOp(
        helper.ident("y"),
        .@"+",
        helper.binOp(helper.int(2), .@"*", helper.ident("x")),
    ));
    try std.testing.expectEqualDeep(parseExpr("2.0 * y ^ 3", DefaultCtx(void).precedence), helper.binOp(
        helper.float("2.0"),
        .@"*",
        helper.binOp(helper.ident("y"), .@"^", helper.int(3)),
    ));
}

const NestType = enum(comptime_int) { none, paren, bracket };
const ParseExprImplInnerUpdate = struct {
    tokenizer: Tokenizer,
    result: ExprNode,
};
fn parseExprImpl(
    comptime expr: []const u8,
    comptime precedence: Operator.PrecedenceValues,
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
                .char => |val| result = res_copy.concatExpr(.{ .char = @enumFromInt(val) }),
                .float => |val| result = res_copy.concatExpr(.{ .float = Number{ .src = val } }),
                .field => |field| result = res_copy.concatFieldAccess(field),
                .op => |op| result = res_copy.concatOp(op, precedence),

                .paren_open => {
                    const update = parseExprImpl(expr, precedence, .paren, tokenizer);
                    tokenizer = update.tokenizer;
                    result = res_copy.concatExpr(.{ .group = update.result.dedupe() });
                },
                .paren_close => {
                    if (nest_type != .paren) @compileError("Unexpected closing parentheses");
                    break;
                },

                .bracket_open => {
                    const update = parseExprImpl(expr, precedence, .bracket, tokenizer);
                    tokenizer = update.tokenizer;
                    result = .{ .index_access = &.{
                        .accessed = res_copy,
                        .accessor = update.result,
                    } };
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
    index_access: *const IndexAccess,
    bin_op: BinOp,
    un_op: UnOp,

    inline fn concatOp(
        comptime base: ExprNode,
        comptime op: Operator,
        comptime precedence: Operator.PrecedenceValues,
    ) ExprNode {
        return switch (base) {
            .null => .{ .un_op = .{
                .op = std.enums.nameCast(Operator, op),
                .val = ExprNode.dedupe(.null),
            } },

            .ident,
            .field_access,
            .index_access,
            .integer,
            .char,
            .float,
            .group,
            => .{ .bin_op = .{
                .lhs = base.dedupe(),
                .op = std.enums.nameCast(Operator, op),
                .rhs = ExprNode.dedupe(.null),
            } },

            .bin_op => |bin| switch (bin.rhs.*) {
                .null => .{ .bin_op = .{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = ExprNode.dedupe(.{ .un_op = .{
                        .op = std.enums.nameCast(Operator, op),
                        .val = ExprNode.dedupe(.null),
                    } }),
                } },

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => blk: {
                    const old_prec = @field(precedence, @tagName(bin.op)) orelse @compileError(@tagName(bin.op) ++ " is not a binary operator");
                    const new_prec = @field(precedence, @tagName(op)) orelse @compileError(@tagName(op) ++ " is not a binary operator");
                    if (old_prec < 0 and new_prec < 0) {
                        @compileError(@tagName(bin.op) ++ " cannot be chained with " ++ @tagName(op));
                    }
                    if (old_prec < new_prec) {
                        break :blk .{ .bin_op = .{
                            .lhs = bin.lhs,
                            .op = bin.op,
                            .rhs = ExprNode.dedupe(.{ .bin_op = .{
                                .lhs = bin.rhs,
                                .op = std.enums.nameCast(Operator, op),
                                .rhs = ExprNode.dedupe(.null),
                            } }),
                        } };
                    }
                    break :blk .{ .bin_op = .{
                        .lhs = base.dedupe(),
                        .op = std.enums.nameCast(Operator, op),
                        .rhs = ExprNode.dedupe(.null),
                    } };
                },
                .bin_op, .un_op => .{ .bin_op = .{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = bin.rhs.concatOp(op).dedupe(),
                } },
            },
            .un_op => |un| switch (un.val.*) {
                .null => un.concatOp(op).*,

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => .{ .bin_op = .{
                    .lhs = base.dedupe(),
                    .op = op,
                    .rhs = ExprNode.dedupe(.null),
                } },

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
                .null => .{ .bin_op = .{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = new.dedupe(),
                } },

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
                => .{ .bin_op = .{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = bin.rhs.concatExpr(new).dedupe(),
                } },
            },
            .un_op => |un| un.insertExprAsInnerTarget(new),
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
            => .{ .field_access = .{
                .accessed = base.dedupe(),
                .accessor = dedupeSlice(u8, field),
            } },
            .bin_op => |bin| .{ .bin_op = .{
                .lhs = bin.lhs,
                .op = bin.op,
                .rhs = bin.rhs.concatFieldAccess(field).dedupe(),
            } },
            .un_op => |un| .{ .un_op = .{
                .op = un.op,
                .val = un.val.concatFieldAccess(field),
            } },
        };
    }

    const dedupe = struct {
        inline fn dedupe(comptime expr: ExprNode) *const ExprNode {
            comptime return switch (expr) {
                .null => dedupeImpl(expr),
                .ident => |ident| dedupeImpl(.{ .ident = dedupeSlice(u8, ident) }),
                .integer => dedupeImpl(expr),
                .char => dedupeImpl(expr),
                .float => |num| dedupeImpl(.{ .float = Number{ .src = dedupeSlice(u8, num.src) } }),
                .group => |group| dedupeImpl(.{ .group = group.dedupe() }),
                .field_access => |fa| dedupeImpl(.{ .field_access = .{
                    .accessed = fa.accessed.dedupe(),
                    .accessor = dedupeSlice(u8, fa.accessor),
                } }),
                .index_access => |ia| dedupeImpl(.{ .index_access = .{
                    .accessed = ia.accessed.dedupe(),
                    .accessor = ia.accessor.dedupe(),
                } }),
                .bin_op => |bin| dedupeImpl(.{ .bin_op = .{
                    .lhs = bin.lhs.dedupe(),
                    .op = bin.op,
                    .rhs = bin.rhs.dedupe(),
                } }),
                .un_op => |un| dedupeImpl(.{ .un_op = .{
                    .op = un.op,
                    .val = un.val.dedupe(),
                } }),
            };
        }
        fn dedupeImpl(comptime expr: ExprNode) *const ExprNode {
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
        accessed: ExprNode,
        accessor: ExprNode,

        inline fn dedupe(comptime ia: IndexAccess) IndexAccess {
            return .{
                .accessed = ia.accessed.dedupe(),
                .accessor = ia.accessor.dedupe(),
            };
        }
    };
    const BinOp = struct {
        lhs: *const ExprNode,
        op: Operator,
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
        op: Operator,
        val: *const ExprNode,

        inline fn dedupe(comptime un: UnOp) UnOp {
            return .{
                .op = un.op,
                .val = un.val.dedupe(),
            };
        }

        inline fn insertExprAsInnerTarget(comptime un: UnOp, comptime expr: ExprNode) ExprNode {
            return switch (un.val.*) {
                .null => .{ .un_op = .{
                    .op = un.op,
                    .val = expr.dedupe(),
                } },
                .un_op => |inner| .{ .un_op = .{
                    .op = un.op,
                    .val = inner.insertExprAsInnerTarget(expr).dedupe(),
                } },
                else => @compileError(std.fmt.comptimePrint("Unexpected token '{}'", .{un.val.fmt()})),
            };
        }

        inline fn concatOp(comptime un: UnOp, comptime op: Operator) *const ExprNode {
            if (un.concatOpInnerImpl(op)) |updated| {
                return ExprNode.dedupe(.{ .un_op = updated });
            }
            const op_tag = std.enums.nameCast(Operator, op);
            return .{ .bin_op = .{
                .lhs = ExprNode.dedupe(.{ .un_op = un }),
                .op = op_tag,
                .rhs = ExprNode.dedupe(.null),
            } };
        }

        /// returns null if the inner-most target of the unary operations is already present,
        /// meaning the unary operator can simply be used as the LHS of a binary operation
        inline fn concatOpInnerImpl(comptime un: UnOp, comptime op: Operator) ?UnOp {
            switch (un.val.*) {
                .null => return .{
                    .op = un.op,
                    .val = ExprNode.dedupe(.{ .un_op = .{
                        .op = std.enums.nameCast(Operator, op),
                        .val = ExprNode.dedupe(.null),
                    } }),
                },
                .un_op => |inner| return if (inner.concatOpInnerImpl(op)) |updated| .{
                    .op = un.op,
                    .val = ExprNode.dedupe(.{ .un_op = updated }),
                },
                else => return null,
            }
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
            const str = switch (formatter.expr) {
                .null => "null",
                .ident => |ident| ident,
                .field_access => |field| std.fmt.comptimePrint("{}.{s}", .{ field.accessed.fmt(), field.accessor }),
                .index_access => |index| std.fmt.comptimePrint("{}[{}]", .{ index.accessed.fmt(), index.accessor.fmt() }),
                .integer => |int| std.fmt.comptimePrint("{d}", .{int}),
                .char => |char| std.fmt.comptimePrint("'{u}'", .{@intFromEnum(char)}),
                .float => |num| std.fmt.comptimePrint(num.src),
                .group => |group| std.fmt.comptimePrint("({})", .{group.fmt()}),
                .bin_op => |bin_op| std.fmt.comptimePrint("{} {s} {}", .{ bin_op.lhs.fmt(), @tagName(bin_op.op), bin_op.rhs.fmt() }),
                .un_op => |un_op| std.fmt.comptimePrint("{s}{}", .{ @tagName(un_op.op), un_op.val.fmt() }),
            };
            try writer.writeAll(str);
        }
    };
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
        @setEvalBranchQuota(@min(std.math.maxInt(u32), (excluded.len + 1) * 100));
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
