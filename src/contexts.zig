const std = @import("std");
const comath = @import("main.zig");
const operator = comath.operator;
const util = @import("util");

pub fn DefaultEvalNumberLiteral(comptime src: []const u8) type {
    return switch (std.zig.parseNumberLiteral(src)) {
        .int, .big_int => comptime_int,
        .float => comptime_float,
        .failure => noreturn,
    };
}
pub fn defaultEvalNumberLiteral(comptime src: []const u8) DefaultEvalNumberLiteral(src) {
    return switch (std.zig.parseNumberLiteral(src)) {
        .int => |val| val,
        .big_int => |base| blk: {
            var x = 0;
            const digits_start = if (base == .decimal) 0 else 2;
            for (src[digits_start..]) |digit| {
                const val = switch (digit) {
                    '0'...'9' => digit - '0',
                    'A'...'F' => digit - 'A' + 10,
                    'a'...'f' => digit - 'a' + 10,
                    else => @compileError("Invalid integer literal '" ++ src ++ "'"),
                };
                if (val >= @intFromEnum(base)) @compileError("Invalid integer literal '" ++ src ++ "'");
                x *= @intFromEnum(base);
                x += val;
            }
            break :blk x;
        },
        .float => std.fmt.parseFloat(f128, src) catch |err| @compileError(@errorName(err)),
        .failure => |failure| @compileError(stringifyNumberParseFailure(src, failure)),
    };
}

inline fn stringifyNumberParseFailure(
    comptime num_src: []const u8,
    comptime failure: std.zig.number_literal.Error,
) []const u8 {
    switch (failure) {
        .leading_zero => "Invalid leading zeroes in '" ++ num_src ++ "'",
        .digit_after_base => "Expected digit after base in '" ++ num_src ++ "'",
        .upper_case_base, .invalid_float_base => "Invalid base in '" ++ num_src ++ "'",
        .repeated_underscore, .invalid_underscore_after_special => "Invalid underscore in '" ++ num_src ++ "'",
        .invalid_digit => |info| std.fmt.comptimePrint(
            "Invalid digit '{c}' in '{s}' with base '{s}'",
            .{ num_src[info.i], num_src, @tagName(info.base) },
        ),
        .invalid_digit_exponent => |exp_idx| std.fmt.comptimePrint(
            "Invalid exponent '{c}' in '{s}'",
            .{ num_src[exp_idx], num_src },
        ),
        .duplicate_period => "Duplicate periods in '" ++ num_src ++ "'",
        .duplicate_exponent => "Duplicate exponents in '" ++ num_src ++ "'",
        .exponent_after_underscore => "Exponent after underscore in '" ++ num_src ++ "'",
        .special_after_underscore => |spec_idx| std.fmt.comptimePrint(
            "Invalid '{c}' after underscore in '{s}'",
            .{ num_src[spec_idx], num_src },
        ),
        .trailing_special,
        .trailing_underscore,
        .invalid_character,
        .invalid_exponent_sign,
        => |err_idx| std.fmt.comptimePrint(
            "Invalid '{c}' in '{s}'",
            .{ num_src[err_idx], num_src },
        ),
    }
}

pub fn DefaultEvalIdent(comptime ident: []const u8) type {
    _ = ident;
    return noreturn;
}
pub fn defaultEvalIdent(ctx: anytype, comptime ident: []const u8) error{}!DefaultEvalIdent(ident) {
    _ = ctx;
    comptime unreachable;
}

const SimpleUnOp = enum { @"-" };
const SimpleBinOp = enum {
    @"+",
    @"+|",
    @"+%",

    @"-",
    @"-|",
    @"-%",

    @"*",
    @"*|",
    @"*%",

    @"/",
    @"%",

    @"^",
    @"@",
};
const simple_relations = .{
    .@"+" = .{ .prec = 10, .assoc = .left },
    .@"+|" = .{ .prec = 10, .assoc = .left },
    .@"+%" = .{ .prec = 10, .assoc = .left },

    .@"-" = .{ .prec = 10, .assoc = .left },
    .@"-|" = .{ .prec = 10, .assoc = .left },
    .@"-%" = .{ .prec = 10, .assoc = .left },

    .@"*" = .{ .prec = 20, .assoc = .left },
    .@"*|" = .{ .prec = 20, .assoc = .left },
    .@"*%" = .{ .prec = 20, .assoc = .left },

    .@"/" = .{ .prec = 20, .assoc = .left },
    .@"%" = .{ .prec = 20, .assoc = .left },

    .@"^" = .{ .prec = 30, .assoc = .right },
    .@"@" = .{ .prec = 0, .assoc = .left },
};

pub inline fn simpleCtx(sub_ctx: anytype) SimpleCtx(@TypeOf(sub_ctx)) {
    return .{ .sub_ctx = sub_ctx };
}
pub fn SimpleCtx(comptime SubCtx: type) type {
    return struct {
        sub_ctx: SubCtx,
        const Self = @This();

        pub const allow_unused_inputs = @hasDecl(Ns, "allow_unused_inputs") and Ns.allow_unused_inputs;

        pub const UnOp = if (!@hasDecl(Ns, "UnOp")) SimpleUnOp else operator.OpEnumUnion(SimpleUnOp, Ns.UnOp);
        pub const BinOp = if (!@hasDecl(Ns, "BinOp")) SimpleBinOp else operator.OpEnumUnion(SimpleBinOp, Ns.BinOp);
        pub const relations: operator.RelationMap(BinOp) = if (!@hasDecl(Ns, "relations"))
            simple_relations // if this issues an error about missing fields, you need to specify the relations of your custom operators in your SubCtx
        else
            Ns.relations;

        pub const EvalNumberLiteral = DefaultEvalNumberLiteral;
        pub const evalNumberLiteral = defaultEvalNumberLiteral;

        pub const EvalIdent = comath.contexts.DefaultEvalIdent;
        pub const evalIdent = comath.contexts.defaultEvalIdent;

        pub fn EvalProperty(comptime Lhs: type, comptime field: []const u8) type {
            if (@hasDecl(Ns, "EvalProperty")) {
                if (Ns.EvalProperty(Lhs, field) != noreturn) {
                    return Ns.EvalProperty(Lhs, field);
                }
            }
            return std.meta.FieldType(Lhs, field);
        }
        pub inline fn evalProperty(ctx: Self, lhs: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(lhs), field) {
            const Lhs = @TypeOf(lhs);
            if (@hasDecl(Ns, "EvalProperty")) {
                if (Ns.EvalProperty(Lhs, field) != noreturn) {
                    return ctx.sub_ctx.evalProperty(lhs, field);
                }
            }
            return @field(lhs, field);
        }

        pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {
            if (@hasDecl(Ns, "EvalIndexAccess")) {
                if (Ns.EvalIndexAccess(Lhs, Rhs) != noreturn) {
                    return Ns.EvalIndexAccess(Lhs, Rhs);
                }
            }
            return std.meta.Elem(Lhs);
        }
        pub inline fn evalIndexAccess(ctx: Self, lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);
            if (@hasDecl(Ns, "EvalIndexAccess")) {
                if (Ns.EvalIndexAccess(Lhs, Rhs) != noreturn) {
                    return ctx.sub_ctx.evalIndexAccess(lhs, rhs);
                }
            }
            return lhs[rhs];
        }

        pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {
            if (@hasDecl(Ns, "EvalFuncCall")) {
                if (Ns.EvalFuncCall(Callee, Args) != noreturn) {
                    return Ns.EvalFuncCall(Callee, Args);
                }
            }
            const Ret = @typeInfo(util.ImplicitDeref(Callee)).Fn.return_type orelse blk: {
                const callee: Callee = undefined;
                const args: Args = undefined;
                break :blk @TypeOf(@call(.auto, callee, args));
            };
            return util.GetPayloadIfErrorUnion(Ret);
        }
        pub inline fn evalFuncCall(ctx: Self, callee: anytype, args: anytype) !EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {
            const Callee = @TypeOf(callee);
            const Args = @TypeOf(args);

            if (@hasDecl(Ns, "EvalFuncCall")) {
                if (Ns.EvalFuncCall(Callee, Args) != noreturn) {
                    return ctx.sub_ctx.evalFuncCall(callee, args);
                }
            }
            return @call(.auto, callee, args);
        }

        pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {
            if (@hasDecl(Ns, "EvalUnOp")) {
                if (Ns.EvalUnOp(op, T) != noreturn) {
                    return Ns.EvalUnOp(op, T);
                }
            }
            return switch (@field(UnOp, op)) {
                .@"-" => T,
            };
        }
        pub inline fn evalUnOp(ctx: Self, comptime op: []const u8, val: anytype) !EvalUnOp(op, @TypeOf(val)) {
            if (@hasDecl(Ns, "EvalUnOp")) {
                if (Ns.EvalUnOp(op, @TypeOf(val)) != noreturn) {
                    return ctx.sub_ctx.evalUnOp(op, val);
                }
            }
            return switch (@field(UnOp, op)) {
                .@"-" => -val,
            };
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            if (@hasDecl(Ns, "EvalBinOp")) {
                if (Ns.EvalBinOp(Lhs, op, Rhs) != noreturn) {
                    return Ns.EvalBinOp(Lhs, op, Rhs);
                }
            }

            const lhs: Lhs = std.mem.zeroes(Lhs);
            const rhs: Rhs = std.mem.zeroes(Rhs);
            return switch (@field(BinOp, op)) {
                .@"+" => @TypeOf(lhs + rhs),
                .@"+%" => @TypeOf(lhs +% rhs),
                .@"+|" => @TypeOf(lhs +| rhs),

                .@"-" => @TypeOf(lhs - rhs),
                .@"-%" => @TypeOf(lhs -% rhs),
                .@"-|" => @TypeOf(lhs -| rhs),

                .@"*" => @TypeOf(lhs * rhs),
                .@"*%" => @TypeOf(lhs *% rhs),
                .@"*|" => @TypeOf(lhs *| rhs),

                .@"/" => @TypeOf(lhs / rhs),
                .@"%" => @TypeOf(lhs % rhs),

                .@"^" => @TypeOf(lhs ^ rhs),
                .@"@" => [rhs.len][lhs[0].len]@TypeOf(lhs[0][0]),
            };
        }
        pub inline fn evalBinOp(ctx: Self, lhs: anytype, comptime op: []const u8, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);
            if (@hasDecl(Ns, "EvalBinOp")) {
                if (Ns.EvalBinOp(Lhs, op, Rhs) != noreturn) {
                    return ctx.sub_ctx.evalBinOp(lhs, op, rhs);
                }
            }
            return switch (@field(BinOp, op)) {
                .@"+" => lhs + rhs,
                .@"+%" => lhs +% rhs,
                .@"+|" => lhs +% rhs,

                .@"-" => lhs - rhs,
                .@"-%" => lhs -% rhs,
                .@"-|" => lhs -% rhs,

                .@"*" => lhs * rhs,
                .@"*%" => lhs *% rhs,
                .@"*|" => lhs *% rhs,

                .@"/" => lhs / rhs,
                .@"%" => lhs % rhs,

                .@"^" => blk: {
                    const T = @TypeOf(lhs, rhs);
                    comptime if (T == comptime_int or
                        T == comptime_float)
                    {
                        @setEvalBranchQuota(@min(std.math.maxInt(u32), rhs * 10));
                        var x: T = 1;
                        for (0..rhs) |_| x *= lhs;
                        break :blk x;
                    };

                    break :blk std.math.pow(T, lhs, rhs);
                },
                .@"@" => blk: {
                    const columns_num = lhs[0].len;
                    const rows_num = rhs.len;
                    const T = @TypeOf(lhs[0][0]);

                    @setEvalBranchQuota(1000 + rhs.len * 10);
                    var res: [columns_num][rows_num]T = undefined;
                    inline for (rhs, 0..) |prev_column, i| {
                        var column: @Vector(columns_num, T) = .{0} ** columns_num;

                        inline for (0..lhs.len) |j| {
                            const mask = ([1]i32{@intCast(j)}) ** columns_num;
                            var vi = @shuffle(T, prev_column, undefined, mask);

                            vi = vi * lhs[j];
                            column += vi;
                        }

                        res[i] = column;
                    }
                    break :blk res;
                },
            };
        }

        const Ns = switch (@typeInfo(SubCtx)) {
            .Struct, .Union, .Enum => SubCtx,
            .Pointer => |pointer| if (pointer.size != .One)
                struct {}
            else switch (@typeInfo(pointer.child)) {
                .Struct, .Union, .Enum, .Opaque => pointer.child,
                else => struct {},
            },
            else => struct {},
        };
    };
}

test simpleCtx {
    try util.testing.expectEqual(5, comath.eval("a + b", simpleCtx({}), .{ .a = 2, .b = 3 }));
    try util.testing.expectEqual(1, comath.eval("a +% b", simpleCtx({}), .{ .a = @as(u8, std.math.maxInt(u8)), .b = 2 }));
    try util.testing.expectEqual(59049, comath.eval("3^(2 * a + -b)", simpleCtx({}), .{ .a = 7, .b = 4 }));

    try util.testing.expectEqual([2][2]u16{ .{ 140, 320 }, .{ 146, 335 } }, comath.eval("a @ b", simpleCtx({}), .{
        .a = [3][2]u16{ .{ 1, 4 }, .{ 2, 5 }, .{ 3, 6 } },
        .b = [2][3]u16{ .{ 10, 20, 30 }, .{ 11, 21, 31 } },
    }));
    try util.testing.expectEqual([2][2]u16{ .{ 188, 422 }, .{ 207, 468 } }, comath.eval("a @ b @ c", simpleCtx({}), .{
        .a = [3][2]u16{ .{ 1, 4 }, .{ 2, 5 }, .{ 3, 6 } },
        .b = [2][3]u16{ .{ 1, 2, 4 }, .{ 2, 3, 6 } },
        .c = [2][2]u16{ .{ 8, 2 }, .{ 3, 6 } },
    }));

    // complex precedence interactions
    try util.testing.expectEqual(6 - 3 + 4 + 2, comath.eval("6*1-3*1+4*1+2", simpleCtx({}), .{}));

    const op_override_ctx = simpleCtx(struct {
        pub const UnOp = enum { @"++" };
        pub const BinOp = enum { @"^", @"$" };
        pub const relations = .{
            .@"$" = .{ .prec = 2, .assoc = .right },
        };
        pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {
            return switch (@field(UnOp, op)) {
                .@"++" => T,
            };
        }
        pub fn evalUnOp(_: @This(), comptime op: []const u8, val: anytype) EvalUnOp(op, @TypeOf(val)) {
            return switch (@field(UnOp, op)) {
                .@"++" => val + 1,
            };
        }
        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            return switch (@field(BinOp, op)) {
                .@"^", .@"$" => @TypeOf(
                    @as(Lhs, undefined),
                    @as(Rhs, undefined),
                ),
            };
        }
        pub fn evalBinOp(_: @This(), lhs: anytype, comptime op: []const u8, rhs: anytype) EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            return switch (@field(BinOp, op)) {
                .@"^" => lhs ^ rhs,
                .@"$" => std.math.log(@TypeOf(lhs, rhs), lhs, rhs),
            };
        }
    }{});
    try util.testing.expectEqual(2, comath.eval("(++2 ^ 5) $ 36", op_override_ctx, .{}));
}

pub inline fn fnMethodCtx(
    sub_ctx: anytype,
    /// must be a struct literal wherein each field name is an operator,
    /// with a string literal value corresponding to the method name, or a list of possible method names.
    /// ie `.{ .@"+" = "add", .@"-" = &.{ "sub", "neg" } }`
    comptime method_names: anytype,
) FnMethodCtx(@TypeOf(sub_ctx), method_names) {
    return .{ .sub_ctx = sub_ctx };
}
pub fn FnMethodCtx(
    comptime SubCtx: type,
    comptime method_names: anytype,
) type {
    {
        const Deduped = DedupedMethodNames(method_names);
        if (@TypeOf(method_names) != Deduped) {
            return FnMethodCtx(SubCtx, Deduped{});
        }
    }
    return struct {
        sub_ctx: SubCtx,
        const Self = @This();

        pub const allow_unused_inputs = @hasDecl(Ns, "allow_unused_inputs") and Ns.allow_unused_inputs;

        pub const UnOp = Ns.UnOp;
        pub const BinOp = Ns.BinOp;
        pub const relations = Ns.relations;

        pub fn EvalNumberLiteral(comptime src: []const u8) type {
            if (@hasDecl(Ns, "EvalNumberLiteral")) {
                return Ns.EvalNumberLiteral(src);
            }
            return DefaultEvalNumberLiteral(src);
        }
        pub inline fn evalNumberLiteral(comptime src: []const u8) EvalNumberLiteral(src) {
            if (@hasDecl(Ns, "evalNumberLiteral")) {
                return Ns.evalNumberLiteral(src);
            }
            return defaultEvalNumberLiteral(src);
        }

        pub fn EvalIdent(comptime ident: []const u8) type {
            if (@hasDecl(Ns, "EvalIdent")) {
                return Ns.EvalIdent(ident);
            }
            return DefaultEvalIdent(ident);
        }
        pub fn evalIdent(comptime ident: []const u8) type {
            if (@hasDecl(Ns, "EvalIdent")) {
                return Ns.evalIdent(ident);
            }
            return defaultEvalIdent(ident);
        }

        pub fn EvalProperty(comptime T: type, comptime field: []const u8) type {
            if (getOpMapping(T, "evalProperty", method_names, 2)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(T, name)));
                const method_info = @typeInfo(Method).Fn;
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const val: (method_info.params[0].type orelse T) = undefined;
                const field_arg = if (method_info.params[1].type) |FieldArg| castStringTo(FieldArg, field) else field;
                return util.GetPayloadIfErrorUnion(@TypeOf(@field(T, name)(val, field_arg)));
            }
            return Ns.EvalProperty(T, field);
        }
        pub inline fn evalProperty(ctx: Self, val: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(val), field) {
            const T = @TypeOf(val);
            if (getOpMapping(@TypeOf(val), "evalProperty", method_names, 2)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(T, name)));
                const method_info = @typeInfo(Method).Fn;

                const field_arg = if (method_info.params[1].type) |FieldArg| castStringTo(FieldArg, field) else field;
                return @field(T, name)(val, field_arg);
            }
            return ctx.sub_ctx.evalProperty(val, field);
        }

        pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {
            if (getOpMapping(Lhs, "evalIndexAccess", method_names, 2)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(Rhs, name)));
                const method_info = @typeInfo(Method).Fn;
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const lhs: (method_info.params[0].type orelse Lhs) = undefined;
                const rhs: (method_info.params[0].type orelse Rhs) = undefined;
                return util.GetPayloadIfErrorUnion(@TypeOf(@field(Lhs, name)(lhs, rhs)));
            }
            return Ns.EvalIndexAccess(Lhs, Rhs);
        }
        pub inline fn evalIndexAccess(ctx: Self, lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            if (getOpMapping(Lhs, "evalIndexAccess", method_names, 2)) |name| {
                return @field(Lhs, name)(lhs, rhs);
            }
            return ctx.sub_ctx.evalIndexAccess(lhs, rhs);
        }

        pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {
            const args_info = @typeInfo(Args).Struct;
            if (getOpMapping(Callee, "evalFuncCall", method_names, 1 + args_info.fields.len)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(Callee, name)));
                const method_info = @typeInfo(Method).Fn;
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const lhs: (method_info.params[0].type orelse Callee) = undefined;
                const rhs: (method_info.params[0].type orelse Args) = undefined;
                return util.GetPayloadIfErrorUnion(@TypeOf(@call(.auto, @field(Callee, name), .{lhs} ++ rhs)));
            }
            return Ns.EvalFuncCall(Callee, Args);
        }
        pub inline fn evalFuncCall(ctx: Self, callee: anytype, args: anytype) !EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {
            const Callee = @TypeOf(callee);
            const Args = @TypeOf(args);
            const args_info = @typeInfo(Args).Struct;
            if (getOpMapping(Callee, "evalFuncCall", method_names, 1 + args_info.fields.len)) |name| {
                return @call(.auto, @field(Callee, name), .{callee} ++ args);
            }
            return ctx.sub_ctx.evalFuncCall(callee, args);
        }

        pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {
            if (getOpMapping(T, op, method_names, 1)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(T, name)));
                const method_info = @typeInfo(Method).Fn;
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const val: (method_info.params[0].type orelse T) = undefined;
                return util.GetPayloadIfErrorUnion(@TypeOf(@field(T, name)(val)));
            }
            return Ns.EvalUnOp(op, T);
        }
        pub inline fn evalUnOp(ctx: Self, comptime op: []const u8, val: anytype) !EvalUnOp(op, @TypeOf(val)) {
            const Val = @TypeOf(val);
            if (comptime getOpMapping(Val, op, method_names, 1)) |name| {
                return @field(Val, name)(val);
            }
            return ctx.sub_ctx.evalUnOp(op, val);
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            if (comptime getOpMapping(Lhs, op, method_names, 2)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(Lhs, name)));
                const method_info = @typeInfo(Method).Fn;
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const lhs: (method_info.params[0].type orelse Lhs) = undefined;
                const rhs: (method_info.params[1].type orelse Rhs) = undefined;
                return util.GetPayloadIfErrorUnion(@TypeOf(@field(Lhs, name)(lhs, rhs)));
            }
            return Ns.EvalBinOp(Lhs, op, Rhs);
        }
        pub inline fn evalBinOp(ctx: Self, lhs: anytype, comptime op: []const u8, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            if (comptime getOpMapping(Lhs, op, method_names, 2)) |name| {
                return @field(Lhs, name)(lhs, rhs);
            }
            return ctx.sub_ctx.evalBinOp(lhs, op, rhs);
        }

        const Ns = switch (@typeInfo(SubCtx)) {
            .Struct, .Union, .Enum => SubCtx,
            .Pointer => |pointer| if (pointer.size != .One)
                struct {}
            else switch (@typeInfo(pointer.child)) {
                .Struct, .Union, .Enum, .Opaque => pointer.child,
                else => struct {},
            },
            else => struct {},
        };
    };
}

inline fn castStringTo(comptime T: type, comptime str: []const u8) T {
    comptime switch (@typeInfo(T)) {
        .Enum => return @field(T, str),
        .Array => return str[0..].*,
        .Pointer => |pointer| ptr: {
            var sentinel: ?u8 = null;
            switch (pointer.size) {
                .Slice, .Many => {
                    if (pointer.child != u8) break :ptr;
                    if (pointer.sentinel) |maybe_ptr| blk: {
                        const ptr = maybe_ptr orelse break :blk;
                        sentinel = @as(*align(1) const pointer.child, @ptrCast(ptr)).*;
                    }
                },
                .One => switch (@typeInfo(pointer.child)) {
                    .Enum => return &@field(pointer.child, str),
                    .Array => |array| {
                        if (array.child != u8) break :ptr;
                        if (array.sentinel) |maybe_ptr| blk: {
                            const ptr = maybe_ptr orelse break :blk;
                            sentinel = @as(*align(1) const pointer.child, @ptrCast(ptr)).*;
                        }
                    },
                },
                else => break :ptr,
            }
            return if (sentinel) |s|
                str[0..] ++ &[_:s]u8{}
            else
                str[0..];
        },
        else => {},
    };
    @compileError("Can't cast to type " ++ @typeName(T));
}

inline fn getOpMapping(
    comptime Operand: type,
    comptime op: []const u8,
    comptime mapping: anytype,
    comptime arity: comptime_int,
) ?[]const u8 {
    comptime {
        if (!@hasField(@TypeOf(mapping), op)) return null;
        const entry = @field(mapping, op);

        const T = util.ImplicitDeref(Operand);
        switch (@typeInfo(T)) {
            .Struct, .Union, .Enum, .Opaque => {},
            else => return null,
        }

        const is_single = @TypeOf(entry) == []const u8;
        const entry_list: []const []const u8 = if (is_single) &.{entry} else entry;

        var idx: comptime_int = entry_list.len;

        const result = for (entry_list, 0..) |name, i| {
            if (!@hasDecl(T, name)) continue;
            const Fn = util.ImplicitDeref(@TypeOf(@field(T, name)));
            if (@typeInfo(Fn).Fn.params.len != arity) continue;
            idx = i + 1;
            break name;
        } else null;

        for (entry_list[idx..]) |next_name| {
            if (!@hasDecl(T, next_name)) continue;
            const NextFn = util.ImplicitDeref(@TypeOf(@field(T, next_name)));
            if (@typeInfo(NextFn).Fn.params.len != arity) continue;
            @compileError("Ambiguous resolution between method '" ++ result.? ++ "' and '" ++ next_name ++ "'");
        }

        return result;
    }
}

fn DedupedMethodNames(comptime method_names: anytype) type {
    const info = @typeInfo(@TypeOf(method_names)).Struct;

    @setEvalBranchQuota(1000 + info.fields.len * 10);
    var fields: [info.fields.len]std.builtin.Type.StructField = undefined;
    for (&fields, info.fields) |*new, old| {
        const T = switch (@typeInfo(old.type)) {
            .Pointer => |pointer| switch (@typeInfo(pointer.child)) {
                .Int, .ComptimeInt => []const u8,
                .Array => |array| switch (@typeInfo(array.child)) {
                    .Int, .ComptimeInt => []const u8,
                    else => []const []const u8,
                },
                else => []const []const u8,
            },
            else => @compileError("Unexpected type " ++ @typeName(old.type)),
        };
        const old_value: T = @as(*align(1) const old.type, @ptrCast(old.default_value)).*;
        new.* = .{
            .name = util.dedupe.scalarSlice(u8, old.name[0..].*),
            .type = T,
            .is_comptime = true,
            .default_value = @ptrCast(util.dedupe.scalarValue(old_value)),
            .alignment = 0,
        };
    }

    const deduped_fields = util.dedupe.scalarSlice(std.builtin.Type.StructField, fields);
    return DedupedMethodNamesImpl(deduped_fields);
}
fn DedupedMethodNamesImpl(comptime fields: []const std.builtin.Type.StructField) type {
    return @Type(.{ .Struct = .{
        .layout = .Auto,
        .backing_integer = null,
        .is_tuple = false,
        .decls = &.{},
        .fields = fields,
    } });
}

test fnMethodCtx {
    const CustomNum = enum(i32) {
        _,

        pub inline fn from(value: anytype) @This() {
            return @enumFromInt(value);
        }

        pub inline fn add(self: @This(), other: @This()) @This() {
            return @enumFromInt(@intFromEnum(self) + @intFromEnum(other));
        }
        pub inline fn sub(self: @This(), other: @This()) @This() {
            return @enumFromInt(@intFromEnum(self) - @intFromEnum(other));
        }
        pub inline fn neg(self: @This()) @This() {
            return @enumFromInt(-@intFromEnum(self));
        }
        pub inline fn mul(self: @This(), other: anytype) @This() {
            return @enumFromInt(@intFromEnum(self) * switch (@TypeOf(other)) {
                @This() => @intFromEnum(other),
                comptime_int => other,
                else => |Other| @compileError("Unexpected type " ++ @typeName(Other)),
            });
        }
    };

    const fm_ctx = fnMethodCtx(simpleCtx({}), .{
        .@"+" = "add",
        .@"-" = &.{ "sub", "neg" },
        .@"*" = "mul",
        .evalFuncCall = "mul",
    });
    try util.testing.expectEqual(CustomNum.from(2), comath.eval("a + -b - c", fm_ctx, .{
        .a = @as(CustomNum, @enumFromInt(22)),
        .b = @as(CustomNum, @enumFromInt(9)),
        .c = @as(CustomNum, @enumFromInt(11)),
    }));
    try util.testing.expectEqual(CustomNum.from(77), comath.eval("a(b) * 1", fm_ctx, .{
        .a = @as(CustomNum, @enumFromInt(11)),
        .b = @as(CustomNum, @enumFromInt(7)),
    }));
    try util.testing.expectEqual(CustomNum.from(62), comath.eval("num(31)(2)", fm_ctx, .{
        .num = CustomNum.from,
    }));
}
