# comath: comptime math
The main function this library provides is `eval`, which is of the form
```zig
fn eval(
    comptime expr: []const u8,
    ctx: anytype,
    inputs: anytype,
) !Eval(expr, @TypeOf(ctx), @TypeOf(inputs))
```
wherein the `expr` is expected to be source code representing an expression comprised of:
* Identifiers, whose value should be assigned by a field in `inputs` of the same name, or be defined by the `ctx`.
* Integer, float, or character literals
* Grouped expressions delimited by parentheses
* Field accesses (`expression`.`identifier`|`operator symbols`)
* Index accesses (`expression`[`expressions`])
* Function calls (`expression`(`expressions`))
* Unary operations (`operator` `expression`)
* Binary operations (`expression` `operator` `expression`)

Given an expression that is successfully parsed into an AST at comptime, said AST will then be evaluated.
The semantics of all the operations are defined by the `ctx` parameter, which should be a type that conforms to a concrete interface,
which can be described in pseudo-code as:
```zig
{
    /// If the context namespace does not have it declared, it will be assumed to be false.
    /// If true, the `inputs` struct parameter of the `eval` function may contain
    /// fields which are not used in the `expr`.
    /// If false, a compile error will be issued should any of the fields in `inputs`
    /// not be referenced in `expr`.
    pub const allow_unused_inputs: bool = ...; // defaults to `false`

    /// Should return `true` for any string of symbols corresponding to a recognized unary operator.
    pub inline fn matchUnOp(comptime str: []const u8) bool {...}

    /// Should return `true` for any string of symbols corresponding to a recognized binary operator.
    pub inline fn matchBinOp(comptime str: []const u8) bool {...}

    /// Returns the order of the binary operators `lhs` and `rhs`, where `matchBinOp(lhs) = true`, and
    /// `matchBinOp(rhs) = true`.
    pub inline fn orderBinOp(comptime lhs: []const u8, comptime rhs: []const u8) operator.Order {...}

    /// Determines the value and type of number literals.
    pub fn EvalNumberLiteral(comptime src: []const u8) type {...}
    pub fn evalNumberLiteral(comptime src: []const u8) EvalNumberLiteral(src) {...}

    /// Determines the value and type of identifiers, overriding those which would otherwise be
    /// determined via the `inputs` struct. `EvalIdent` returning `noreturn` causes
    /// `eval` to instead look for the identifier in the `inputs` struct.
    pub fn EvalIdent(comptime ident: []const u8) type {...}
    pub fn evalIdent(ctx: @This(), comptime ident: []const u8) !EvalIdent(ident) {...}

    /// Corresponds to `lhs.accessor`.
    pub fn EvalProperty(comptime Lhs: type, comptime accessor: []const u8) type {...}
    pub fn evalProperty(ctx: @This(), lhs: anytype, comptime accessor: []const u8) !EvalProperty(@TypeOf(lhs), accessor) {...}

    /// Corresponds to `lhs[rhs...]`, where `rhs` is a tuple whose elements are the list of indices used to access `lhs`.
    pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {...}
    pub fn evalIndexAccess(ctx: @This(), lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {...}

    /// Corresponds to `callee(args...)`, where `args` is a tuple whose elements are the arguments to `callee`.
    pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {...}
    pub fn evalFuncCall(ctx: @This(), callee: anytype, args: anytype) !EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {...}

    /// Corresponds to `op value`.
    /// In most contexts, it should be sufficient to assume `@hasField(UnOp, op)`.
    pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {...}
    pub fn evalUnOp(ctx: @This(), comptime op: []const u8, value: anytype) !EvalUnOp(op, @TypeOf(value)) {...}

    /// Corresponds to `lhs op rhs`
    /// In most contexts, it should be sufficient to assume `@hasField(BinOp, op)`.
    pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {...}
    pub fn evalBinOp(ctx: @This(), lhs: anytype, comptime op: []const u8, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {...}
}
```

Some built-in contexts are provided in the `contexts` namespace, both for use and for reference.

Example code:
```zig
const std = @import("std");
const comath = @import("comath");

test {
    const ctx = comath.contexts.simpleCtx({});
    const value = comath.eval("a * 2", ctx, .{ .a = 4 }) catch |err| switch (err) {};
    try std.testing.expect(value == 8);
}
```

### Notable Implementations
To see an example of the terse notation enabled by comath, see [zilliam](https://github.com/ymndoseijin/zilliam), 
