pub const Eval = @import("eval.zig").Eval;
pub const eval = @import("eval.zig").eval;

pub const operator = @import("operator.zig");
pub const contexts = @import("contexts.zig");

pub const Char = @import("parse.zig").Char;
pub const Number = @import("parse.zig").Number;

comptime {
    _ = @import("eval.zig");
    _ = operator;
    _ = contexts;
    _ = Char;
    _ = Number;
}
