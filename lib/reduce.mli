val evaluated : Syntax.expr -> bool

exception Reduce

val top_reduction : Syntax.expr -> Syntax.expr
