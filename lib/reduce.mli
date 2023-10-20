val evaluated : Syntax.expr -> bool

exception Reduce

val top_reduction : Syntax.expr -> Syntax.expr
val eval : Syntax.expr -> Syntax.expr
val eval_steps : Syntax.expr -> Syntax.expr
