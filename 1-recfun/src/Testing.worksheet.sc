enum Expr:
    class Number(x: Int)
    class Var(x: String)
    class Sum(x: Expr, y: Expr)
    class Prod(x: Expr, y: Expr)

