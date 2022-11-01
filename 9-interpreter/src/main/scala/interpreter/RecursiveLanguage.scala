package interpreter

object RecursiveLanguage {
  /** Expression tree, also called Abstract Syntax Tree (AST) */
  enum Expr:
    case Constant(value: Int)
    case Name(name: String)
    case BinOp(op: BinOps, arg1: Expr, arg2: Expr)
    case IfNonzero(cond: Expr, caseTrue: Expr, caseFalse: Expr)
    case Call(function: Expr, arg: Expr)
    case Fun(param: String, body: Expr)

    /** The empty list, also known as nil. */
    case Empty

    /** A compound data type composed of a head and a tail. */
    case Cons(head: Expr, tail: Expr)

    /** A pattern matching expression for Empty and Cons. */
    case Match(scrutinee: Expr, caseEmpty: Expr, headName: String, tailName: String, caseCons: Expr)
  import Expr._

  /** Primitive operations that operation on constant values. */
  enum BinOps:
    case Plus, Minus, Times, DividedBy, Modulo, LessEq
  import BinOps._

  def evalBinOp(op: BinOps)(ex: Expr, ey: Expr): Expr =
    (op, ex, ey) match
      case (Plus,   Constant(x), Constant(y)) => Constant(x + y)
      case (Minus,  Constant(x), Constant(y)) => Constant(x - y)
      case (Times,  Constant(x), Constant(y)) => Constant(x * y)
      case (LessEq, Constant(x), Constant(y)) => Constant(if x <= y then 1 else 0)
      case (Modulo,    Constant(x), Constant(y)) => if y == 0 then error("Division by zero") else Constant(x % y)
      case (DividedBy, Constant(x), Constant(y)) => if y == 0 then error("Division by zero") else Constant(x / y)
      case _ => error(s"Type error in ${show(BinOp(op, ex, ey))}")

  type DefEnv = Map[String, Expr]

  /** Evaluates a progam e given a set of top level definition defs */
  def eval(e: Expr, defs: DefEnv): Expr =
    e match
      // Literals
      case Constant(c) => e
      case Fun(n, body) => e
      
      // Identifiers
      case Name(n) =>
        defs.get(n) match
          case None => error(s"Unknown name $n")
          case Some(body) => eval(body, defs)
      
      // Operations
      case BinOp(op, e1, e2) =>
        evalBinOp(op)(eval(e1, defs), eval(e2, defs))
      case IfNonzero(cond, caseTrue, caseFalse) =>
        if eval(cond, defs) != Constant(0) then eval(caseTrue, defs)
        else eval(caseFalse, defs)
      case Call(fun, arg) =>
        Logger.log(show(e))
        Logger.indent()
        val eFun = eval(fun, defs)
        val eArg = eval(arg, defs)
        eFun match
          case Fun(n, body) =>
            Logger.unindent()
            Logger.log(s"FUN: ${show(eFun)}  ARG: ${show(eArg)}")
            val bodySub = subst(body, n, eArg)
            Logger.log(s"${show(bodySub)}")
            Logger.indent()
            val res = eval(bodySub, defs)
            Logger.unindent()
            Logger.log(s"+--> ${show(res)}")
            res
          case _ => error(s"Cannot apply non-function ${show(eFun)} in a call")
      case Empty => Empty
      case Cons(head,tail) => Cons(eval(head,defs), eval(tail,defs))
      case Match(scrutinee, emptyCase, headName, tailName, consCase) => eval(scrutinee, defs) match
        case Empty => eval(emptyCase,defs)
        case Cons(head, tail) => 



        // case Cons(head,tail) => 
        //   val newConsCase = subst(subst(consCase, headName, eval(head, defs)), tailName, eval(tail, defs))
        //   eval(newConsCase, defs)
        // case _ => error(s"Cannot use $scrutinee in a matching expression")


  /** Substitutes Name(n) by r in e. */
  def subst(e: Expr, n: String, r: Expr): Expr =
    e match
      case Constant(c) => e
      case Name(s) => if s == n then r else e
      case BinOp(op, e1, e2) =>
        BinOp(op, subst(e1, n, r), subst(e2, n, r))
      case IfNonzero(cond, trueE, falseE) =>
        IfNonzero(subst(cond, n, r), subst(trueE, n, r), subst(falseE, n, r))
      case Call(f, arg) =>
        Call(subst(f, n, r), subst(arg, n, r))
      case Fun(param, body) =>
        // If n conflicts with param, there cannot be a reference to n in the
        // function body, there is nothing so substite.
        if param == n then e
        else
          val fvs = freeVars(r)
          // If the free variables in r contain param the naive substitution would
          // change the meaning of param to reference to the function parameter.
          if fvs.contains(param) then
            // Perform alpha conversion in body to eliminate the name collision.
            val param1 = differentName(param, fvs)
            val body1 = alphaConvert(body, param, param1)
            Fun(param1, subst(body1, n, r))
          else
            // Otherwise, substitute in the function body anyway.
            Fun(param, subst(body, n, r))
      case Empty => Empty
      case Cons(head, tail) => Cons(subst(head,n,r),subst(tail,n,r))
      case Match(scrutinee, caseEmpty, headName, tailName, caseCons) =>
        if headName == n || tailName == n then
          // If n conflicts with headName or tailName, there cannot be a reference
          // to n in caseCons. Simply substite n by r in scrutinee and caseEmpty.
          Match(
            subst(scrutinee, n, r),
            subst(caseEmpty, n, r),
            headName,
            tailName,
            caseCons
          )
        else
          // If the free variables in r contain headName or tailName, the naive
          // substitution would change their meaning to reference to pattern binds.

          // Perform alpha conversion in caseCons to eliminate the name collision.

          // Otherwise, substitute in scrutinee, caseEmpty & caseCons anyway.
          val freeVarsOfR = freeVars(r)
          val newHeadName = if freeVarsOfR contains headName then differentName(headName, freeVarsOfR + tailName) else headName 
          val newTailName = if freeVarsOfR contains tailName then differentName(tailName, freeVarsOfR + headName) else tailName
          val newCaseCons = alphaConvert(alphaConvert(caseCons, headName, newHeadName), tailName, newTailName)
          Match(
            subst(scrutinee, n, r),
            subst(caseEmpty, n, r),
            newHeadName,
            newTailName,
            subst(newCaseCons, n, r)
          )

  def differentName(n: String, s: Set[String]): String =
    if s.contains(n) then differentName(n + "'", s)
    else n

  /** Computes the set of free variable in e. */
  def freeVars(e: Expr): Set[String] =
    e match
      case Constant(c) => Set()
      case Name(s) => Set(s)
      case BinOp(op, e1, e2) => freeVars(e1) ++ freeVars(e2)
      case IfNonzero(cond, trueE, falseE) => freeVars(cond) ++ freeVars(trueE) ++ freeVars(falseE)
      case Call(f, arg) => freeVars(f) ++ freeVars(arg)
      case Fun(param, body) => freeVars(body) - param
      case Empty => Set()
      case Cons(head, tail) => freeVars(head) ++ freeVars(tail)
      case Match(scrutinee, caseEmpty, headName, tailName, caseCons) =>
        freeVars(scrutinee) ++ freeVars(caseEmpty) ++ freeVars(caseCons) - headName - tailName
      // DONE: Add cases for Empty, Cons & Match

  /** Substitutes Name(n) by Name(m) in e. */
  def alphaConvert(e: Expr, n: String, m: String): Expr =
    val convert: Expr => Expr = x => alphaConvert(x,n,m) // Hurray for curried functions
    e match
      case Constant(c) => e
      case Name(s) => if s == n then Name(m) else e
      case BinOp(op, e1, e2) =>
        BinOp(op, alphaConvert(e1, n, m), alphaConvert(e2, n, m))
      case IfNonzero(cond, trueE, falseE) =>
        IfNonzero(alphaConvert(cond, n, m), alphaConvert(trueE, n, m), alphaConvert(falseE, n, m))
      case Call(f, arg) =>
        Call(alphaConvert(f, n, m), alphaConvert(arg, n, m))
      case Fun(param, body) =>
        // If n conflicts with param, there cannot be references to n in body,
        // as these would reference param instead.
        if param == n then e
        else Fun(param, alphaConvert(body, n, m))
      case Empty => Empty
      case Cons(head,tail) => Cons(convert(head), convert(tail))
      case Match(scrutinee, emptyCase, headName, tailName, consCase) =>
        val newConsCase = if headName == n || tailName == n 
                          then consCase 
                          else convert(consCase)
        Match(convert(scrutinee),convert(emptyCase),headName, tailName, newConsCase)

  case class EvalException(msg: String) extends Exception(msg)

  def error(msg: String) = throw EvalException(msg)

  // Printing and displaying

  /** Pretty print an expression as a String. */
  def show(e: Expr): String =
    e match
      case Constant(c) => c.toString
      case Name(n) => n
      case BinOp(op, e1, e2) =>
        val opString = op match
          case Plus      => "+"
          case Minus     => "-"
          case Times     => "*"
          case DividedBy => "/"
          case Modulo    => "%"
          case LessEq    => "<="
        s"($opString ${show(e1)} ${show(e2)})"
      case IfNonzero(cond, caseTrue, caseFalse) =>
        s"(if ${show(cond)} then ${show(caseTrue)} else ${show(caseFalse)})"
      case Call(f, arg) => show(f) + "(" + show(arg) + ")"
      case Fun(n, body) => s"($n => ${show(body)})"
      case Empty => "[]"
      case Cons(head, tail) => s"${show(head)}::${show(tail)}"
      case Match(s,ec,h,t,cc) => s"""
        ${show(s)} match
          case [] => ${show(ec)}
          case $h::$t = ${show(cc)}
        """

  /** Pretty print top-level definition as a String. */
  def showEnv(env: Map[String, Expr]): String =
    env.map { case (name, body) => s"def $name =\n  ${show(body)}" }.mkString("\n\n") + "\n"

  /** Evaluates an expression with the given top-level definitions with logging enabled. */
  def tracingEval(e: Expr, defs: DefEnv): Expr =
    Logger.on()
    val evaluated = eval(e, defs)
    println(s" ~~> $evaluated\n")
    Logger.off()
    evaluated

  def minus(e1: Expr, e2: Expr)     = BinOp(BinOps.Minus,  e1, e2)
  def plus(e1: Expr, e2: Expr)      = BinOp(BinOps.Plus,   e1, e2)
  def leq(e1: Expr, e2: Expr)       = BinOp(BinOps.LessEq, e1, e2)
  def times(e1: Expr, e2: Expr)     = BinOp(BinOps.Times,  e1, e2)
  def modulo(e1: Expr, e2: Expr)    = BinOp(BinOps.Modulo, e1, e2)
  def dividedBy(e1: Expr, e2: Expr) = BinOp(BinOps.DividedBy, e1, e2)
  // The {Name => N} import syntax renames Name to N in this scope
  import Expr.{Name => N, Constant => C, _}

  /** Examples of top-level definitions (used in tests) */
  val definitions: DefEnv = Map[String, Expr](
    "fact" -> Fun("n",
       IfNonzero(N("n"),
         times(N("n"),
               Call(N("fact"), minus(N("n"), C(1)))),
         C(1))),

    "square" -> Fun("x",
      times(N("x"), N("x"))),

    "twice" -> Fun("f", Fun("x",
      Call(N("f"), Call(N("f"), N("x"))))),

    // TODO Implement map (see recitation session)
    "map" -> Fun("ls",Fun("f",
      Match(N("ls"),
          Empty,
        "x","xs",
          Cons(
            Call(N("f"),N("x"))
          ,
            Call(Call(N("map"),
              N("xs")
            ),
              N("f"))
          )
      )
    )),

    // TODO Implement gcd (see recitation session)
    "gcd" -> Fun("a",Fun("b",
      IfNonzero(N("b"),
        Call(Call(N("gcd"), 
          N("b")
        ),
          BinOp(Modulo,
            N("a")
          ,
            N("b")
          )
        )
      ,
        N("a")
    ))),
    // def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a%b)

    // TODO Implement foldLeft (see recitation session)
    "foldLeft" -> Fun("ls", Fun("acc", Fun("f",
      Match(N("ls"),
        N("acc")
      ,
      "x","xs",
        Call(Call(Call(N("foldLeft"),
          N("xs")
        ),
          Call(Call(N("f"),
            N("acc")
          ),
            N("x")
          )
        ),
          N("f")
        )
      )
    ))),


    // TODO Implement foldRight (analogous to foldLeft, but operate right-to-left)
    "foldRight" -> Fun("ls", Fun("acc",Fun("f",
      Match(N("ls"),
        N("acc")
      ,
      "x","xs",
        Call(Call(N("f"),
          N("x")
        ),
          Call(Call(Call(N("foldRight"),
            N("xs")
          ),
            N("acc")
          ),
            N("f")
          )
        )
      )
    )))
  )

  def main(args: Array[String]): Unit =
    println(showEnv(definitions))
    tracingEval(Call(N("fact"), C(6)), definitions)
    tracingEval(Call(Call(N("twice"), N("square")), C(3)), definitions)
    // val sum = "sum" -> Fun("a", Fun("b", BinOp(BinOps.Plus, N("a"), N("b"))))
    // val list1 = Cons(C(1), Cons(C(2), Cons(C(3), Empty)))
    // tracingEval(Call(Call(Call(N("foldLeft"), list1), C(100)), N("sum")), definitions + sum)
    // val expr = Call(Call(Call(Name("foldLeft"), list1), Constant(0)), Fun("a",Fun("x",BinOp(Plus,Name("a"),Name("x")))))
    // tracingEval(expr, definitions)
}
