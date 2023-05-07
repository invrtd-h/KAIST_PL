sealed trait Expr

case class Num(n: Int) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Val(x: String, i: Expr, b: Expr) extends Expr
case class Id(x: String) extends Expr

type Env = Map[String, Int]

def interpret(e: Expr, env: Env): Int = e match {
  case Num(n) => n
  case Add(l, r) => interpret(l, env) + interpret(r, env)
  case Sub(l, r) => interpret(l, env) - interpret(r, env)
  case Val(x, i, b) => interpret(b, env + (x -> interpret(i, env)))
  case Id(x) => env(x)
}

def interpret_from_scratch(e: Expr): Int = interpret(e, Map())

val tc1 = Val("x", Val("x", Num(3), Sub(Num(5), Id("x"))), Add(Num(1), Id("x")))
val tc2 = Val("x", Num(3), Val("y", Num(5), Add(Num(1), Id("x"))))
val tc3 = Val("x", Num(3), Val("x", Num(5), Add(Num(1), Id("x"))))
val tc4 = Val("x", Val("y", Num(0), Val("x", Num(0), Num(0))), Val("y", Val("y", Num(0), Num(0)), Num(0)))

interpret_from_scratch(tc1)
interpret_from_scratch(tc2)
interpret_from_scratch(tc3)
interpret_from_scratch(tc4)

def shadowed(e: Expr): Set[String] = {
  def helper(e: Expr, env: Set[String]): Set[String] = e match {
    case Num(_) => Set()
    case Add(l, r) => helper(l, env).concat(helper(r, env))
    case Sub(l, r) => helper(l, env).concat(helper(r, env))
    case Val(x, i, b) => {
      val next_env = env + x
      val ret: Set[String] = helper(i, next_env).concat(helper(b, next_env))
      if (env.contains(x)) {
        ret + x
      } else {
        ret
      }
    }
    case Id(_) => Set()
  }

  helper(e, Set())
}

shadowed(tc1)
shadowed(tc2)
shadowed(tc3)
shadowed(tc4)