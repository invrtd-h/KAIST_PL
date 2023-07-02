sealed trait Expr

case class Num(num: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Val(id: String, init: Expr, nextExpr: Expr) extends Expr
case class Id(id: String) extends Expr
case class Fun(argName: String, expr: Expr) extends Expr
case class App(f: Expr, arg: Expr) extends Expr

sealed trait Value
type Env = Map[String, Value]

case class NumV(num: Int) extends Value
case class CloV(argName: String, expr: Expr, env: Env) extends Value

def pret(expr: Expr, env: Env): Value = expr match {
  case Num(n) => NumV(n)
  case Add(lhs, rhs) =>
    val NumV(n) = pret(lhs, env)
    val NumV(m) = pret(rhs, env)
    NumV(n + m)
  case Sub(lhs, rhs) =>
    val NumV(n) = pret(lhs, env)
    val NumV(m) = pret(rhs, env)
    NumV(n - m)
  case Val(id, init, nextExpr) =>
    pret(nextExpr, env + (id -> pret(init, env)))
  case Id(id) => env(id)
  case Fun(argName, expr) => CloV(argName, expr, env)
  case App(f, arg) =>
    val CloV(argName, expr, fEnv) = pret(f, env)
    pret(expr, fEnv + (argName -> pret(arg, env)))
}