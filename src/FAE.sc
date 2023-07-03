import scala.language.postfixOps

sealed trait Expr {
  def +(rhs: Expr) = Add(this, rhs)
  def plus(rhs: Expr) = Add(this, rhs)
  def -(rhs: Expr) = Sub(this, rhs)
  def minus(rhs: Expr) = Sub(this, rhs)

  def <|(arg: Expr) = App(this, arg)
  def apply(arg: Expr) = App(this, arg)
}

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

def pret(expr: Expr, env: Env = Map()): Value = expr match {
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

object sugar {
  implicit class NumSugar(num: Int) {
    def i: Num = Num(num)
  }

  implicit class IdSugar(id: String) {
    def id: Id = Id(id)
  }

  implicit class ValSugar(id: String) {
    def be(expr: Expr): Help = Help(id, expr)

    case class Help(id: String, init: Expr) {
      def in(nextExpr: Expr) = Val(id, init, nextExpr)
    }
  }
}

import sugar._

val lambda = (tup: (String, Expr)) => Fun(tup._1, tup._2)

pret(1.i + 3.i + 5.i - 2.i)

pret{
  "x" be 3.i in {
    "y" be 8.i in {
      "x".id + "y".id
    }
  }
}

pret{
  "f" be lambda("x" -> ("x".id + 3.i)) in {
    "f".id <| 4.i
  }
}