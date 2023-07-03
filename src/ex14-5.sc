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
case class If0(cond: Expr, trueExpr: Expr, falseExpr: Expr) extends Expr
case class Val(id: String, init: Expr, nextExpr: Expr) extends Expr
case class Id(id: String) extends Expr
case class Fun(argName: String, expr: Expr) extends Expr
case class App(f: Expr, arg: Expr) extends Expr

sealed trait Value
type Env = Map[String, Value]

case class NumV(num: Int) extends Value
case class CloV(argName: String, expr: Expr, env: Env) extends Value
case class ExprV(expr: Expr, env: Env, var cache: Option[Value]) extends Value

object wrapper {
  private def force(value: Value): Value = value match {
    case ExprV(_, _, Some(cache)) => cache
    case exprV@ExprV(e, env, None) =>
      val cache = force(pret(e, env))
      exprV.cache = Some(cache)
      cache
    case _ => value
  }

  def pret(expr: Expr, env: Env): Value = {
    println("expr: %s".format(expr))
    val ret = expr match {
      case Num(n) => NumV(n)
      case Add(lhs, rhs) =>
        val NumV(n) = force(pret(lhs, env))
        val NumV(m) = force(pret(rhs, env))
        NumV(n + m)
      case Sub(lhs, rhs) =>
        val NumV(n) = force(pret(lhs, env))
        val NumV(m) = force(pret(rhs, env))
        NumV(n - m)
      case If0(cond, trueExpr, falseExpr) =>
        force(pret(cond, env)) match {
          case NumV(condVal) =>
            if (condVal == 0)
              pret(trueExpr, env)
            else
              pret(falseExpr, env)
          case CloV(_, _, _) => pret(falseExpr, env)
        }
      case Val(id, init, nextExpr) =>
        pret(nextExpr, env + (id -> pret(init, env)))
      case Id(id) => env(id)
      case Fun(argName, expr) => CloV(argName, expr, env)
      case App(f, arg) =>
        val CloV(argName, expr, fEnv) = force(pret(f, env))
        pret(expr, fEnv + (argName -> ExprV(arg, env, None)))
    }
    println("ret: %s".format(expr, ret))
    ret
  }
}

import wrapper._

def pret0(expr: Expr): Value = pret(expr, Map())

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

try {
  pret0 {
    lambda("y", "y".id.apply(3.i)).apply(
      lambda("x", 1.i.apply(2.i))
    )
  }
} catch {
  case e: Exception => e
}

/*
Execution result:

expr: App(Fun(y,App(Id(y),Num(3))),Fun(x,App(Num(1),Num(2))))
expr: Fun(y,App(Id(y),Num(3)))
ret: Fun(y,App(Id(y),Num(3)))
expr: App(Id(y),Num(3))
expr: Id(y)
ret: Id(y)
expr: Fun(x,App(Num(1),Num(2)))
ret: Fun(x,App(Num(1),Num(2)))
expr: App(Num(1),Num(2))
expr: Num(1)
ret: Num(1)
val res0: Object = scala.MatchError: NumV(1) (of class NumV)
 */

try {
  pret0 {
    "f" be lambda("x" -> "y".id) in {
      3.i + 4.i
    }
  }
}

/*
Execution result:

expr: Val(f,Fun(x,Id(y)),Add(Num(3),Num(4)))
expr: Fun(x,Id(y))
ret: Fun(x,Id(y))
expr: Add(Num(3),Num(4))
expr: Num(3)
ret: Num(3)
expr: Num(4)
ret: Num(4)
ret: Add(Num(3),Num(4))
ret: Val(f,Fun(x,Id(y)),Add(Num(3),Num(4)))
val res1: Value = NumV(7)

Note that the wrong function f is not evaluated.
 */