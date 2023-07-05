import scala.language.postfixOps
import scala.sys.error

object myMain {
  sealed trait Expr {
    def +(rhs: Expr): Expr = Add(this, rhs)
    def plus(rhs: Expr): Expr = Add(this, rhs)
    def -(rhs: Expr): Expr = Sub(this, rhs)
    def minus(rhs: Expr): Expr = Sub(this, rhs)
    def <|(arg: Expr): Expr = App(this, arg)
    def apply(arg: Expr): Expr = App(this, arg)
    def _1: Expr = First(this)
    def _2: Expr = Second(this)
    def getHead: Expr = Head(this)
    def getTail: Expr = Tail(this)
    def ::(rhs: Expr): Expr = Cons(rhs, this)
  }

  case class Num(num: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr
  case class If0(cond: Expr, trueExpr: Expr, falseExpr: Expr) extends Expr
  case class Val(id: String, init: Expr, nextExpr: Expr) extends Expr
  case class Id(id: String) extends Expr
  case class Fun(argName: String, expr: Expr) extends Expr
  case class App(f: Expr, arg: Expr) extends Expr
  case class Pair(first: Expr, second: Expr) extends Expr
  case class First(pair: Expr) extends Expr
  case class Second(pair: Expr) extends Expr
  case object NilE extends Expr
  case class Cons(head: Expr, tail: Expr) extends Expr
  case class Head(list: Expr) extends Expr
  case class Tail(list: Expr) extends Expr

  sealed trait Value
  type Env = Map[String, Value]

  case class NumV(num: Int) extends Value
  case class PairV(first: Value, second: Value) extends Value
  case object NilV extends Value
  case class ConsV(head: Value, tail: Value) extends Value
  case class CloV(argName: String, expr: Expr, env: Env) extends Value
  case class ExprV(expr: Expr, env: Env, var cache: Option[Value]) extends Value

  def force(value: Value): Value = value match {
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
      case Pair(first, second) =>
        PairV(ExprV(first, env, None), ExprV(second, env, None))
      case First(pair) =>
        val Pair(first, _) = pair
        force(pret(first, env))
      case Second(pair) =>
        val Pair(_, second) = pair
        force(pret(second, env))
      case NilE => NilV
      case Cons(head, tail) =>
        ConsV(ExprV(head, env, None), pret(tail, env))
      case Head(list) =>
        val ConsV(headV, _): Value = force(pret(list, env))
        force(headV)
      case Tail(list) =>
        val ConsV(_, tailV): Value = force(pret(list, env))
        force(tailV) match {
          case NilV => NilV
          case ret@ConsV(_, _) => ret
          case _ => error("Not a list")
        }
    }
    println("ret: %s".format(ret))
    ret
  }
}

import myMain._

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
      def in(nextExpr: Expr): Expr = Val(id, init, nextExpr)
    }
  }
}

import sugar._

val lambda = (tup: (String, Expr)) => Fun(tup._1, tup._2)

def fun(argName: String)(expr: Expr): Expr =
  Fun(argName, expr)

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
ret: CloV(y,App(Id(y),Num(3)),Map())
expr: App(Id(y),Num(3))
expr: Id(y)
ret: ExprV(Fun(x,App(Num(1),Num(2))),Map(),None)
expr: Fun(x,App(Num(1),Num(2)))
ret: CloV(x,App(Num(1),Num(2)),Map())
expr: App(Num(1),Num(2))
expr: Num(1)
ret: NumV(1)
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
ret: CloV(x,Id(y),Map())
expr: Add(Num(3),Num(4))
expr: Num(3)
ret: NumV(3)
expr: Num(4)
ret: NumV(4)
ret: NumV(7)
ret: NumV(7)
val res1: myMain.Value = NumV(7)

Note that the wrong function f is not evaluated.
 */

try {
  pret0 {
    Pair(3.i, lambda("x" -> "x".id) + 4.i)
  }
} // no err throw

try {
  pret0 {
    Pair(3.i, lambda("x" -> "x".id) + 4.i)._1 + 5.i
  }
} // no err throw

try {
  pret0 {
    Pair(3.i, lambda("x" -> "x".id) + 4.i)._2 + 5.i
  }
} catch {
  case e: Exception => e
} // scala.MatchError incurs

try {
  pret0 {
    ((0.i + NilE) :: (2.i :: NilE)).getTail.getHead
  }
}

try {
  pret0 {
    ((0.i + NilE) :: (2.i :: NilE)).getHead
  }
} catch {
  case e: Exception => e
}

try {
  pret0 {
    (0.i :: 1.i).getHead
  }
}

try {
  pret0 {
    (0.i :: 1.i).getTail
  }
} catch {
  case e: Exception => e
} // NOT A LIST error occurs