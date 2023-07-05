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
    def delay: Expr = Delay(this)
    def laz: Expr = Lazy(this)
    def force: Expr = Force(this)
  }

  case class Num(num: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr
  case class Val(id: String, init: Expr, nextExpr: Expr) extends Expr
  case class Id(id: String) extends Expr
  case class Fun(argName: String, expr: Expr) extends Expr
  case class App(f: Expr, arg: Expr) extends Expr
  case class Delay(expr: Expr) extends Expr
  case class Lazy(expr: Expr) extends Expr
  case class Force(expr: Expr) extends Expr

  sealed trait Value
  type Env = Map[String, Value]

  case class NumV(num: Int) extends Value
  case class CloV(argName: String, expr: Expr, env: Env) extends Value
  case class DelayV(expr: Expr, env: Env, var cache: Option[Value]) extends Value
  case class LazyV(expr: Expr, env: Env, var cache: Option[Value]) extends Value

  def force(value: Value): Value = value match {
    case DelayV(_, _, Some(cache)) => cache
    case delayV@DelayV(expr, env, _) =>
      val cache = pret(expr, env)
      delayV.cache = Some(cache)
      cache
    case LazyV(_, _, Some(cache)) => cache
    case lazyV@LazyV(expr, env, _) =>
      val cache = force(pret(expr, env))
      lazyV.cache = Some(cache)
      cache
    case _ => value
  }

  def pret(expr: Expr, env: Env): Value = {
    println("expr: %s".format(expr))
    val ret = expr match {
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
      case Delay(dExpr) => DelayV(dExpr, env, None)
      case Lazy(lExpr) => LazyV(lExpr, env, None)
      case Force(fExpr) => force(pret(fExpr, env))
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

// TODO write a test code