/*
Implementation of FAE with pairs.
 */

import scala.sys.error

sealed trait Expr

case class Num(num: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Id(id: String) extends Expr
case class Fun(x: String, expr: Expr) extends Expr
case class App(f: Expr, arg: Expr) extends Expr
case class Pair(first: Expr, second: Expr) extends Expr

case class Acc1(pair: Expr) extends Expr
case class Acc2(pair: Expr) extends Expr

sealed trait Value

type Env = Map[String, Value]

case class NumV(n: Int) extends Value
case class CloV(x: String, expr: Expr, env: Env) extends Value
case class PairV(first: Value, second: Value) extends Value

def pret(e: Expr, env: Env): Value = e match {
  case Num(n) => NumV(n)
  case Add(l, r) => pret(l, env) match {
    case NumV(n) => pret(r, env) match {
      case NumV(m) => NumV(n + m)
      case _ => error("not an integer")
    }
    case _ => error("not an integer")
  }
  case Sub(l, r) => pret(l, env) match {
    case NumV(n) => pret(r, env) match {
      case NumV(m) => NumV(n - m)
      case _ => error("not an integer")
    }
    case _ => error("not an integer")
  }
  case Id(x) => env(x)
  case Fun(x, b) => CloV(x, b, env)
  case App(f, a) => pret(f, env) match {
    case CloV(x, b, fEnv) => pret(b, fEnv + (x -> pret(a, env)))
    case _ => error("f not a function")
  }
  case Pair(first, second) => PairV(pret(first, env), pret(second, env))
  case Acc1(pair) => pret(pair, env) match {
    case PairV(first, _) => first
    case _ => error("not a pair")
  }
  case Acc2(pair) => pret(pair, env) match {
    case PairV(_, second) => second
    case _ => error("not a pair")
  }
}

{
  val myEnv: Env = Map()
  val myPair = Pair(Num(8), Pair(Num(320), Num(42)))
  pret(myPair, myEnv)

  Acc1(Acc2(myPair))
  pret(Acc1(Acc2(myPair)), myEnv)
}