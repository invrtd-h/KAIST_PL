/*
Implementation of FAE with error values.
 */

sealed trait Expr

case class Num(num: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Id(id: String) extends Expr
case class Fun(x: String, expr: Expr) extends Expr
case class App(f: Expr, arg: Expr) extends Expr

sealed trait Value

type Env = Map[String, Value]

case class NumV(num: Int) extends Value
case class CloV(x: String, expr: Expr, env: Env) extends Value
case class Err(errDoc: String) extends Value

def pret(e: Expr, env: Env): Value = e match {
  case Num(num) => NumV(num)
  case Add(lhs, rhs) =>
    val lhsVal = pret(lhs, env)
    lhsVal match {
    case NumV(lhsNum) =>
      val rhsVal = pret(rhs, env)
      rhsVal match {
      case NumV(rhsNum) => NumV(lhsNum + rhsNum)
      case Err(errDoc) => Err(errDoc)
      case _ => Err("Add arg not a number ### rhs=%s evaluated %s".format(rhs, rhsVal))
    }
    case Err(errDoc) => Err(errDoc)
    case _ => Err("Add arg not a number ### lhs=%s evaluated %s".format(lhs, lhsVal))
  }
  case Sub(lhs, rhs) =>
    val lhsVal = pret(lhs, env)
    lhsVal match {
      case NumV(lhsNum) =>
        val rhsVal = pret(rhs, env)
        rhsVal match {
          case NumV(rhsNum) => NumV(lhsNum + rhsNum)
          case Err(errDoc) => Err(errDoc)
          case _ => Err("Sub arg not a number ### rhs=%s evaluated %s".format(rhs, rhsVal))
        }
      case Err(errDoc) => Err(errDoc)
      case _ => Err("Sub arg not a number ### lhs=%s evaluated %s".format(lhs, lhsVal))
    }
  case Id(id) =>
    if (env.contains(id))
      env(id)
    else
      Err("id %s definition not found".format(id))
  case Fun(x, expr) => CloV(x, expr, env)
  case App(f, arg) =>
    val fVal = pret(f, env)
    fVal match {
      case CloV(x, expr, env) => pret(expr, env + (x -> pret(arg, env)))
      case _ => Err("First arg not a function ### fun=%s evaluated %s".format(f, fVal))
    }
}

val myEnv: Env = Map()

{
  val myErr: Expr = App(Num(1), Num(2))
  pret(myErr, myEnv)
}

{
  val myErr: Expr = App(Fun("x", Add(Id("x"), Id("y"))), Num(1))
  pret(myErr, myEnv)
}

{
  val myErr: Expr = Add(Num(1), Id("x"))
  pret(myErr, myEnv)
}

{
  val myErr: Expr = Add(Num(1), Fun("x", Id("x")))
  pret(myErr, myEnv)
}