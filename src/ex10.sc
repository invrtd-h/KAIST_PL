sealed trait Expr

case class Num(num: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Id(id: String) extends Expr
case class Fun(x: String, expr: Expr) extends Expr
case class App(f: Expr, arg: Expr) extends Expr
case class If0(e: Expr, eTrue: Expr, eFalse: Expr) extends Expr
case class Rec(f: String, x: String, defExpr: Expr, callExpr: Expr) extends Expr

sealed trait Value

type Env = Map[String, Value]

case class NumV(num: Int) extends Value
case class CloV(x: String, expr: Expr, var env: Env) extends Value
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
  case If0(e, eTrue, eFalse) =>
    val flag = pret(e, env)
    flag match {
      case NumV(num) => if (num == 0) pret(eTrue, env) else pret(eFalse, env)
      case Err(errDoc) => Err(errDoc)
      case _ => Err("Flag not a number ### flag=%s evaluated %s".format(e, flag))
    }
  case Rec(f, x, defExpr, callExpr) =>
    val cloV = CloV(x, defExpr, env)
    val newEnv = env + (f -> cloV)
    cloV.env = newEnv
    pret(e, newEnv)
}

