sealed trait Expr

case class Num(num: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Id(id: String) extends Expr
case class Fun(argName: String, expr: Expr) extends Expr
case class App(f: Expr, arg: Expr) extends Expr
case class SetVar(varName: String, expr: Expr) extends Expr
case class Seq(lhs: Expr, rhs: Expr) extends Expr

sealed trait Value

type Addr = Int
type Env = Map[String, Addr]
type Store = Map[Addr, Value]

case class NumV(num: Int) extends Value
case class CloV(varName: String, expr: Expr, var env: Env) extends Value
case class Err(errDoc: String) extends Value

def pret(expr: Expr, env: Env, store: Store): (Value, Store) =
  expr match {
    case Num(num) => (NumV(num), store)
    case Id(id) => (store(env(id)), store)
    case Fun(argName, expr) => (CloV(argName, expr, env), store)

    case Add(lhs, rhs) =>
      val (lVal, sto1) = pret(lhs, env, store)
      lVal match {
        case NumV(n) =>
          val (rVal, sto2) = pret(rhs, env, sto1)
          rVal match {
            case NumV(m) => (NumV(n + m), sto2)
            case Err(errDoc) => (Err(errDoc), sto2)
            case _ => (Err("Add arg not a number ### rhs=%s evaluated %s".format(rhs, rVal)), sto2)
          }
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ => (Err("Add arg not a number ### lhs=%s evaluated %s".format(lhs, lVal)), sto1)
      }
    case Sub(lhs, rhs) =>
      val (lVal, sto1) = pret(lhs, env, store)
      lVal match {
        case NumV(n) =>
          val (rVal, sto2) = pret(rhs, env, sto1)
          rVal match {
            case NumV(m) => (NumV(n - m), sto2)
            case Err(errDoc) => (Err(errDoc), sto2)
            case _ => (Err("Sub arg not a number ### rhs=%s evaluated %s".format(rhs, rVal)), sto2)
          }
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ => (Err("Sub arg not a number ### lhs=%s evaluated %s".format(lhs, lVal)), sto1)
      }
    case App(f, arg) =>
      val (fVal, sto1) = pret(f, env, store)
      fVal match {
        case CloV(varName, expr, fEnv) =>
          arg match {
            // call by reference
            case Id(id) =>
              env.get(id) match {
                case Some(addr) => pret(expr, fEnv + (varName -> addr), sto1)
                case None => (Err("id not defined in env ### id=%s env=%s".format(id, env)), sto1)
              }
            // call by value
            case _ =>
              val (argVal, sto2) = pret(arg, env, sto1)
              argVal match {
                case Err(errDoc) => (Err(errDoc), sto2)
                case _ =>
                  val addr = sto2.keys.maxOption.getOrElse(0) + 1
                  pret(expr, env + (varName -> addr), sto2 + (addr -> argVal))
              }
          }
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ => (Err("Arg1 not a function ### f=%s evaluated %s".format(f, fVal)), sto1)
      }
    case SetVar(varName, expr) =>
      val (exprVal, sto1) = pret(expr, env, store)
      exprVal match {
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ =>
          env.get(varName) match {
            case Some(addr) => (exprVal, sto1 + (addr -> exprVal))
            case None => (Err("variable name not defined ### varName=%s not in env=%s".format(varName, env)), sto1)
          }
      }
  }