sealed trait Expr

case class Num(num: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class If0(condition: Expr, exprTrue: Expr, exprFalse: Expr) extends Expr
case class Id(id: String) extends Expr
case class Var(varName: String, init: Expr, nextExpr: Expr) extends Expr
case class Fun(argName: String, expr: Expr) extends Expr
case class App(f: Expr, arg: Expr) extends Expr
case class SetVar(varName: String, expr: Expr) extends Expr
case class Seq(lhs: Expr, rhs: Expr) extends Expr
case class Deref(ptr: Expr) extends Expr
case class Ref(id: String) extends Expr
case class Assign(ptr: Expr, assignExpr: Expr) extends Expr

sealed trait Value

type Addr = Int
type Env = Map[String, Addr]
type Store = Map[Addr, Value]

case class NumV(num: Int) extends Value
case class CloV(varName: String, expr: Expr, env: Env) extends Value
case class PtrV(addr: Addr) extends Value
case class Err(errDoc: String) extends Value

def pret(expr: Expr, env: Env, store: Store): (Value, Store) =
  expr match {
    case Num(num) => (NumV(num), store)
    case Id(id) => (store(env(id)), store)
    case Fun(argName, expr) => (CloV(argName, expr, env), store)
    case Var(varName, init, nextExpr) =>
      val (initVal, sto1) = pret(init, env, store)
      initVal match {
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ =>
          val addr = sto1.keys.maxOption.getOrElse(0) + 1
          pret(nextExpr, env + (varName -> addr), sto1 + (addr -> initVal))
      }

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
    case If0(condition, exprTrue, exprFalse) =>
      val (condVal, sto1) = pret(condition, env, store)
      condVal match {
        case NumV(n) =>
          if (n == 0) pret(exprTrue, env, sto1) else pret(exprFalse, env, sto1)
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ => (Err("If stmt condition not number ### cond=%s evaluated %s".format(condition, condVal)), sto1)
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
    case Seq(lhs, rhs) =>
      val (lhsVal, sto1) = pret(lhs, env, store)
      lhsVal match {
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ => pret(rhs, env, sto1)
      }

    case Deref(ptr) =>
      val (ptrVal, sto1) = pret(ptr, env, store)
      ptrVal match {
        case PtrV(addr) =>
          sto1.get(addr) match {
            case Some(value) => (value, sto1)
            case None => (Err("Invalid ptr address ### ptrVal=%d, store=%s".format(addr, sto1)), sto1)
          }
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ => (Err("Deref expr not a pointer ### expr=%s evaluated %s".format(ptr, ptrVal)), sto1)
      }
    case Ref(id) =>
      env.get(id) match {
        case Some(value) => (PtrV(value), store)
        case None => (Err("Undefined name"), store)
      }
    case Assign(ptr, assignExpr) =>
      val (assignVal, sto1) = pret(assignExpr, env, store)
      assignVal match {
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ =>
          val (ptrVal, sto2) = pret(ptr, env, sto1)
          ptrVal match {
            case PtrV(addr) =>
              sto2.get(addr) match {
                case Some(_) => (assignVal, sto2 + (addr -> assignVal))
                case None => (Err("Deref expr not a pointer ### expr=%s evaluated %s".format(ptr, ptrVal)), sto2)
              }
            case Err(errDoc) => (Err(errDoc), sto1)
            case _ => (Err("Deref expr not a pointer ### expr=%s evaluated %s".format(ptr, ptrVal)), sto1)
          }
      }
  }

// 12.1
val myEnv: Env = Map()
val myStore: Store = Map()

{
  val myVal = App(Fun("x", Id("x")), App(Fun("x", Id("x")), Num(1)))
  pret(myVal, myEnv, myStore)
}

{
  val myVal = Var("x", Num(1), Id("x"))
  pret(myVal, myEnv, myStore)
}

{
  val myVal = Var("x", Num(1), SetVar("x", Num(2)))
  pret(myVal, myEnv, myStore)
}

{
  val myVal = Var("f", Fun("x", If0(Id("x"), Num(0), Add(App(Id("f"), Sub(Id("x"), Num(1))), Id("x")))), App(Id("f"), Num(10)))
  pret(myVal, myEnv, myStore)
}

{
  val myVal = Var("x", Num(1), Ref("x"))
  pret(myVal, myEnv, myStore)
}

{
  val myVal = Var("x", Num(1), Deref(Ref("x")))
  pret(myVal, myEnv, myStore)
}