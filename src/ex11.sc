sealed trait Expr

case class Num(num: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Id(id: String) extends Expr
case class Fun(varName: String, expr: Expr) extends Expr
case class App(f: Expr, arg: Expr) extends Expr
case class NewBox(expr: Expr) extends Expr
case class OpenBox(box: Expr) extends Expr
case class SetBox(box: Expr, expr: Expr) extends Expr
case class Seq(lhs: Expr, rhs: Expr) extends Expr

sealed trait Value

type Env = Map[String, Value]

type Addr = Int

type Storage = Map[Addr, Value]

case class NumV(num: Int) extends Value
case class CloV(varName: String, expr: Expr, var env: Env) extends Value
case class BoxV(id: Addr) extends Value
case class Err(errDoc: String) extends Value

def pret(expr: Expr, env: Env, storage: Storage): (Value, Storage) =
  expr match {
    // the storage does not change
    case Num(num) => (NumV(num), storage)
    case Id(id) => (env(id), storage)
    case Fun(varName, expr) => (CloV(varName, expr, env), storage)

    // does not directly change the storage, but can change it in recursive calls
    case Seq(lhs, rhs) =>
      val (_, sto1) = pret(lhs, env, storage)
      pret(rhs, env, sto1)
    case Add(lhs, rhs) =>
      val (lVal, sto1) = pret(lhs, env, storage)
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
      val (lVal, sto1) = pret(lhs, env, storage)
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
      val (cloV, sto1) = pret(f, env, storage)
      cloV match {
        case CloV(varName, expr, fEnv) =>
          val (argVal, sto2) = pret(arg, env, sto1)
          pret(expr, fEnv + (varName -> argVal), sto2)
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ => (Err("Arg1 not a function ### f=%s evaluated %s".format(f, cloV)), sto1)
      }

    // box op
    case NewBox(expr) =>
      val (value, sto1) = pret(expr, env, storage)
      val id = sto1.keys.maxOption match {
        case Some(n) => n + 1
        case None => 0
      }
      (BoxV(id), storage + (id -> value))
    case OpenBox(expr) =>
      val (box, sto1) = pret(expr, env, storage)
      box match {
        case BoxV(id) => (sto1(id), sto1)
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ => (Err("OpenBox stmt arg not a box ### box=%s evaluated %s".format(expr, box)), sto1)
      }
    case SetBox(box, expr) =>
      val (boxV, sto1) = pret(box, env, storage)
      boxV match {
        case BoxV(id) =>
          val (value, sto2) = pret(expr, env, sto1)
          (value, sto2 + (id -> value))
        case Err(errDoc) => (Err(errDoc), sto1)
        case _ => (Err("SetBox stmt arg not a box ### box=%s evaluated %s".format(expr, boxV)), sto1)
      }
  }