sealed trait Expr

case class FunDef(f: String, x: String, b: Expr)

case class Num(n: Int) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Val(x: String, i: Expr, b: Expr) extends Expr
case class Id(x: String) extends Expr
case class Call(f: String, a: Expr) extends Expr

type Env = Map[String, Int]
type FEnv = Map[String, FunDef]

def interpret(e: Expr, env: Env, fEnv: FEnv): Int = e match {
  case Num(n) => n
  case Add(l, r) => {
    interpret(l, env, fEnv) + interpret(r, env, fEnv)
  }
  case Sub(l, r) => {
    interpret(l, env, fEnv) - interpret(r, env, fEnv)
  }
  case Val(x, i, b) => {
    interpret(b, env + (x -> interpret(i, env, fEnv)), fEnv)
  }
  case Id(x) => env(x)
  case Call(f, a) => {
    val FunDef(_, x, e) = fEnv(f)
    interpret(e, Map(x -> interpret(a, env, fEnv)), fEnv)
  }
}

val myFEnv: FEnv = Map(
  "twice" -> FunDef("twice", "x", Add(Id("x"), Id("x"))),
  "x" -> FunDef("x", "y", Id("y")),
  "f" -> FunDef("f", "x", Add(Id("x"), Num(1))),
  "g" -> FunDef("g", "g", Id("g")),
)

val myEnv: Env = Map()

def myInterpret(e: Expr): Int = interpret(e, myEnv, myFEnv)

//myInterpret(Call("twice", Id("twice")))
// "twice" is not defined in myEnv
myInterpret(Val("x", Num(5), Call("x", Id("x"))))
myInterpret(Call("g", Num(3)))
//myInterpret(Call("g", Id("f")))
// "f" is not defined in myEnv
//myInterpret(Call("g", Id("g")))
// "g" is not defined in myEnv

