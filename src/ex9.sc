import scala.sys.error

sealed trait Expr

case class Num(n: Int) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Val(x: String, i: Expr, b: Expr) extends Expr
case class Id(x: String) extends Expr
case class Fun(x: String, b: Expr) extends Expr
case class App(f: Expr, a: Expr) extends Expr

sealed trait Value
type Env = Map[String, Value]

case class NumV(n: Int) extends Value
case class CloV(p: String, b: Expr, e: Env) extends Value

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
  case Val(x, i, b) => pret(b, env + (x -> pret(i, env)))
  case Id(x) => env(x)
  case Fun(x, b) => CloV(x, b, env)
  case App(f, a) =>
    val CloV(x, b, fEnv) = pret(f, env)
    pret(b, fEnv + (x -> pret(a, env)))
}

// ex9.1

/*
pret(val x=5 in val f=\y.y+x in (\g.f(g 1))(\x.x), env=0)
->
pret(val f=\y.y+x in (\g.f(g 1))(\x.x), env={x->5})
->
pret((\g.f(g 1))(\x.x), env={x->5, f->\y.y+5})
->
pret(f(g 1), env={x->5, f->\y.y+5, g->\x.x})
->
pret(y+5, env={x->5, f->\y.y+5, g->\x.x, y->(g 1)})
where
pret(g 1, env={x->5, f->\y.y+5, g->\x.x}) -> 1
->
we got: 5 + 1 = 6.
 */

val myEnv: Env = Map()

pret(Val("x", Num(5), Val("f", Fun("y", Add(Id("y"), Id("x"))),
  App(Fun("g", App(Id("f"), App(Id("g"), Num(1)))), Fun("x", Id("x"))))), myEnv)

//ex9.2

/*
1. We cannot use variables defined in f.
2. We cannot use variables defined outside of f.
 */

//ex9.4

def desugar(e: Expr): Expr = e match {
  case Num(n) => Num(n)
  case Id(x) => Id(x)
  case Add(l, r) => Add(desugar(l), desugar(r))
  case Sub(l, r) => Sub(desugar(l), desugar(r))
  case Val(x, e, b) => App(desugar(Fun(x, b)), desugar(e))
  case Fun(x, b) => Fun(x, desugar(b))
  case App(f, a) => App(desugar(f), desugar(a))
}

// try: val x=2 in (val y=4 in x+y)
{
  val sugared = Val("x", Num(2), Val("y", Num(4), Add(Id("x"), Id("y"))))
  desugar(sugared)
  // result: App(Fun(x,App(Fun(y,Add(Id(x),Id(y))),Num(4))),Num(2))
  // which means
  // \x.\y.x+y 4 2
}

//ex9.9

/*
sigma |- e1 => v1, sigma |- e2 => v2
------------------------------------
sigma |- (e1,e2) => (v1,v2)

sigma |- (e1,e2) => (v1,v2)
---------------------------
sigma |- (e1,e2).1 => v1
sigma |- (e1,e2).2 => v2

(8,(320,43).1).2 => (320,43).1 => 320
 */