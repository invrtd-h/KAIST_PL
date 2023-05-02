import scala.annotation.tailrec

sealed trait AE
case class Num(value: Int) extends AE
case class Add(left: AE, right: AE) extends AE
case class Sub(left: AE, right: AE) extends AE

val n = Num(10)
val m = Num(5)
val e1 = Add(n, m)
val e2 = Sub(e1, Num(3))

def eval(e: AE): Int = e match {
  case Num(n) => n
  case Add(l, r) => eval(l) + eval(r)
  case Sub(l, r) => eval(l) - eval(r)
}

eval(e1)
eval(e2)

def grade(score: Int): String = {
  score / 10 match {
    case 10 | 9 => "A"
    case 8 => "B"
    case 7 => "C"
    case 6 => "D"
    case _ => "F"
  }
}

grade(85)
grade(50)

// Nested Patterns

def optimizeExpr(e: AE): AE = e match {
  case Num(_) => e
  case Add(Num(0), r) => optimizeExpr(r)
  case Add(l, Num(0)) => optimizeExpr(l)
  case Add(l, r) => Add(optimizeExpr(l), optimizeExpr(r))
  case Sub(l, Num(0)) => optimizeExpr(l)
  case Sub(l, r) => Sub(optimizeExpr(l), optimizeExpr(r))
}

val eNonOptimized = Add(Num(0), Sub(Sub(Num(0), Add(Num(2), Num(0))), Num(0)))
optimizeExpr(eNonOptimized)

// Patterns with Binders

case class Abs(ae: AE) extends AE

def optimizeAbs(e: AE): AE = e match {
  case Num(_) => e
  case Add(l, r) => Add(optimizeAbs(l), optimizeAbs(r))
  case Sub(l, r) => Sub(optimizeAbs(l), optimizeAbs(r))
  case Abs(e0 @ Abs(_)) => optimizeAbs(e0)
  case Abs(e0) => Abs(optimizeAbs(e0))
}

val eNonOptimized = Abs(Abs(Abs(Num(8))))
optimizeAbs(eNonOptimized)

// Tuple Patterns

@tailrec
def equal(ll: List[Int], lr: List[Int]): Boolean = {
  (ll, lr) match {
    case (hl :: tl, hr :: tr) => {
      hl == hr && equal(tl, tr)
    }
    case (Nil, Nil) => true
    case _ => false
  }
}

// Pattern Guards
sealed trait BST
case object EMPTY_BST extends BST
case class Node(root: Int, l: BST, r: BST) extends BST

def add(t: BST, n: Int): BST = {
  t match {
    case EMPTY_BST => Node(n, EMPTY_BST, EMPTY_BST)
    case Node(m, t0, t1) if n < m =>
      Node(m, add(t0, n), t1)
    case Node(m, t0, t1) if n > m =>
      Node(m, t0, add(t1, n))
    case _ => t
  }
}

// Pattern Matching in Anonymous Functions

def toSum(l: List[(Int, Int)]): List[Int] = {
  l.map {
    case (n, m) => n + m
  }
}

toSum(List((1, 2), (3, 4)))