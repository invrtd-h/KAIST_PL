def f(x: Int): Int = {
  x
}

def g(h: Int => Int): Int = {
  h(0)
}

g(f)

((x: Int) => x)(0)

((x: Int) => {
  val y = x * x
  y
})(7)

val g0 = (h: Int => Int) => {
  h(0)
}
g0(_ + 1)

val g1 = (x: Int) => x

val sum = (x: Int, y: Int) => {
  x + y
}
val add_5 = sum(_, 5)
add_5(10)

val myList = List(0, 1, 2, 3, 4, 5)

def inc1(l: List[Int]): List[Int] = {
  l.map(_ + 1)
}
inc1(myList)

val square = (l: List[Int]) => {
  l.map(h => h * h)
}
square(myList)


