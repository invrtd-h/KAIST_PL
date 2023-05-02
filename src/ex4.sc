val myList = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

/**
 * ex4.1
 */

def incBy(l: List[Int], n: Int): List[Int] = {
  l.map(_ + n)
}

incBy(myList, 3)
incBy(myList, 15)

/**
 * ex4.2
 */

def gt(l: List[Int], n: Int): List[Int] = {
  l.filter(_ > n)
}

gt(myList, 7)

/**
 * ex4.3
 */

def append(l: List[Int], n: Int): List[Int] = {
  l.foldRight(List(n))((h: Int, t: List[Int]) => {h :: t})
}

append(myList, 101)

/**
 * ex4.4
 */

def reverse(l: List[Int]): List[Int] = {
  l.foldLeft(List[Int]())((h: List[Int], t: Int) => {t :: h})
}

reverse(myList)