import scala.annotation.tailrec

/**
 * Ex 3.1
 */
case class Student(name: String, height: Int)

def names(l: List[Student]): List[String] = l match {
  case Nil => Nil
  case student :: others => student.name :: names(others)
}

def rev(l: List[Student]): List[Student] = {
  @tailrec
  def aux(l: List[Student], ret: List[Student]): List[Student] = l match {
    case Nil => ret
    case h :: t => aux(t, h :: ret)
  }
  aux(l, Nil)
}

def names2(l: List[Student]): List[String] = {
  @tailrec
  def aux(l: List[Student], ret: List[String]): List[String] = l match {
    case Nil => ret
    case head :: tails => aux(tails, head.name :: ret)
  }
  aux(rev(l), Nil)
}

val students = List(
  Student("Who", 170),
  Student("What", 155),
  Student("Why", 190),
)
names(students)
names2(students)

/**
 * Ex 3.2
 */

def tall(l: List[Student]): List[Student] = {
  @tailrec
  def aux(l: List[Student], ret: List[Student]): List[Student] = l match {
    case Nil => ret
    case head :: tails => if (head.height >= 170) {
      aux(tails, head :: ret)
    } else {
      aux(tails, ret)
    }
  }
  aux(rev(l), Nil)
}

tall(students)

/**
 * Ex 3.3
 */

def length(l: List[Int]): Int = {
  @tailrec
  def aux(l: List[Int], ret: Int): Int = l match {
    case Nil => ret
    case _ :: tails => aux(tails, ret + 1)
  }
  aux(l, 0)
}

val ints = List(1, 2, 3, 4, 10, 20, 30, 40)
length(ints)
ints.length

/**
 * Ex 3.4
 */

def append(l: List[Int], n: Int): List[Int] = l match {
  case Nil => n :: Nil
  case head :: tails => head :: append(tails, n)
}

append(ints, 9)
ints.appended(9)