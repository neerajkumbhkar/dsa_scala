package scala99Problem.practice
/*
* P22 (*) Create a list containing all integers within a given range.
Example:

scala> range(4, 9)
res0: List[Int] = List(4, 5, 6, 7, 8, 9)
* */
object P22_CreateARange extends App {
  def createARangeOfInt(from: Int, to: Int): List[Int] = {
    def _createARangeOfInt(from: Int, to: Int, res: List[Int]): List[Int] = {
      if(from==to) res:::List(from) else _createARangeOfInt(from+1,to, res:::List(from))
    }
    _createARangeOfInt(from,to,List())
  }

  // Tail recursive.
  def rangeTailRecursive(start: Int, end: Int): List[Int] = {
    def rangeR(end: Int, result: List[Int]): List[Int] = {
      if (end < start) result
      else rangeR(end - 1, end :: result)
    }

    rangeR(end, Nil)
  }

  // The classic functional approach would be to use `unfoldr`, which Scala
  // doesn't have.  So we'll write one and then use it.
  def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] =
    f(s) match {
      case None => Nil
      case Some((r, n)) => r :: unfoldRight(n)(f)
    }

  def rangeFunctional(start: Int, end: Int): List[Int] =
    unfoldRight(start) { n =>
      if (n > end) None
      else Some((n , n + 1))
    }

  // Recursive.
  def rangeRecursive(start: Int, end: Int): List[Int] =
    if (end < start) Nil
    else start :: rangeRecursive(start + 1, end)
  print(rangeFunctional(4,9))
}
