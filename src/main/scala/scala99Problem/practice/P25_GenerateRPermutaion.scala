package scala99Problem.practice

import scala99Problem.practice.P24_Lotto.removeAt

/*
* P25 (*) Generate a random permutation of the elements of a list.
Hint: Use the solution of problem P23.

Example:

scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
* */
object P25_GenerateRPermutaion extends App {
  // This algorithm is O(n^2), but it makes up for that in simplicity of
  // implementation.

  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      println("rendom select start")
      println("lngth=>" + ls.length)
      println("n=>" + n)
      if (n <= 0) Nil
      else {
        val rndm = r.nextInt(ls.length)
        val (rest, e) = removeAt(rndm, ls)
        println("e=>" + e)
        e :: randomSelectR(n - 1, rest, r)
      }

    randomSelectR(n, ls, new util.Random)
  }

  def randomPermute1[A](ls: List[A]): List[A] = randomSelect(ls.length, ls)

  // The canonical way to shuffle imperatively is Fisher-Yates.  It requires a
  // mutable array.  This is O(n).
  def randomPermute(ls: List[Any]): List[Any] = {
    val rand = new util.Random
    val a = ls.toArray
    for (i <- a.length - 1 to 1 by -1) {
      val i1 = rand.nextInt(i + 1)
      val t = a(i)
      a.update(i, a(i1))
      a.update(i1, t)
    }
    a.toList
  }

  // Efficient purely functional algorithms for shuffling are a lot harder.  One
  // is described in http://okmij.org/ftp/Haskell/perfect-shuffle.txt using
  // Haskell. Implementing it in Scala is left as an exercise for the reader.
  println(randomPermute1(List('a', 'b', 'c', 'd', 'e', 'f')))
}
