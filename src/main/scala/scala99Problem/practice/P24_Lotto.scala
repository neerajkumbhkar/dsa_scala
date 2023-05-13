package scala99Problem.practice

/*
* P24 (*) Lotto: Draw
�
N different random numbers from the set
1..
�
1..M.
Example:

scala> lotto(6, 49)
res0: List[Int] = List(23, 1, 17, 33, 21, 37)
* */
object P24_Lotto extends App {

  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
    println("r" + n)
    ls.splitAt(n) match
      case (Nil, _) if n < 0 => throw new NoSuchElementException
      case (pre, e :: post) => {
        println("pre=>" + pre)
        println("post=>" + post)
        println("e=>" + e)
        (pre ::: post, e)
      }
      case (pre, Nil) => throw new NoSuchElementException
  }

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

  def lotto(count: Int, max: Int): List[Int] = {
    println("List.range(1, max + 1))=> " + List.range(1, max + 1))
    randomSelect(count, List.range(1, max + 1))
  }

  print(lotto(6, 49))
}
