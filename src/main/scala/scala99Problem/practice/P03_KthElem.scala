package scala99Problem.practice
/*
* P03 (*) Find the Kth element of a list.
By convention, the first element in the list is element 0.

Example:

scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
* */
object P03_KthElem extends App {
  def findKthElem[A](k:Int,ls:List[A]):A = {
      (k, ls) match
        case (_, Nil) => throw new IllegalArgumentException("ill")
        case (0, x :: _) => x
        case (n, _ :: xs) =>  findKthElem(n - 1, xs)
    }

  def findKthElem1[A](k:Int,ls:List[A]):A={
    print(ls.zipWithIndex)
    ls.zipWithIndex.filter(_._2==k).head._1
  }

  def findKthElem2[A](k:Int,list: List[A]):A = {
    list.take(k+1).last
  }

  print(findKthElem2(2, List(1,2,3,4,5,6,7,8)))
}
