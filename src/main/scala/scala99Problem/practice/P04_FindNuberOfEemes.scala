package scala99Problem.practice
/*
* P04 (*) Find the number of elements of a list.
Example:

scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
* */
object P04_FindNuberOfEemes extends App {
  def findLength[A](ls:List[A]):Int={
    def _findLength[A](count: Int, ls: List[A]): Int = ls match {
      case Nil => count
      case _ :: xs => _findLength(count + 1, xs)
    }
    _findLength(0, ls)
  }
  def findLength1[A](ls:List[A]):Int={
    ls.foldLeft(0){(c,_)=>c+1}
  }


  print(findLength1(List(1,2,3)))
}
