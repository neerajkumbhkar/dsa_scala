package scala99Problem.practice
/*
* P18 (**) Extract a slice from a list.
Given two indices,
�
I and
�
K, the slice is the list containing the elements from and including the
�
Ith element up to but not including the
�
Kth element of the original list. Start counting the elements with 0.

Example:

scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g)
* */
object P18_ExtractASlice extends App {
  def listSlice[A](from:Int,to:Int,ls:List[A]):List[A]={
    ls.take(to).drop(from)
  }
  def listSlice1[A](from:Int,to:Int,ls:List[A]):List[A]={
    def _listSlice1[A](from: Int, to: Int,rem: List[A]): List[A] = (from,to,rem) match
      case (0,0,rem) => rem
      case (0,to,rem) => _listSlice1(0,to-1,rem.init)
      case (from,to,rem) => _listSlice1(from-1,to,rem.tail)
    _listSlice1(from,to-from,ls)
  }


  print(listSlice1(3,7,List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
}
