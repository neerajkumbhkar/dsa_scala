package scala99Problem.practice
/*
* P21 (*) Insert an element at a given position into a list.
Example:

scala> insertAt('new, 1, List('a', 'b', 'c', 'd'))
res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
* */
object P21_InsertAElemAt extends App {
  def insertAElemAt[A](a:A,pos:Int,ls:List[A]):List[A] = {
    (pos,ls) match
      case (0,List()) => List(a)
      case (pos,ls) if(pos>ls.length || pos<0 ) => throw new IllegalArgumentException()
      case (0,ls) => List(a):::ls
      case (pos,x::xs) => List(x):::insertAElemAt(a,pos-1,xs)
  }
  print(insertAElemAt('n', 0, List('a')))
}
