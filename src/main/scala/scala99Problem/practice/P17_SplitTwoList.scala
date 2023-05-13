package scala99Problem.practice
/*
* P17 (*) Split a list into two parts.
The length of the first part is given. Use a Tuple for your result.

Example:

scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
* */
object P17_SplitTwoList extends App {
  def splitTwoList[A](n:Int,ls:List[A]):(List[A],List[A]) = {
    (ls.take(n),ls.takeRight(ls.length-n))
  }
  def splitTwoListRec[A](n1:Int,ls:List[A]):(List[A],List[A])={
    def _splitTwoListRec(n:Int, res:List[A],rem:List[A]):(List[A],List[A])=(n,rem) match
      case (_,Nil)=> throw new IllegalArgumentException()
      case (1,x::xs)=>(res:::List(x),xs)
      case (n,x::xs)=>_splitTwoListRec(n-1,res:::List(x),xs)
    _splitTwoListRec(n1,List(),ls)
  }

  print(splitTwoListRec(2, List('a', 'b', 'c', 'd', 'e', 'f')))
}
