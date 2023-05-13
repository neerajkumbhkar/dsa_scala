package scala99Problem.practice
/*
* P19 (**) Rotate a list
�
N places to the left.
Examples:

scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
* */
object P19_RotateAList extends App {
  def rotateAList1[A](n:Int,ls:List[A]):List[A]={
    val k: Int = n % ls.length
    val k1:Int = if(k<0) k+ls.length else k
    def _rotateAList[A](k:Int,res:List[A],rem:List[A]):List[A] = (k,rem) match
      case (0,rem) => rem:::res
      case (k,x::xs) => _rotateAList(k-1,res:::List(x),xs)
    _rotateAList(k1,List(),ls)
  }

  def rotateAList[A](rot: Int, ls: List[A]): List[A] = {
    val rotation = if (ls.isEmpty) 0 else rot % ls.length
    if (rotation < 0) rotateAList(ls.length + rot, ls)
    else ls.drop(rotation) ::: ls.take(rotation)
  }

  print(rotateAList1(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
}
