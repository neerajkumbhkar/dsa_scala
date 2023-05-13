package scala99Problem.practice
/*
* P16 (**) Drop every
�
Nth element from a list.
Example:

scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
* */
object P16_DropEveryNth extends App {
  def dropEveryNth[A](n:Int,ls:List[A]):List[A] = {
    ls.grouped(n).flatMap(_.take(n-1)).toList
  }

  def dropEveryNth1[A](n: Int, ls: List[A]): List[A] = {
    ls.zipWithIndex.filter(e=>(e._2+1)%n!=0).map(_._1)
  }

  def dropEveryNth2[A](n:Int,ls:List[A]):List[A] ={
    def _dropEveryNth2[A](n1: Int, res: List[A],rem: List[A]): List[A] = {
      if(n>1) {
        (n1, rem) match
          case (_, Nil) => res
          case (0, rem) => res ::: rem
          case (1, _ :: xs) => _dropEveryNth2(n, res, xs)
          case (n1, x :: xs) => _dropEveryNth2(n1 - 1, res ::: List(x), xs)
      } else {
        throw new IllegalArgumentException("n must be greater thn 1")
      }
    }
    _dropEveryNth2(n, List(), ls)
  }


  print(dropEveryNth2(4, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','@','#','$')))
}
