package scala99Problem.practice
/*
* P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

Example:

scala> pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
res0: List[List[Symbol]] = List(List('a', 'a', 'a', 'a'), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
* */
object P09_PackConDup extends App {
  def packConsecativeDuplicate[A](ls:List[A]):List[List[A]] = {
    def _packDuplicateSubList(res: List[List[A]], rem: List[A]): List[List[A]] = rem match
      case Nil => res
      case x :: xs => if (res.isEmpty || res.last.head != x) _packDuplicateSubList(res ::: List(List(x)), xs) else _packDuplicateSubList(res.init ::: List(res.last ::: List(x)), xs)
    _packDuplicateSubList(List(), ls)
  }

  def packConsecativeDuplicate1[A](ls: List[A]): List[List[A]] = {
    def _packDuplicateSubList(res: List[List[A]], rem: List[A]): List[List[A]] = rem match
      case Nil => res
      case _ => val (l,r) = rem.span(_== rem.head)
        _packDuplicateSubList(res:::List(l),r)

    _packDuplicateSubList(List(), ls)
  }

  print(packConsecativeDuplicate1(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
}
