package scala99Problem.practice
/*
* P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
*  Only elements with duplicates are transferred as (N, E) terms.

Example:

scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
* */
object P011_ModifiedRunLengthEncoding extends App {
  def modifiedRunLengthEncoding[A](ls:List[A]):List[Any] = {
     def _runLEncode(res:List[Any],rem:List[A]):List[Any]=rem match
       case Nil => res
       case _ => {
         val (l,r) = rem.span(_==rem.head)
         _runLEncode(if(l.length<=1) res:::List(l.head) else res:::List((l.length,l.head)),r)
       }
       _runLEncode(List(),ls)
  }
  print(modifiedRunLengthEncoding(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
}
