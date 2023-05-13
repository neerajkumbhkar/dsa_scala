package scala99Problem.practice
/*
* P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.

Example:

scala> decode(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
* */
object P012_DecodeRunLength extends  App {
  def decodeRunLength[A](ls:List[(Int,A)]) = ls.flatMap(i=>List.fill(i._1)(i._2))
  print(decodeRunLength((List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')))))
} 
