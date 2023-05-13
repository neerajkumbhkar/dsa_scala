package scala99Problem.practice
/*
* P14 (*) Duplicate the elements of a list.
Example:

scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
* */
object P14_DuplicateEveryElem extends App {
  def dup[A](ls:List[A]):List[A]= ls.flatMap {i=>List.fill(2)(i)}
  def dup1[A](ls:List[A]):List[A]= ls.flatMap {i=>List(i,i)}
  print(dup1(List('a','b','c','c','d')))
}
