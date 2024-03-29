package scala99Problem.practice
/*
* P15 (**) Duplicate the elements of a list a given number of times.
Example:

scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
* */
object P15_DupNTimes extends App {
  def dupNTimes[A](n:Int,ls:List[A]):List[A] = ls.flatMap(List.fill(n)(_))
  print(dupNTimes(3, List('a', 'b', 'c', 'c', 'd')))
}
