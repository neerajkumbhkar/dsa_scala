package scala99Problem.practice

/*
*P01 (*) Find the last element of a list.
Example:

scala> last(List(1, 1, 2, 3, 5, 8))
res0: Int = 8
*
* */
object P01_FindLastElem extends App {
  def findLastElem[A](ls:List[A]):A=ls.last

  def findLastElemRec[A](ls:List[A]):A=ls match
    case Nil => throw new NoSuchElementException("List is empty")
    case x::Nil => x
    case _::xs => findLastElem(xs)

  print(findLastElem(List(1)))
}
