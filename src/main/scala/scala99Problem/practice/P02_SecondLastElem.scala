package scala99Problem.practice
/*
*P02 (*) Find the last but one element of a list.
Example:

scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
* */
object P02_SecondLastElem extends App {
  def findSecondLastElem[A](ls:List[A]):A= {
    if(ls.isEmpty) {
      throw new IllegalArgumentException()
    } else if(!ls.isEmpty && ls.length==1) {
      ls.last
    } else {
      ls.init.last
    }
  }

  def findSecondLastElemRec[A](ls:List[A]):A = ls match
    case Nil => throw new NoSuchElementException("List is empty")
    case x::Nil => x
    case x::_::Nil => x
    case _::xs => findSecondLastElem(xs)

  print(findSecondLastElem(List(-1)))
}
