package scala99Problem.practice
/*
* /*
 The problem
 P07 (**) Flatten a nested list structure.

 Example:

 scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
 res0: List[Any] = List(1, 1, 2, 3, 5, 8)

 */
* */
object P07_FlattenList extends App {
  def flattenLs[A](ls:List[Any]):List[A] = ls match
    case Nil => Nil
    case (xs1:List[A])::(xs2:List[A]) => flattenLs(xs1):::flattenLs(xs2)
    case (x:A)::(xs:List[A]) => x::flattenLs(xs)

  def flattertNastedList[A](list: List[A]): List[A] = list match
    case Nil => Nil
    case (xs: List[A]) :: (xs1: List[A]) => flattertNastedList(xs) ::: flattertNastedList(xs1)
    case (x: A) :: (xs: List[A]) => x :: flattertNastedList(xs)

  def flattertNastedList1(list: List[Any]): List[Any] = list match
    case Nil => Nil
    case (xs: List[_]) :: tail => flattertNastedList(xs) ::: flattertNastedList(tail)
    case (x: Any) :: tail => x :: flattertNastedList(tail)

  def flatternNastedList2(ls: List[Any]): List[Any] = {
    def _flatten(res: List[Any], rem: List[Any]): List[Any] = rem match {
      case Nil => res
      case (h: List[Any]) :: Nil => _flatten(res, h)
      case (h: List[Any]) :: tail => _flatten(res ::: h, tail)
      case h :: tail => _flatten(res ::: List(h), tail)
    }

    _flatten(List(), ls)
  }

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  print(flattenLs(List(List(1, 1), 2, List(3, List(5, 8)))))
}
