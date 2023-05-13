package scala99Problem.practice
/*
* P08 (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
* */
object elemConsDup extends App {
  def removeDup[A](ls:List[A]):List[A]= ls match
    case Nil=>Nil
    case _::Nil => ls
    case x::xs => if(x==xs.head) removeDup(xs) else x::removeDup(xs)

  def removeDup1[A](ls:List[A]):List[A] = ls.foldLeft(List[A]()){
      case (x, e) => if(x.isEmpty || x.last!=e) x:::List(e) else x
    }


  println(removeDup1(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
  println(removeDup1(List('a')))
  println(removeDup1(List()))
}
