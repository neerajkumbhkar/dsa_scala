package scala99Problem.practice

object P05_reverseAList extends App {
  def reverse[A](l:List[A]):List[A]=l.reverse

  def reverseRec[A](l:List[A]):List[A]=l match
    case Nil => Nil
    case x::Nil => List(x)
    case x::xs => reverse(xs):::List(x)

  def reverseTailRec[A](l: List[A]): List[A] = {
    def _reversTailRec(res:List[A],rem:List[A]):List[A]={
      rem match
        case Nil => res
        case x :: xs => _reversTailRec(List(x):::res,xs)
    }
    _reversTailRec(List(),l)
  }

  def reverseAList[A](ls:List[A]):List[A]={
    ls.foldLeft(List[A]())((h,t)=>t::h)
  }

  print(reverseAList(List(1,2,3)))
}
