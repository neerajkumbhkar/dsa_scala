package scala99Problem.practice
/*
* P06 (*) Find out whether a list is a palindrome.
Example:

scala> isPalindrome(List(1, 2, 3, 2, 1))
res0: Boolean = true
* */
object P06_ListPalindrom extends App {
  def listPalindrom[A](ls:List[A]):Boolean={
    ls match
      case Nil => throw new IllegalArgumentException()
      case _::Nil => true
      case x::xs => if (x==xs.last) listPalindrom(xs.init) else false
  }

  def listPalindrom1[A](ls:List[A]):Boolean={
    ls match
      case Nil => throw new IllegalArgumentException()
      case _ :: Nil => true
      case flr(f,l,r) => f==l && listPalindrom(r)
  }

  def listPalindrom2[A](res:Boolean,rem:List[A]):Boolean= rem match
    case Nil => res
    case _::Nil => res
    case flr(f,l,r) => listPalindrom2(res && f==l,r)

  object flr{
    def unapply[A](list: List[A]) = list match
      case Nil =>None
      case x::Nil => Some(x,x,Nil)
      case x::xs => Some(x,xs.last,xs.init)
  }
  print(listPalindrom2(true,List(1,2,3,2,1,2)))
}
