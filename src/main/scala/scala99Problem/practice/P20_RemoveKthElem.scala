package scala99Problem.practice
/*
* P20 (*) Remove the
�
Kth element from a list.
Return the list and the removed element in a Tuple. Elements are numbered from 0.

Example:

scala> removeAt(1, List('a, 'b, 'c, 'd))
res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
* */
object P20_RemoveKthElem extends App {
  def removeKthElem1[A](n:Int,ls:List[A]):(List[A],A)={
    def _removeKthElem1[A](n: Int, res:List[A], rem: List[A]): (List[A], A) = (n, rem) match
      case (_, Nil) => throw new IllegalArgumentException()
      case (0, x :: xs) => (res:::xs, x)
      case (i, x :: xs) => _removeKthElem1(i - 1,res:::List(x), xs)
    _removeKthElem1(n,List(),ls)
  }

  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
    def _removeAt[A](n: Int, res: List[A], rem: List[A]): (List[A], A) = {
      (n, res, rem) match
        case (0, res, h :: t) => if (n == 0) (res ::: t, h) else (res ::: List(h) ::: t.tail, t.head)
        case (n, res, h :: t) => _removeAt(n - 1, res ::: List(h), t)
    }

    _removeAt(n, List(), ls)
  }

  def removeAt1[A](n: Int, ls: List[A]): (List[A], A) = {
    if (ls.length <= n) throw new NoSuchElementException()
    (ls.take(n) ::: ls.drop(n).tail, ls(n))
  }

  def removeAt2[A](n: Int, ls: List[A]): (List[A], A) = {
    (ls.take(n) ::: ls.takeRight(ls.length - n).tail, ls(n))
  }

  def removeAt3[A](n: Int, ls: List[A]): (List[A], A) = {
    print(ls.splitAt(n))
    (ls.zipWithIndex.filter(_._2 != n).map(_._1), ls(n))
  }

  def removeAt4[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post) => (pre ::: post, e)
    case (_, Nil) => throw new NoSuchElementException
  }

  def removeAt5[A](n: Int, ls: List[A]): (List[A], A) =
    if (n < 0) throw new NoSuchElementException
    else (n, ls) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (t, e) = removeAt(n - 1, ls.tail)
        (ls.head :: t, e)
      }
    }

print(removeKthElem1(1, List('a', 'b', 'c', 'd')))
}
