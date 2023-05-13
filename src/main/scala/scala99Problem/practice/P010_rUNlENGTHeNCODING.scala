package scala99Problem.practice
/*
* P10 (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where
�
N is the number of duplicates of the element
�
E.

Example:

scala> encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
* */
object P010_rUNlENGTHeNCODING extends App {

  def runLengthEncoding[A](ls:List[A]):List[(Int,A)] ={
    def _runLengthEncoding[A](res:List[(Int, A)],rem: List[A]): List[(Int, A)] = rem match
      case Nil => res
      case x::Nil => List((1, x))
      case rem =>
        val (l:List[A], r:List[A]) = rem.span(_ == rem.head)
        _runLengthEncoding(res:::List((l.length,l.head)),r)
    _runLengthEncoding(List(),ls)
  }

  def runLengthEncoding1[A](ls:List[A]):List[(Int,A)] = {
    def _runLengthHelper[A](ls:List[A]):List[List[A]]={
      def _runLengthEncoding[A](res: List[List[A]], rem: List[A]): List[List[A]] = rem match
        case Nil => res
        case _ =>
          val (l: List[A], r: List[A]) = rem.span(_ == rem.head)
          _runLengthEncoding(res:::List(l), r)
      _runLengthEncoding(Nil, ls)
    }
    _runLengthHelper(ls).map(i=>(i.length,i.head))
  }


  print(runLengthEncoding1(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) )
}
