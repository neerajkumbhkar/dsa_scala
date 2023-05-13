object StringComparision extends App {
  def strComp(s: String): String = {
    def strCompHelper(res: List[Char], sr: List[Char]): List[Char] = {
      sr match
        case x :: Nil => res ::: List(x)
        case x :: xs => if (x == xs.head) strCompHelper(res, xs) else strCompHelper(res ::: List(x), xs)
    }
    strCompHelper(List(), s.toList).foldLeft("")(_ + _)
  }

  def strComp1(s: String): String = {
    def strCompHelper1(res: List[List[Char]], sr: List[Char]): List[List[Char]] = {
      sr match
        case Nil => res
        case _ =>
          val (l:List[Char],r:List[Char])=sr.span(_== sr.head)
          strCompHelper1(res:+l,r)
    }
  strCompHelper1(List(),s.toList).map(i=>i.head.toString+i.length).mkString
  }
  println(strComp1("aabbccddddf"))
}