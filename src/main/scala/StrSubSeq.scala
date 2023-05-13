object StrSubSeq extends App {
  def strSubSeq(str:String):List[String] = {
    def _strSubSeq(l:String,ls: List[Char]): List[String]= {
      ls match
        case Nil => List():+l
        case arr =>
          val faith = _strSubSeq(l,arr.tail)
          println(faith)
          faith.map(ch=>arr.head.toString+ch)
    }
    _strSubSeq("",str.toList)
  }
println(strSubSeq("abc"))
}
