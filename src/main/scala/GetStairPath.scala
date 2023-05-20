object GetStairPath extends App {
  def getStairPath(n: Int): List[String] = {
    n match
      case n if n < 0 => List()
      case n if n == 0 => List("")
      case n =>
       val paths = for {
          i <- 1 to n
          p <- getStairPath(n - i).map(i.toString+_)
        } yield p
       paths.toList
  }

  def getStairPath1(n: Int): List[String] = {
    n match
      case n if n < 0 => List()
      case n if n == 0 => List("")
      case n => getStairPath1(n - 1).map("1"+ _):::getStairPath1(n - 1).map("2"+ _):::getStairPath1(n - 3).map("3" + _)
  }

println(getStairPath(4))
}
