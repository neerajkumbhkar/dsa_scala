package scala99Problem.practice

object GetMazePath extends App {
  def geMazPaths(sr:Int,sc:Int,dr:Int,dc:Int):List[String]={
    (sr,sc,dr,dc) match
      case (sr,sc,dr,dc) if sr==dr && sc==dc => List("")
      case (sr,sc,dr,dc) if sc>dc || sr>dr => List()
      case (sr,sc,dr,dc) => geMazPaths(sr,sc+1,dr,dc).map("h"+_):::geMazPaths(sr+1,sc,dr,dc).map("v"+_)
  }
println(geMazPaths(0,0,2,2))
}
