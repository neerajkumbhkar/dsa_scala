package scala99Problem.practice

import scala99Problem.practice.GetKPC.getKPC

object GetKPC extends App{
  //val m= Map('0'->";",'1'->"abc",'2'->"def",'3'->"ghi",'4'->"jkl",'5'->"mno",'6'->"pqrs",'7'->"tu",'8'->"vwx",'9'->"yz")
  val m= Map(0->";",1->"abc",2->"def",3->"ghi",4->"jkl",5->"mno",6->"pqrs",7->"tu",8->"vwx",9->"yz")
  val arr= Array(";","abc","def","ghi","jkl","mno","pqrs","tu","vwx","yz")
  def getKPC(s:String):List[String] = {
    s.toList match
      case Nil => List("")
      case x::xs =>
        val code = arr(x-'0')
        val res = for{
          char<-code
          rc <- getKPC(xs.mkString)
        } yield char+rc
        res.toList
  }
  println(getKPC("123"))
}

