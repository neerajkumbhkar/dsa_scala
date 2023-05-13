import scala.reflect.ClassTag
/*
* The State Of Wakanda - 1
The historic state of Wakanda has various monuments and souvenirs which are visited by many travelers every day. The guides follow a prescribed route of visiting the monuments which improve them understand the relevance of each monument.

The route of the monument is fixed and expressed in a 2-d matrix where the travelers visit the prescribed next monument. For example

1  2  3
4  5  6
7  8  9

is the prescribed route and the visitors travels this path: 1->2->3->4->5->6->7->8->9

However, a certain visitor decides to travel a different path as follows:
1. He first travels southwards till no further south places are available.
2. He then moves only 1 place eastwards.
3. He starts to move again towards north till any further north moves are available.
This continues till all the places are covered.

For example, the monuments are named as follows:
1  2  3
4  5  6
7  8  9

Path followed by traveler: 1->4->7->8->5->2->3->6->9

You are required to print the path that this traveler follows to visit all places.
* */
object Wave2D extends App {
  
  //def wave2D[A: ClassTag](arr: Array[Array[A]]): Array[Array[A]] = {
  def _wave2D[A: ClassTag](res: Array[Array[A]]): Unit = {
    var c = 0
    while (c<res(0).length) {
      if (c%2==0) {
        for(i<-res.indices){
          print(res(i)(c))
        }
      } else {
        for(j<-res.indices.reverse) {
          print(res(j)(c))
        }
      }
      c=c+1
    }
    println()
  }

  def wave2D[A: ClassTag](res: Array[Array[A]]): Unit = {
    res.zipWithIndex.map { i =>
      i._1.zipWithIndex.map{j=>
        if (i._2 % 2 == 0) {
          for (i1 <- i._1.indices) {
            print(res(j._2)(i1))
          }
        } else {
          for (j1 <- (0 until  i._1.length)) {
            print(res(j1)(i._2))
          }
        }
        println()
      }
    }
  }

  def wave2DTrans(matrix:Array[Array[Int]]):List[Int] = {
    def _wave2DTrans(res:List[Int],ls:List[List[Int]]):List[Int] = {
      ls.transpose match
        case Nil => res
        //case x::Nil => x
        case x::xs => (x:::res):::_wave2DTrans(res,xs.transpose.reverse)
    }
    _wave2DTrans(List(),matrix.toList.map(_.toList))
  }

  val a = Array.ofDim[Int](4, 3)
  a(0)(0) = 1
  a(0)(1) = 2
  a(0)(2) = 3
  a(1)(0) = 4
  a(1)(1) = 5
  a(1)(2) = 6
  a(2)(0) = 7
  a(2)(1) = 8
  a(2)(2) = 9
  a(3)(0) = 10
  a(3)(1) = 11
  a(3)(2) = 12

  /*
  * 1 2 3
  * 4 5 6
  * 7 8 9
  * 10 11 12
  * */
  //a.foreach{i=>i.foreach(print(_))}
  print(wave2DTrans(a))
}
