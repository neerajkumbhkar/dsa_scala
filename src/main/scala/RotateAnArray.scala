import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object RotateAnArray {
  def main(args: Array[String]): Unit = {
      val sc = new Scanner(System.in)
      val n = sc.nextInt()
      val arr = ArrayBuffer[Int]()
        for(e<-0 until n){
          arr+=sc.nextInt()
        }
      val k = sc.nextInt()
      rotate(arr,k).foreach(print(_))
  }
  def rotate(arr:ArrayBuffer[Int],k:Int):ArrayBuffer[Int]={
     val n = arr.length
     var rot = k%n
     rot = if (rot<0) rot+n else rot
     reverse(arr,0,n-rot-1)
     reverse(arr,n-rot,n-1)
     reverse(arr,0,n-1)
     arr
  }
  def reverse(ints: ArrayBuffer[Int], l: Int, r: Int) = {
    var l1 = l
    var r1 = r
    while(l1<r1) {
      val tmp = ints(l1)
      ints(l1) = ints(r1)
      ints(r1) = tmp
      l1=l1+1
      r1 = r1-1
    }

  }
}
