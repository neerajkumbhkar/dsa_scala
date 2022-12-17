import java.util
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object SumOfTwoArray {

  def sumOfTwoArr(arr1:Array[Int], n1: Int, arr2: Array[Int], n2: Int): Unit = {
    var p1=n1-1
    var p2= n2-1
    val n3 = if(n1>n2) n1 else n2
    var p3 = n3-1
    var sum = 0
    var carry = 0
    var arr3 = Array[Int]()
    while(p3>0){
      sum=(if(arr1(p1)>=0) arr1(p1) else 0 ) + (if(arr2(p2)>=0) arr2(p2) else 0) + carry
      val add = sum%10
      arr3 = Array.fill(n3){add}
      carry = sum/10
      p1 = p1-1
      p2 = p2-1
      p3 = p3-1
    }
    if(carry>0) print(carry)
    arr3.foreach(print(_))
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n1 = sc.nextInt()
    val arr1 = Array.fill(n1){
        sc.nextInt()
      }
    val n2 = sc.nextInt()
    val arr2 = Array.fill(n2) {
        sc.nextInt()
    }
    sumOfTwoArr(arr1,n1,arr2,n2)
  }
}
