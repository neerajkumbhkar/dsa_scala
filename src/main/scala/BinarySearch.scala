import java.util.Scanner
import collection.Searching.Found
import scala.annotation.tailrec
object BinarySearch {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val arr = Array.fill(sc.nextInt()){sc.nextInt()}
    val key = sc.nextInt()
    print(binarySearch1(arr,key))
    print(search(arr,key))
  }
  def binarySearch1(arr:Array[Int],key:Int):Int = {
    @tailrec
    def helper(l:Int,r:Int):Int={
      if l>r then -1
      else
        val mid = l + (r-l)/2
        mid match {
          case mid if arr(mid) == key => mid
          case mid if arr(mid) < key => helper(mid + 1, r)
          case _ => helper(l, mid - 1)
        }
    }
    helper(0,arr.length-1)
  }


  def search(nums: Array[Int], target: Int): Int =
    nums.search(target) match {
      case Found(index) => index
      case _ => -1
    }
}
