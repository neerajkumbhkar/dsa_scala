package scala99Problem.practice

import scala.annotation.tailrec

object CielFloor extends App {
  def binarySearchCielFloor(arr: Array[Int], key: Int): (Int,Int) = {
    @tailrec
    def helper(l: Int, r: Int,f:Int,c:Int): (Int,Int) = {
      if l > r then (f,c)
      else
        val mid = l + (r - l) / 2
        mid match {
          case mid if arr(mid) == key => (arr(mid),arr(mid))
          case mid if arr(mid) < key =>
            val floor = arr(mid)
            helper(mid + 1, r,floor,c)
          case _ =>
            val ciel = arr(mid)
            helper(l, mid - 1,f,ciel)
        }
    }
    helper(0, arr.length - 1,-1,-1)
  }
println(binarySearchCielFloor(Array(1,2,4,5,6),0))
}
