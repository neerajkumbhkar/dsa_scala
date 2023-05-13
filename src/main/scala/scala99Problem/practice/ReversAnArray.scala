package scala99Problem.practice

object ReversAnArray extends App {
  def reversAnArray(arr:Array[Int]):Array[Int] = {
    arr.foldLeft(Array[Int]())((h,t) => t+:h)
  }
  reversAnArray(Array(1,2,3,4)).foreach(println)
}
