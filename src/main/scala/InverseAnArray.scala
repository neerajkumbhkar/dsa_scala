import java.util.Scanner

object InverseAnArray {

  def inverseArray(arr: Array[Int]) = {
    // create an array with new keyword and with an initial size
    val f = new Array[Int](arr.length)
    for(i<-0 until arr.length){
      val a = arr(i)
      f(a)=i
    }
    f
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val arr = Array.fill(n){sc.nextInt()}
    inverseArray(arr).foreach(print(_))
  }
}
