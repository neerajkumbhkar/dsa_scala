import java.util.Scanner

object SubSetOfArray {

  def subSetArr(arr: Array[Int]): Unit = {
    val n = arr.length
    val trans = Math.pow(2, n).toInt
    var ans = ""
    for (i <- 0 until trans) {
      var temp = i
      for (j <- (0 to arr.length - 1).reverse) {
        ans = if (temp % 2 == 0) ans else arr(j) + ans
        temp = temp / 2
      }
      ans
    }
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val arr = Array.fill(sc.nextInt()){sc.nextInt()}
    subSetArr(arr)
  }
}
