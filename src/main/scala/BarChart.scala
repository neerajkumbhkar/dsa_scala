import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
object BarChart {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    var arr = ArrayBuffer[Int]()
    var max = Int.MinValue
    for (i <- 0 until n) {
      val e = sc.nextInt()
      arr+=e
      max = Math.max(max, arr(i))
    }
    (1 to max).reverse.foreach { i=>
      arr.foreach { e=>
        if (e>=i) print("*\t") else  print("\t")
      }
      println()
    }
  }
}
