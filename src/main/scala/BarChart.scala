import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

/*
* 1. You are given a number n, representing the size of array a.
2. You are given n numbers, representing elements of array a.
3. You are required to print a bar chart representing value of arr a.
Input Format
A number n
n1
n2
.. n number of elements
Output Format
A bar chart of asteriks representing value of array a

Constraints
1 <= n <= 30
0 <= n1, n2, .. n elements <= 10
Sample Input
5
3
1
0
7
5
Sample Output
      *
      *
      *	*
      *	*
*			*	*
*			*	*
*	*		*	*
* */
object BarChart extends App {
  /*def main(args: Array[String]): Unit = {
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
  }*/

  def barChart(ls:List[Int]):Unit={
    (1 to ls.max).reverse.foreach(i =>
      ls.foreach(e =>
        if(i<=e) print("*\t") else print("\t")
      )
      println()
    )
  }
barChart(List(5,3,0,7,5))
}
