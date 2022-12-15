import java.util.Scanner
import scala.util.control.Breaks.{break, breakable}

object PrimeFactorization {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    var n = sc.nextInt()
    var div = 2
    breakable{
      while (n > 1) {
        if (n % div == 0) {
          println(div)
          n = n / div
        } else if (div * div > n) {
          break
        } else {
          div = div + 1
        }
      }
    }
    if(n!=1) println(n)
  }
}
