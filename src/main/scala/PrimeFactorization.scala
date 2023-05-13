import java.util.Scanner
import scala.util.control.Breaks.{break, breakable}

/*
* Prime Factorization Of A Number
Easy  Prev   Next
1. You are required to display the prime factorization of a number.
    2. Take as input a number n.
    3. Print all its prime factors from smallest to largest.

Input Format
n, an integer
Output Format
p1  p2  p3  p4.. all prime factors of n

Sample Input
1440
Sample Output
2 2 2 2 2 3 3 5
* */
object PrimeFactorization extends App {
  /*def main(args: Array[String]): Unit = {
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
  }*/

  def primeFactorization(n: Int): List[Int] = {
    def _primeFactorization(n: Int, d: Int, res: List[Int]): List[Int] = {
      n match
        case 0 | 1 => res
        case i =>
          if (i % d == 0) {
            _primeFactorization(i / d, d, res ::: List(d))
          } else if (d * d > i) {
            _primeFactorization(0, d, res ::: List(i))
          } else {
            _primeFactorization(i, d + 1, res)
          }
    }
    _primeFactorization(n, 2, List())
  }


  print(primeFactorization(1440))
}
