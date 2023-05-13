import java.util.Scanner
import scala.util.control.Breaks.{break, breakable}

object PrimeNumber extends App {
 /* def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val t = sc.nextInt()
    for(i<-0 until(t)){
      val n = sc.nextInt()
      var isPrime = true
      breakable{
        for (i <- 2 to n if (i * i <= n)) {
          if(n%i==0){
            isPrime = false
            break
          }
        }
      }

      if(isPrime) println("prime") else println("not prime")
    }
  }*/

  def isPrime(n:Int):Boolean= n match
    case 0 | 1  => throw new IllegalArgumentException()
    case i => {
        !(2 to scala.math.sqrt(i).toInt).exists(x => i % x == 0)
    }
  println(isPrime(21))
}
