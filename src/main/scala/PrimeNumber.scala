import java.util.Scanner
import scala.util.control.Breaks.{break, breakable}

object PrimeNumber {
  def main(args: Array[String]): Unit = {
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
  }
}
