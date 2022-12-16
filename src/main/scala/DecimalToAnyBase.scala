import java.util.Scanner

object DecimalToAnyBase {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    var n = sc.nextInt()
    val b = sc.nextInt()
    var ans = 0
    var it = 0
    while (n > 0) {
        var rem = n%b
        it = it+1
        n=n/b
        ans=ans+rem*Math.pow(10,it).toInt
    }
    print(ans)
  }
}
