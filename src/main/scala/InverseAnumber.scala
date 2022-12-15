import java.util.Scanner

object InverseAnumber {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    var n = sc.nextInt()
    var y = 1
    var ans = 0
    while(n>0){
      val x = n%10
      ans = ans+y*Math.pow(10,x-1).toInt
      n = n/10
      y = y+1
    }
    print(ans)
  }
}
