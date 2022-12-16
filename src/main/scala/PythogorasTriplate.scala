import java.util.Scanner

object PythogorasTriplate {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val a = sc.nextInt()
    val b = sc.nextInt()
    val c =sc.nextInt()
    var z = false
    if (a>b && a>c && ((b*b)+(c*c))==a*a){
      z = true
    } else if (b > a && b > c && ((a * a) + (c * c)) == b * b) {
      z=true
    } else if (c > a && c > b && ((a * a) + (b * b)) == c * c) {
      z = true
    }
    print(z)
  }
}
