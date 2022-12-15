import java.util.Scanner

object GCD {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    var n1 = sc.nextInt()
    var n2 = sc.nextInt()
    val oa = n1
    val ob = n2
    while (n1 % n2 != 0) {
      var reminder = n1 % n2
      n1 = n2
      n2 = reminder
    }
    val gcd = n2
    val lcm = (oa * ob)/gcd
    println(gcd)
    println(lcm)
  }
}
