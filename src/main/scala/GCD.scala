import java.util.Scanner

/*
* 1. You are required to print the Greatest Common Divisor (GCD) of two numbers.
2. You are also required to print the Lowest Common Multiple (LCM) of the same numbers.
3. Take input "num1" and "num2" as the two numbers.
4. Print their GCD and LCM.
Input Format
num1
num2
.. the numbers whose GCD and LCM we have to find.
Output Format
a
b
.. where 'a' and 'b' are the GCD and LCM respectively.

Constraints
2 <= n <= 10^9
Sample Input
36
24
Sample Output
12
72
* */
object GCD extends App {
    def gcd(a1:Int,b1:Int):Int = (a1,b1) match
      case  (a1,0) => a1
      case (a1,b1) => gcd(b1,a1%b1)
/*  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    var n1 = sc.nextInt()
    var n2 = sc.nextInt()
    val oa = n1
    val ob = n2
    while (n1 % n2 != 0) {
      val reminder = n1 % n2
      n1 = n2
      n2 = reminder
    }
    val gcd = n2
    val lcm = (oa * ob)/gcd
    println(gcd)
    println(lcm)
  }*/
  println(gcd(36,24))
}
