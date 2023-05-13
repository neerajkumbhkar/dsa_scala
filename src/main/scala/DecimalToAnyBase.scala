import java.util.Scanner
/*
* Decimal To Any Base
1. You are given a decimal number n.
2. You are given a base b.
3. You are required to convert the number n into its corresponding value in base b.
Input Format
A number n
A base b
Output Format
A number representing corresponding value of n in number system of base b
*
Constraints
0 <= d <= 512
2 <= b <= 10

Sample Input
57
 2
Sample Output
111001
* */
object DecimalToAnyBase extends App {
  /*def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    var n = sc.nextInt()
    val b = sc.nextInt()
    var ans = 0
    var it = 0
    while (n > 0) {
        var rem = n%b
        n=n/b
        ans=ans+rem*Math.pow(10,it).toInt
        it = it+1
    }
    print(ans)
  }*/

  def decimalToAnyBase(d:Int,b:Int):Int = {
    def _decimalToAnyBase(dec:Int,base:Int,ans:Int,it:Int):Int ={
      dec match
        case 0 => ans
        case i =>
          val rem = i%b
          val ans1 = ans+rem*(math.pow(10,it)).toInt
          _decimalToAnyBase(dec/base,base,ans1,it+1)
    }
    _decimalToAnyBase(d,b,0,0)
  }
  print(decimalToAnyBase(57,2))
}
