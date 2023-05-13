import java.util.Scanner
/*
* Inverse Of A Number
Easy  Prev   Next
1. You are given a number following certain constraints.
2. The key constraint is if the number is 5 digits long, it'll contain all the digits from 1 to 5 without missing any and without repeating any. e.g. 23415 is a 5 digit long number containing all digits from 1 to 5 without missing and repeating any digit from 1 to 5.Take a look at few other valid numbers - 624135, 81456273 etc.Here are a few invalid numbers - 139, 7421357 etc.
3. The inverse of a number is defined as the number created by interchanging the face value and index of digits of number.e.g. for 426135 (reading from right to left, 5 is in place 1, 3 is in place 2, 1 is in place 3, 6 is in place 4, 2 is in place 5 and 4 is in place 6), the inverse will be 416253 (reading from right to left, 3 is in place 1, 5 is in place 2,2 is in place 3, 6 is in place 4, 1 is in place 5 and 4 is in place 6) More examples - inverse of 2134 is 1243 and inverse of 24153 is 24153
4. Take as input number "n", assume that the number will follow constraints.
5. Print it's inverse.
Input Format
"n" where n is any integer, following constraints defined above.
Output Format
"i", the inverted number
*
Constraints
1 <= n < 10^8, and follwing other constraints defined above.
Sample Input
28346751
Sample Output
73425681
* */
object InverseAnumber extends App {
  //impretive way
 /* def main(args: Array[String]): Unit = {
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
*/
  //fuctional way
  def inverseANumberFun(n:Int):Int = {
    def _inverseANumber(acc:Int,pos:Int,i:Int):Int = {
      i match
        case 0 => acc
        case i => {
          val lastDigit = i%10
          _inverseANumber(acc+(pos*(math.pow(10,lastDigit-1).toInt)),pos+1,i/10)
        }
    }
    _inverseANumber(0,1,n)
  }
  print(inverseANumberFun(28346751))
}
