
/*
* 1. You are given two numbers n and k. You are required to rotate n, k times to the right. If k is positive, rotate to the right i.e. remove rightmost digit and make it leftmost. Do the reverse for negative value of k. Also k can have an absolute value larger than number of digits in n.
2. Take as input n and k.
3. Print the rotated number.
4. Note - Assume that the number of rotations will not cause leading 0's in the result. e.g. such an input will not be given
   n = 12340056
   k = 3
   r = 05612340

Input Format
"n" where n is any integer.
"K" where k is any integer.
Output Format
"r", the rotated number

Constraints
1 <= n < 10^9
-10^9 < k < 10^9
Sample Input
562984
2
Sample Output
845629
* */
object RotateANumber extends App {

  def rotateANumber(rot: Int, i: Int): Int = {
    val nod = (math.log10(i) + 1).toInt
    val k = rot % nod
    val kth = if (k < 0) k + nod else k
    val div = math.pow(10, kth).toInt
    val remDigit = i / div
    val rotDigit = i % div
    rotDigit * (math.pow(10, (math.log10(remDigit)+1).toInt).toInt) + remDigit
  }

print(rotateANumber(2, 562984))
}
