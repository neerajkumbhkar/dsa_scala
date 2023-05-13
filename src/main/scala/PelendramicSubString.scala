import scala.::
import collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/*
* Print All Palindromic Substrings
Easy  Prev   Next
1. You are given a string.
2. You have to print all palindromic substrings of the given string.
Input Format
A String
Output Format
All palindromic substrings(one in a line).
First, all palindromic substrings starting from first character of string will be printed, then from second character and so on
*
* Sample Input
abcc
Sample Output
a
b
c
cc
c
*
* */
object PelendramicSubString extends App {
  def pelendramicSubString(s: String): ArrayBuffer[String] = {
    var res: ArrayBuffer[String] = ArrayBuffer("")
    for (i <- 0 until s.length) {
      for (j <- i until s.length) {
        res = res++printPalindrom(s.substring(i, j + 1))
      }
    }
    res
  }

  def printPalindrom(s: String): ArrayBuffer[String] = {
    //val arrStr = Array(s)
    def isArrayPalindrome[String](res:ArrayBuffer[String],ary: String): ArrayBuffer[String] =
      val ans = ary.toString.toArray.view
      .zip(ary.toString.toArray.reverse.view)
      .forall({ case (a, b) =>
        a == b
      }) 
      if (ans==true)
        val buff = res:+ary
        buff
      else res
    isArrayPalindrome(ArrayBuffer(),s)
  }


  //print(pelendramicSubString("abcc".toCharArray).toList)
  print(pelendramicSubString("abcc"))
}
