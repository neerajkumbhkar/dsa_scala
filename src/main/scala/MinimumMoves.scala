/*
* 2027. Minimum Moves to Convert String
Easy
359
59
Companies
You are given a string s consisting of n characters which are either 'X' or 'O'.

A move is defined as selecting three consecutive characters of s and converting them to 'O'. Note that if a move is applied to the character 'O', it will stay the same.

Return the minimum number of moves required so that all the characters of s are converted to 'O'.



Example 1:

Input: s = "XXX"
Output: 1
Explanation: XXX -> OOO
We select all the 3 characters and convert them in one move.
Example 2:

Input: s = "XXOX"
Output: 2
Explanation: XXOX -> OOOX -> OOOO
We select the first 3 characters in the first move, and convert them to 'O'.
Then we select the last 3 characters and convert them so that the final string contains all 'O's.
Example 3:

Input: s = "OOOO"
Output: 0
Explanation: There are no 'X's in s to convert.
* */

object MinimumMoves extends App {
  def minimumMoves(s: String): Int = {
    ((s.length - s.replaceAll("X.?.?", "").length) / 3.0).ceil.toInt
  }

  def minimumMoves2(s: String): Int = {
    val it = (s + "").iterator
    it.foldLeft(0)((acc,cur)=>{
      if(cur =='X'){
        it.next()
        it.next()
        acc+1
      } else {
          acc
        }
    })
  }

  def minimumMoves1(s: String): Int = {
    val it = (s + "OO").toIterator
    it.foldLeft(0)((acc, cur) => {
      if (cur == 'X') {
        it.next; it.next; acc + 1
      }
      else acc
    })
  }

  def minimumMoves4(s: String): Int ={
    def _minimumMoves(count:Int,ls:List[Char]): Int = {
      ls match
        case Nil => count
        case x :: Nil => if (x == 'X') count+1 else count
        case x :: xs => if (x =='X') _minimumMoves(count+1,if(!xs.tail.isEmpty) xs.tail.tail else xs) else _minimumMoves(count,xs)
    }
    _minimumMoves(0,(s+"00").toList)
  }


  println(minimumMoves4("XXXXX"))
  print(minimumMoves4("OOOOXXXOXO"))

}

