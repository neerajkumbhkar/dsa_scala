
/*Climb Stairs
Easy  Prev   Next
1. You are given a number n, representing the number of stairs in a staircase.
2. You are on the 0th step and are required to climb to the top.
3. In one move, you are allowed to climb 1, 2 or 3 stairs.
4. You are required to print the number of different paths via which you can climb to the top.
Input Format
A number n
Output Format
A number representing the number of ways to climb the stairs from 0 to top.
Constraints
0 <= n <= 20
Sample Input
  4
Sample Output
  7
*/
object ClaimStaires extends App {
  def claimStairsDp(n: Int): Int = {
    def _claimStairsDp(n: Int, strg: Array[Int]): Int = {
      n match
        case n if n == 0 => 1
        case n if n < 0  => 0
        case n =>
          if (strg(n) != 0) strg(n)
          else
            val fn = _claimStairsDp(n - 1, strg)
              + _claimStairsDp(n - 2, strg)
              + _claimStairsDp(n - 3, strg)
            strg(n) = fn
            fn
    }
    _claimStairsDp(n, new Array[Int](n + 1))
  }

  println(claimStairsDp(4))
}
