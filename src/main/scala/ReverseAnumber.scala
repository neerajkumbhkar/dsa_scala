package scala99Problem.practice

object ReverseAnumber extends App {
  def reverseAnumber(n: Int): Int = {
    def _reverseAnumber(i: Int, i1: Int): Int = i match
      case 0 => i1
      case i =>
        val n = i % 10
        val n1 = n * math.pow(10, (((math.log10(i) + 1).toInt) - 1)).toInt
        _reverseAnumber(i / 10, i1 + n1)

    _reverseAnumber(n, 0)
  }

  println(reverseAnumber(12345))
}
