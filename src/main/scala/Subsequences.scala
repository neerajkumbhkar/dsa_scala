object Subsequences {
  def getSubsequences(str: String): List[String] = {
    str match {
      case "" => List("") // Base case: empty string has one subsequence - the empty string itself
      case s =>
        val subseqWithoutFirst = getSubsequences(s.tail) // Recursively get subsequences without the first character
        subseqWithoutFirst ::: subseqWithoutFirst.map(s.head + _) // Append the first character to each subsequence
    }
  }

  def getSubsequences1(str: String): List[String] = {
    str match {
      case "" => List("") // Base case: empty string has one subsequence - the empty string itself
      case s =>
        val subseqWithFirst = getSubsequences(s.tail).map(s.head + _)
        val subseqWithoutFirst = getSubsequences(s.tail)// Recursively get subsequences without the first character
        subseqWithFirst ::: subseqWithoutFirst// Append the first character to each subsequence
    }
  }

    def getAllSubsequences(strings: List[String]): List[String] = strings match {
      case Nil => List("") // Base case: an empty string represents an empty subsequence
      case head :: tail =>
        val subseqsWithHead = getAllSubsequences(tail).map(head+_)
        val subseqsWithoutHead = getAllSubsequences(tail) // Recursively get subsequences of the remaining strings
        subseqsWithHead ++ subseqsWithoutHead // Append the subsequences without the current string and with the current string prepended
    }


  def main(args: Array[String]): Unit = {
    val str = "abc"
    val subsequences = getSubsequences1(str)
    println("Subsequences of '" + str + "':")
    subsequences.foreach(println)
  }
}

