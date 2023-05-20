object SortBinaryString extends App {
  def sortBinaryString(binaryString: String):String = {
    binaryString.sorted
  }
  val binaryString = "101010011100101"
  println(sortBinaryString(binaryString))
}
