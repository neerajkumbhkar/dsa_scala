object NumberStringEncoding {
  val encodingMap: Map[String, Char] = Map(
    "1" -> 'a',
    "2" -> 'b',
    "3" -> 'c',
    "4" -> 'd',
    "5" -> 'e',
    "6" -> 'f',
    "7" -> 'g',
    "8" -> 'h',
    "9" -> 'i',
    "10" -> 'j',
    "11" -> 'k',
    "12" -> 'l',
    "13" -> 'm',
    "14" -> 'n',
    "15" -> 'o',
    "16" -> 'p',
    "17" -> 'q',
    "18" -> 'r',
    "19" -> 's',
    "20" -> 't',
    "21" -> 'u',
    "22" -> 'v',
    "23" -> 'w',
    "24" -> 'x',
    "25" -> 'y',
    "26" -> 'z'
  )

  def generateEncodings(numberString: String): List[String] = {
    def encodeHelper(numberString: String): List[String] = numberString match {
      case "" => List("")
      case _ =>
        val encodings: List[String] =
          if (numberString.length >= 2 && encodingMap.contains(numberString.substring(0, 2)))
            encodeHelper(numberString.substring(2)).map(encodingMap(numberString.substring(0, 2)) + _)
          else List()

        val singleDigitEncoding: List[String] =
          encodeHelper(numberString.substring(1)).map(encodingMap(numberString.substring(0, 1)) + _)

        encodings ++ singleDigitEncoding
    }

    encodeHelper(numberString)
  }

    def generateEncodingsWithoutMap(numberString: String): List[String] = {
      def encodeHelper(numberString: String): List[String] = numberString match {
        case "" => List("")
        case digit if digit.startsWith("0") => List()
        case digit if digit.length >= 2 && digit.substring(0, 2).toInt <= 26 =>
          encodeHelper(numberString.substring(1)).map((digit.substring(0, 1).toInt + 'a' - 1).toChar + _) ++
            encodeHelper(numberString.substring(2)).map((digit.substring(0, 2).toInt + 'a' - 1).toChar + _)
        case digit =>
          encodeHelper(numberString.substring(1)).map((digit.substring(0, 1).toInt + 'a' - 1).toChar + _)
      }

      encodeHelper(numberString)
    }

  def generateEncodings2(numberString: String): List[String] = {
    def encodeHelper(numberString: String): List[String] = numberString match {
      case "" => List("")
      case _ =>
        val singleDigitEncoding: List[String] =
          if (numberString.charAt(0) != '0')
            encodeHelper(numberString.substring(1)).map((numberString.charAt(0) - '1' + 'a').toChar + _)
          else List()

        val twoDigitEncoding: List[String] =
          if (numberString.length >= 2 && numberString.charAt(0) != '0') {
            val twoDigit = numberString.substring(0, 2).toInt
            if (twoDigit >= 10 && twoDigit <= 26)
              encodeHelper(numberString.substring(2)).map((twoDigit - 1 + 'a').toChar + _)
            else List()
          } else List()

        singleDigitEncoding ++ twoDigitEncoding
    }

    encodeHelper(numberString)
  }

  def generateEncodings3 (numberString: String): List[String] = {
    def encodeHelper(numberString: String): List[String] = numberString.toList match {
      case Nil => List("")
      case head :: tail =>
        val singleDigitEncoding: List[String] = head match {
          case '0' => List()
          case _ => encodeHelper(tail.mkString).map((head - '1' + 'a').toChar + _)
        }

        val twoDigitEncoding: List[String] = numberString.take(2) match {
          case twoDigits if twoDigits.length == 2 && twoDigits.toInt >= 10 && twoDigits.toInt <= 26 =>
            encodeHelper(numberString.drop(2)).map((twoDigits.toInt - 1 + 'a').toChar + _)
          case _ => List()
        }

        singleDigitEncoding ++ twoDigitEncoding
    }

    encodeHelper(numberString)
  }

  def main(args: Array[String]): Unit = {
    val numberString = "123"
    val encodings = generateEncodings(numberString)
    val encodings1 = generateEncodingsWithoutMap(numberString)
    println(encodings)
    println(encodings1)

  }
}

