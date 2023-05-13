/*
* LEETCODE
* https://leetcode.com/problems/spiral-matrix/
*
* Input: matrix = [[1,2,3],[4,5,6],[7,8,9]]
Output: [1,2,3,6,9,8,7,4,5]
*
* Input: matrix = [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
Output: [1,2,3,4,8,12,11,10,9,5,6,7]
* */
object SpiralMatrix extends App {
    def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {

      def rowSlice(rowIdx: Int, colRange: Range): Iterable[Int] =
        colRange.view.map(c => matrix(rowIdx)(c))

      def colSlice(rowRange: Range, colIdx: Int): Iterable[Int] =
        rowRange.view.map(r => matrix(r)(colIdx))

      val values = scala.collection.mutable.ListBuffer[Int]()
      var (m, n) = (matrix.size, matrix(0).size)

      // We will reduce by 2 every iteration,
      // but ceil because we must do a 'partial' iteration for the remainder.
      val iterations = Math.ceil((m min n) / 2.0)

      var i = 0
      while (i < iterations) {
        val top = i
        val right = n - 1 - i
        val bottom = m - i - 1
        val left = i

        // We are guaranteed to have a top row
        val topRow = rowSlice(top, Range.inclusive(left, right))
        values.addAll(topRow)

        // and thus a rightColumn
        val rightColumn = colSlice(Range.inclusive(top + 1, bottom), right)
        values.addAll(rightColumn)

        // But not a bottomRow
        if (bottom > top) {
          val bottomRow = rowSlice(bottom, Range(left, right).reverse)
          values.addAll(bottomRow)

          // Nor a leftColumn
          if (left < right) {
            val leftColumn = colSlice(Range(top + 1, bottom).reverse, left)
            values.addAll(leftColumn)
          }
        }

        i += 1
      }

      values.toList
    }

  import scala.annotation.tailrec

    def spiralOrder1(matrix: Array[Array[Int]]): List[Int] = {
      spiral(0, 0)(Array.emptyIntArray, matrix)
    }

    @tailrec
    def spiral(point: (Int, Int))(curr: Array[Int], matrix: Array[Array[Int]]): List[Int] = {
      /*
      * 1 2 3
      * 4 5 6
      * 7 8 9
      * */
      if (matrix.isEmpty)
        curr.toList
      else {
        val col = if (matrix.head.length == 0) 0 else matrix.head.length - 1
        println(s"col ${col}")
        val row = if (matrix.length == 0) 0 else matrix.length - 1
        println(s"row ${row}")
        println(s"point ${point}")
        println(s"curr ${curr}")
        point match {
          case (0, 0) => spiral(0, col)(curr = curr ++ matrix.head, matrix.tail)
          case (0, _) => spiral(row, col)(curr = curr ++ matrix.map(_(col)), matrix.map(_.dropRight(1)))
          case (_, 0) => spiral(0, 0)(curr = curr ++ matrix.map(_.head).reverse, matrix.map(_.tail))
          case (_, _) => spiral(row, 0)(curr = curr ++ matrix.reverse.head.reverse, matrix.reverse.tail.reverse)
        }
      }
    }

    /*
    *This question is very like RotateImage, we can reuse the function.

    Consider this problem can be convert to get the first line, then rotate the matrix counterclockwise.
    For Example.

    1 2 3              4 5 6              6 9
    4 5 6  => [1,2,3]  7 8 9   => [1,2,3] 5 8  =>  [1,2,3,6,9] 5 8  => [1,2,3,6,9] 8 7 =>
    7 8 9                                 4 7                  4 7                 5 4

    [1,2,3,6,9,8,7] 5 4 => [1,2,3,6,9,8,7] 4 => [1,2,3,6,9,8,7,4] 5 => [1,2,3,6,9,8,7,4,5]
                                           5
    How can we rotate the matrix counterclockwise? Well, a trivial solution is rotate it clockwise three times,

    Another way is to first transpose it and then reverse it.

    4 5 6   =>  4 7  =>  6 9
    7 8 9       5 8      5 8
                6 9      4 7
    Then just implement it
     * */

      /** RunTime Info:
        * 244ms, 44MB
        *
        * @param matrix input matrix
        * @return matrix elements in spiral order
        */
      def spiralOrder0(matrix: Array[Array[Int]]): List[Int] = {
        def snail(xs: List[List[Int]]): List[Int] = xs match {
          case Nil => Nil
          case x :: xs => x ++ snail(xs.transpose.reverse)
        }

        snail(matrix.toList.map(_.toList))
      }







        def spiralOrder2(matrix: Array[Array[Int]]): List[Int] = {
          println(s"matrix head ${matrix.head.toList}")
          /*println(s"matrix transpose ${matrix.transpose.map(_.toList).foreach(println(_))}")
          println(s"matrix tail ${matrix.tail.map(_.toList).foreach(println(_))}")
          println(s"matrix tail transpose ${matrix.tail.transpose.map(_.toList).foreach(println(_))}")*/
          println(s"matrix tail transpose reverse ${matrix.tail.transpose.reverse.map(_.toList).foreach(println(_))}")
          def dfs(mx: Array[Array[Int]]): List[Int] = mx match {
            case mx if mx.isEmpty => List()
            case mx if mx.length == 1 => mx.head.toList
            case _ => mx.head.toList ::: spiralOrder2(mx.tail.transpose.reverse)
          }

          dfs(matrix)
        }


    sealed trait Direction

    case object Right extends Direction

    case object Down extends Direction

    case object Left extends Direction

    case object Up extends Direction


    def spiralOrder3(matrix: Array[Array[Int]]): List[Int] = {
      if (matrix.isEmpty) Nil
      else rec(matrix, Right, List.empty[Int], 0, matrix.length - 1, 0, matrix(0).length - 1)
    }

    def rec(matrix: Array[Array[Int]], direction: Direction, result: List[Int], rowBegin: Int, rowEnd: Int, colBegin: Int, colEnd: Int): List[Int] = {
      if (rowBegin > rowEnd || colBegin > colEnd) result.reverse
      else {
        direction match {
          case Right => {
            val newResult = (colBegin to colEnd).foldLeft(result) { case (acc, col) => matrix(rowBegin)(col) :: acc }
            rec(matrix, nextDirection(direction), newResult, rowBegin + 1, rowEnd, colBegin, colEnd)
          }
          case Down => {
            val newResult = (rowBegin to rowEnd).foldLeft(result) { case (acc, row) => matrix(row)(colEnd) :: acc }
            rec(matrix, nextDirection(direction), newResult, rowBegin, rowEnd, colBegin, colEnd - 1)
          }
          case Left => {
            val newResult = (colEnd to colBegin by -1).foldLeft(result) { case (acc, col) => matrix(rowEnd)(col) :: acc }
            rec(matrix, nextDirection(direction), newResult, rowBegin, rowEnd - 1, colBegin, colEnd)
          }
          case Up => {
            val newResult = (rowEnd to rowBegin by -1).foldLeft(result) { case (acc, row) => matrix(row)(colBegin) :: acc }
            rec(matrix, nextDirection(direction), newResult, rowBegin, rowEnd, colBegin + 1, colEnd)
          }
        }
      }
    }

    def spiralOrder4(matrix: Array[Array[Int]]): List[Int] = {

      if (matrix.isEmpty || matrix(0).isEmpty) return List()

      (matrix.size, matrix(0).size) match {
        case (1, _) => matrix(0).toList
        case (_, 1) => matrix.map(_(0)).toList
        case (h, w) => matrix(0).toList ::: matrix.map(_(w - 1)).tail.toList ::: matrix(h - 1).reverse.tail.toList ::: matrix.map(_(0)).reverse.tail.init.toList ::: spiralOrder4(matrix.init.tail.map(_.init.tail))
      }
    }
    def nextDirection(direction: Direction): Direction = {
      direction match {
        case Right => Down
        case Down => Left
        case Left => Up
        case Up => Right
      }
    }


   /* def spiralOrder5(matrix: Array[Array[Int]]): List[Int] = {
      if (matrix.isEmpty || matrix(0).isEmpty) {
        return List()
      }

      val left = 0
      val right = matrix(0).size - 1
      val upper = 0
      val lower = matrix.size - 1

      leftToRight(left, right, upper, lower, matrix)
    }

    def leftToRight(left: Int, right: Int, upper: Int, lower: Int, matrix: Array[Array[Int]]): List[Int] = {
      if (left <= right && upper <= lower) {
        matrix(upper).view(left, right + 1).foldRight(List[Int]())((elem, l) => elem :: l) ::: upperToLower(left, right, upper + 1, lower, matrix)
      } else {
        List[Int]()
      }
    }

    def upperToLower(left: Int, right: Int, upper: Int, lower: Int, matrix: Array[Array[Int]]): List[Int] = {
      if (left <= right && upper <= lower) {
        (for (row <- upper to lower) yield matrix(row)(right)).foldRight(List[Int]())((elem, l) => elem :: l) ::: rightToLeft(left, right - 1, upper, lower, matrix)
      } else {
        List[Int]()
      }

    }

    def rightToLeft(left: Int, right: Int, upper: Int, lower: Int, matrix: Array[Array[Int]]): List[Int] = {
      if (left <= right && upper <= lower) {
        matrix(lower).view(left, right + 1).foldRight(List[Int]())((elem, l) => l :+ elem) ::: lowerToUpper(left, right, upper, lower - 1, matrix)
      } else {
        List[Int]()
      }
    }

    def lowerToUpper(left: Int, right: Int, upper: Int, lower: Int, matrix: Array[Array[Int]]): List[Int] = {
      if (left <= right && upper <= lower) {
        (for (row <- upper to lower) yield matrix(row)(left)).foldRight(List[Int]())((elem, l) => l :+ elem) ::: leftToRight(left + 1, right, upper, lower, matrix)
      } else {
        List[Int]()
      }
    }*/
    /*
    * 1 2 3
    * 4 5 6
    * 7 8 9
    * */
    val mat = Array(Array(1,2,3),Array(4,5,6),Array(7,8,9))
  print(spiralOrder0(mat))
}
