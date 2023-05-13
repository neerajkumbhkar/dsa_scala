
  object TargetSumSubset {
    def targetSumSubset(list: List[Int], target: Int): List[List[Int]] = (list, target) match {
      case (_, 0) => List(List()) // base case: there exists a subset that adds up to 0 for any set of integers
      case (Nil, _) => List() // base case: there are no integers left and the target is nonzero
      case (head :: tail, t) =>
        val includeHead = targetSumSubset(tail, t - head).map(asf => head :: asf)
        val excludeHead = targetSumSubset(tail, t)
        includeHead ++ excludeHead
    }

    def main(args: Array[String]): Unit = {
      val nums = List(3, 34, 4, 12, 5, 2)
      val target = 7
      val subsets = targetSumSubset(nums, target)
      if (subsets.nonEmpty) {
        println(s"Subsets of $nums that add up to $target:")
        subsets.foreach(println)
      } else {
        println(s"There does not exist a subset of $nums that adds up to $target")
      }
    }

  //  object TargetSumSubset {
      //List(3, 34, 4, 12, 5, 2)
      def targetSumSubset1(nums: List[Int], target: Int): List[List[Int]] = (nums, target) match {
        case (_, 0) => List(List()) // base case: there exists a subset that adds up to 0 for any set of integers
        case (Nil, _) => List() // base case: there are no integers left and the target is nonzero
        case (head :: tail, t) =>
          // recursively find the subsets of the remaining integers that add up to target - head
          val subsetsWithHead = targetSumSubset1(tail, t - head).map(head :: _)
          // recursively find the subsets of the remaining integers that add up to target without including head
          val subsetsWithoutHead = targetSumSubset1(tail, t)
          // concatenate the two sets of subsets to get the final set of subsets that add up to target
          subsetsWithHead ++ subsetsWithoutHead
      }

  //  }

  }




