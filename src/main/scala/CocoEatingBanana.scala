/*
* 875. Koko Eating Bananas
Medium
6.1K
292
Companies
Koko loves to eat bananas. There are n piles of bananas, the ith pile has piles[i] bananas. The guards have gone and will come back in h hours.

Koko can decide her bananas-per-hour eating speed of k. Each hour, she chooses some pile of bananas and eats k bananas from that pile. If the pile has less than k bananas, she eats all of them instead and will not eat any more bananas during this hour.

Koko likes to eat slowly but still wants to finish eating all the bananas before the guards return.

Return the minimum integer k such that she can eat all the bananas within h hours.



Example 1:

Input: piles = [3,6,7,11], h = 8
Output: 4
Example 2:

Input: piles = [30,11,23,4,20], h = 5
Output: 30
Example 3:

Input: piles = [30,11,23,4,20], h = 6
Output: 23
*
* */

object CocoEatingBanana extends App {
  def minEatingSpeed(piles: Array[Int], h: Int): Int = {
    var l = 1
    var r = piles.max

    while (l < r) {
      val mid = l + (r - l) / 2

      if (isValid1(piles, h, mid)) {
        r = mid
      } else {
        l = mid + 1
      }
    }

    l
  }

  def minEatingSpeedIterative(piles: Array[Int], h: Int): Int = {
    var lo = 1
    var hi = 1e9.toInt
    var speed = 1e9.toInt
    while (lo <= hi) {
      var mid = lo + (hi - lo) / 2
      if (chaker(piles, mid, h)) {
        speed = mid
        hi = mid - 1
      } else lo = mid + 1
    }
    speed
  }

  private def isValid(piles: Array[Int], h: Int, speed: Int): Boolean = {
    piles.foldLeft(0)((time, pile) => {
      time + pile / speed + (if (pile % speed != 0) 1 else 0)
    }) <= h
  }

  private def isValid1(piles: Array[Int], h: Int, speed: Int): Boolean = {
    piles.foldLeft(0)((time, pile) => {
      time + Math.ceil(pile / (1.0 * speed)).toInt
    }) <= h
  }

  def chaker(piles: Array[Int], speed: Int, h: Int): Boolean = {
    var time = 0
    for (nob <- piles) {
      time = time + Math.ceil(nob / (1.0 * speed)).toInt
    }
    time <= h
  }

  def minEatingSpeed3(piles: Array[Int], h: Int): Int = {
    val maxPile = piles.max
    var left = (maxPile / h) max 1 max (piles.sum / h)
    var right = maxPile
    while (left < right) {
      val mid = left + (right - left) / 2
      if (timeForEatingPile(speed = mid, piles = piles) > h) {
        // try to increase speed
        left = mid + 1
      } else {
        // try to lower speed
        right = mid
      }
    }
    left
  }

  def timeForEatingPile(speed: Int, piles: Array[Int]) = {
    piles.map {
      case pile if pile % speed == 0 => pile / speed
      case pile => pile / speed + 1
    }.sum
  }

  def minEatingSpeed4(piles: Array[Int], h: Int): Int = {
    val maxPiles = piles.max

    def binarySearch(left: Int, right: Int, k: Int): Int = left match {
      case l if l > right => k
      case _ =>
        val mid = ((left + right) / 2).toDouble
        val hoursToEatPiles = piles.foldLeft(0)((acc, p) => acc + Math.ceil(p / mid).toInt)
        hoursToEatPiles match {
          case p if p < 0 => k
          case p if p <= h => binarySearch(left, mid.toInt - 1, Math.min(mid.toInt, k))
          case _ => binarySearch(mid.toInt + 1, right, k)
        }
    }
    binarySearch(1, maxPiles, maxPiles)
  }

  val piles = Array(3, 6, 7, 11)
  print(minEatingSpeed4(piles, 8))
}
