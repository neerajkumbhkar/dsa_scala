import sun.awt.image.ImageWatched.Link

import java.util.Scanner
import scala.collection.immutable

object SubSetOfArray {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val arr = Array.fill(sc.nextInt()) {
      sc.nextInt()
    }
    //print(subSetOfArrFunc(arr.toList, List()))
   print(subSetOfArr(arr))
  }

  def subSetOfArr(arr: Array[Int]): Unit = {
    (0 until Math.pow(2, arr.length).toInt).foreach { i =>
      var ans = ""
      var temp = i
      arr.reverse.foreach { e =>
        ans = if (temp % 2 == 0) "_\t" + ans else e + "\t" + ans
        temp /= 2
      }
      print(ans+"\t")
    }
  }


/*  def subSetOfArrFunc(arr: Array[Int]): List[String] = {

    (0 until Math.pow(2, arr.length).toInt).toList.map { i =>
      var ans = ""
      var temp = i
      arr.reverse.map { e =>
        ans = if (temp % 2 == 0) "_\t" + ans else e + "\t" + ans
        temp /= 2
      }
      ans
    }
  }*/
}