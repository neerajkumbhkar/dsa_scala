import java.util.Scanner

object SubArray {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val arr = Array.fill(n){sc.nextInt()}
  /*  for(i<-0 until(n)){
      for(j<-i until(n)){
        for(k<-i to j ){
          print(arr(k)+"\t")
        }
        println()
      }
    }*/
    subArrayFun(arr)
  }
  def subArrayFun[Int](ar:Array[Int]):Unit = {
    (0 until ar.length).foreach{i=>
      (i until ar.length).foreach{j=>
        (i to j).foreach{k=>
          print(ar(k))
        }
        println()
      }
    }
  }

}
