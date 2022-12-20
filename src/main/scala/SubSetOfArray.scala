import java.util.Scanner

object SubSetOfArray {
  def main(args: Array[String]): Unit = {
    val sc  =  new Scanner(System.in)
    val arr = Array.fill(sc.nextInt()){sc.nextInt()}
    subSetOfArr(arr)
  }
  def subSetOfArr(arr:Array[Int]):Unit={
    (0 until Math.pow(2,arr.length).toInt).foreach{ i =>
        var ans=""
        var temp=i
        arr.reverse.foreach{ e=>
            ans=if (temp%2==0) "_\t"+ans else e+"\t"+ans
            temp/=2
        }
        println(ans)
    }
  }
}
