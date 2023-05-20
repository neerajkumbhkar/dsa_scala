object Fib extends App {
  def fibnocci(n:Int):Int={
    n match
      case n if n==0|n==1=>n
      case n => fibnocci(n-1)+fibnocci(n-2)
  }

  def fibnocciDp(n: Int): Int = {
    def _fibnocciDp(n: Int,strg:Array[Int]): Int = {
      n match
        case n if n == 0 | n == 1 => n
        case n  =>
          if(strg(n)!=0) strg(n)
          else
            val fn = _fibnocciDp(n - 1,strg) + _fibnocciDp(n - 2,strg)
            strg(n) = fn
            fn
    }
    _fibnocciDp(n,new Array[Int](n+1))
  }

println(fibnocciDp(8))
}
