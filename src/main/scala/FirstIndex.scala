object FirstIndex extends App {
  def firstIndex(arr:Array[Int],idx:Int,elm:Int):Int =
    if(arr.length==idx) idx
    else
      arr match
        case Array() => throw new IllegalArgumentException()
        case Array(t) => if (t == elm) idx else -1
        case _ => firstIndex(arr, idx + 1, elm)

  println(firstIndex(Array(2,3,4,3,8,5),0,3))
}
