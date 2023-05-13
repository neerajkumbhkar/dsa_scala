object KokoEatingBanan {
  def minEatingSpeed(piles:Array[Int],h:Int):Int = {
    val max = piles.reduceLeft(_ max _)
    if h==piles.length then max
    else
      minEatingSpeedHelper(1,1e9.toInt,piles,h)
  }
  def minEatingSpeedHelper(lo: Int, hi: Int, piles:Array[Int],h:Int):Int = {
    val speed = lo+(hi-lo)/2
      speed match
      case speed if lo<=hi => speed
      case speed if chaker(piles,lo,hi)==true => minEatingSpeedHelper(lo,hi-1,piles,h)
      case speed if chaker(piles,lo,hi)==false=> minEatingSpeedHelper(lo+1,hi,piles,h)
  }
  def chaker(piles: Array[Int], speed: Int, h: Int):Boolean={
    var time = 0
    for nob<-piles
      do
        time = time + Math.ceil(nob*1.0/speed).toInt
    time<=h
  }
}
