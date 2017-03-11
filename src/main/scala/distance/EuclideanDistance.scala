package distance

/**
  * Created by hd on 3/4/17.
  */
class EuclideanDistance(x: Seq[Double], y: Seq[Double]) extends Distance(x, y) {
  override def computeDistance: Double = {
    math.sqrt(x.zip(y).map{case (a,b) => a-b}.map(c => math.pow(c,2)).reduce(_+_))
  }
}
