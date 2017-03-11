package distance

/**
  * Created by hd on 3/3/17.
  */
abstract class Distance(x: Seq[Double], y: Seq[Double]) {
  def computeDistance: Double
}
