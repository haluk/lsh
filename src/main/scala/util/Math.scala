package util

/**
  * Created by hd on 3/5/17.
  */
object Math {

  def sieve(s: Stream[Int]): Stream[Int] = {
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  }

  def hashFunction(p: Int): (Int, Int) = {
    val r = scala.util.Random
    val a = r.nextInt(p)
    val b = r.nextInt(p)

    return (a,b)
  }

  def jaccardSimilarity(xs: Seq[Int], ys: Seq[Int]): Double = {
    val union = xs.zip(ys).filterNot(_ == (0,0))
    val intersection = union.filter{case (x,y) => x == y}

    return intersection.size / union.size.toDouble
  }

}
