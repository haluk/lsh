import java.io.File
import java.util.logging.Logger

import distance.EuclideanDistance
import preprocess.Preprocess

import scala.collection.SortedSet
import scala.collection.mutable.ListBuffer

/**
  * Created by hd on 3/4/17.
  */
object Application {
  val LOG = Logger.getLogger(Application.getClass.getName)

  def formattedResult(result: ListBuffer[(String, String, Double)]): Unit = {
    println("%20s\t%20s\t%20s".format("Doc Pair-1", "Doc-Pair-2", "Jaccard Similarity"))
    result.foreach(i =>
      println("%20s\t%20s\t%20s".format(i._1, i._2, i._3))
    )
  }


  def main(args: Array[String]): Unit = {

    val preprocess = new Preprocess
    val lsh = new LSH
    val docShinglePair = lsh.getShingles(args(0), preprocess, lsh)
//    println(docShinglePair)

    val b = 10
    val r = 3
    val n = b*r
    val p = 60289
    val t = 0.9

    val universalSet = SortedSet[String]() ++ docShinglePair.map(x => x._2).flatten.toSet // n
    LOG.info("Universal set size: " + universalSet.size)
    LOG.info("Used prime number: " + p)
    LOG.info("Number of bands: " + b)
    LOG.info("Number of rows in each band: " + r)
    LOG.info("Number of hash functions: " + b*r)
    LOG.info("Similarity threshold (t) is " + t)
//    println(universalSet)
//    val p = lsh.findPrime(universalSet.size)
//    println(p)
    LOG.info("Building occurrence matrix (M) is started ...")
    val occurrenceMatrix = lsh.occurrenceMatrix(docShinglePair)
    LOG.info("Occurrence matrix (M) is built!")
//    println(M.map(_.mkString(" ")).mkString("\n"))

    LOG.info("Generating hash values for each row is started ...")
    val hashValueMatrix = lsh.generateRowHashValues(universalSet.size, p, n);
    LOG.info("Hash values for each row is completed!")

//    println(hashValueMatrix.map(_.mkString(" ")).mkString("\n"))
    LOG.info("Generating signature matrix is started ...")
    val sig = lsh.generateSignatureMatrix(occurrenceMatrix,hashValueMatrix)
    LOG.info("Signature matrix is built!")

//    println(lsh.generateSignatureMatrix(M,hashValueMatrix).map(_.mkString(" ")).mkString("\n"))
//    println("=====")
    LOG.info("Calculating candidate pairs is started ...")
    val candidatePairs = lsh.lshBandedMatrix(b, r, sig)
    LOG.info("Calculating candidate pairs is complete!")
    val result = lsh.displayCandidatePairs(docShinglePair.map(x => x._1), candidatePairs, occurrenceMatrix)
    LOG.info("Reporting results ...")
    formattedResult(result)
//    println(candidatePairs.map(_.mkString(" ")).mkString("\n"))


  }

}
