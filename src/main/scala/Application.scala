import java.io._
import java.util.Properties
import java.util.logging.Logger

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

    val properties = new Properties
    properties.load(new FileReader(new File(args(0))))

    val preprocess = new Preprocess
    val lsh = new LSH
    LOG.info("Reading input files ...")
    val docShinglePair = lsh.getShingles(properties.getProperty("input.dir"), preprocess, lsh)
    LOG.info("Files are read!")

    val b = properties.getProperty("number.of.bands").toInt
    val r = properties.getProperty("rows.in.bands").toInt
    val n = b*r
    val p = properties.getProperty("prime.number").toInt


    LOG.info("Universal set is sorting ...")
    val universalSet = SortedSet[String]() ++ docShinglePair.map(x => x._2).flatten.toSet // n
    LOG.info("Universal set size: " + universalSet.size)
    LOG.info("Used prime number: " + p)
    LOG.info("Number of bands: " + b)
    LOG.info("Number of rows in each band: " + r)
    LOG.info("Number of hash functions: " + b*r)

    var occurrenceMatrix: Array[Array[Int]] = null
    if (properties.getProperty("occurrence.mat") != "") {
      LOG.info("Deserializing occurrence matrix ...")
      val ois = new ObjectInputStream(new FileInputStream(properties.getProperty("occurrence.mat")))
      occurrenceMatrix = ois.readObject.asInstanceOf[Array[Array[Int]]]
      ois.close()
      LOG.info("Occurrence matrix deserialized!")
    }
    else {
      LOG.info("Building occurrence matrix (M) is started ...")
      occurrenceMatrix = lsh.occurrenceMatrix(docShinglePair)
      LOG.info("Occurrence matrix (M) is built!")
      LOG.info("Serializing occurrence matrix ...")
      val oos = new ObjectOutputStream(new FileOutputStream("occurrence_mat.dat"))
      oos.writeObject(occurrenceMatrix)
      oos.close()
      LOG.info("Serialization is complete!")
    }

    LOG.info("Generating hash values for each row is started ...")
    val hashValueMatrix = lsh.generateRowHashValues(universalSet.size, p, n);
    LOG.info("Hash values for each row is completed!")

    LOG.info("Generating signature matrix is started ...")
    val sig = lsh.generateSignatureMatrix(occurrenceMatrix,hashValueMatrix)
    LOG.info("Signature matrix is built!")

    LOG.info("Calculating candidate pairs is started ...")
    val candidatePairs = lsh.lshBandedMatrix(b, r, sig)
    LOG.info("Calculating candidate pairs is complete!")

    LOG.info("Building candidate pairs adjacency matrix ...")
    val result = lsh.buildCandidatePairsAdjMatrix(docShinglePair.map(x => x._1), candidatePairs, occurrenceMatrix)
    LOG.info("Candidate pairs adjacency matrix is built!")

    LOG.info("Reporting results ...")
    formattedResult(result)
  }

}
