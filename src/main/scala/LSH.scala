import java.io.File
import java.nio.charset.{Charset, CodingErrorAction}

import preprocess.Preprocess

import scala.collection.{SortedSet, mutable}
import collection.mutable.{HashMap, ListBuffer, MultiMap}

/**
  * Created by hd on 2/27/17.
  */
class LSH {

  def constructShingles(document: String): Set[String] = {
    return document.split(" ").toSet
  }

  def getShingles(dir: String, preprocess: Preprocess, lsh: LSH): List[(String, Set[String])] = {
    val d = new File(dir)
    val decoder = Charset.forName("UTF-8").newDecoder()
    decoder.onMalformedInput(CodingErrorAction.IGNORE)
    if (d.exists && d.isDirectory) {
      return d.listFiles.filter(_.isFile)
        .toList.map(f => (f.getName, lsh.constructShingles(preprocess.process(scala.io.Source.fromFile(f)(decoder).mkString))))

    }
    return null;
  }

  def occurrenceMatrix(docShinglePair: List[(String, Set[String])]): Array[Array[Int]] = {
    val universalSet = (SortedSet[String]() ++ docShinglePair.map(x => x._2).flatten.toSet).toSeq
    val occurrenceMatrix = Array.ofDim[Int](universalSet.size, docShinglePair.size)

    universalSet.foreach { i =>
      docShinglePair.par.foreach { j =>
        if (j._2.contains(i))
          occurrenceMatrix(universalSet.indexOf(i))(docShinglePair.indexOf(j)) = 1
      }
    }

    return occurrenceMatrix
  }

  def generateRowHashValues(row: Int, p: Int, numberOfHashFunction: Int): Array[Array[Int]] = {
    val hashFunctions = Array.ofDim[(Int, Int)](numberOfHashFunction)
    for (i <- (0 until numberOfHashFunction))
      hashFunctions(i) = util.Math.hashFunction(p)
    val hashValueMatrix = Array.ofDim[Int](row, numberOfHashFunction)
    (0 until row).par.foreach { i =>
      (0 until numberOfHashFunction).foreach { j =>
        hashValueMatrix(i)(j) = (hashFunctions(j)._1 * i + hashFunctions(j)._2) % p
      }
    }
    return hashValueMatrix
  }

  def generateSignatureMatrix(occurrenceMatrix: Array[Array[Int]], hashValueMatrix: Array[Array[Int]]): Array[Array[Int]] = {
    val sig = Array.fill(hashValueMatrix(0).size, occurrenceMatrix(0).size)(Int.MaxValue)
    (0 until occurrenceMatrix.size).par.foreach { r =>
      (0 until occurrenceMatrix(0).size).par.foreach { c =>
        if (occurrenceMatrix(r)(c) == 1) {
          (0 until hashValueMatrix(0).size).foreach { i =>
            sig(i)(c) = Math.min(sig(i)(c), hashValueMatrix(r)(i))
          }
        }
      }
    }

    return sig
  }

  def lshBandedMatrix(band: Int, row: Int, minhashMatrix: Array[Array[Int]]): Array[Array[Int]] = {
    val docBandedSig = minhashMatrix.sliding(row, row).toList
    val candidatePairs = Array.fill(minhashMatrix(0).size, minhashMatrix(0).size)(0)

    for (b <- (0 until docBandedSig.size)) {
      val buckets = new HashMap[Int, mutable.Set[Int]] with MultiMap[Int, Int]
      (0 until minhashMatrix(0).size).foreach { d =>
        val bandedRows = docBandedSig(b).map(r => r(d)).toList
        buckets.addBinding(bandedRows.hashCode(), d)
      }
      for (pairs <- buckets.values.toList) {
        for (i <- (0 until pairs.size)) {
          for (j <- (0 until pairs.size))
            candidatePairs(pairs.toList(i))(pairs.toList(j)) = 1
        }
      }
    }

    return candidatePairs
  }

  def buildCandidatePairsAdjMatrix(docNames: List[String], candidatePairs: Array[Array[Int]], occurenceMatrix: Array[Array[Int]], thr: Double): ListBuffer[(String, String, Double)] = {
    val result: ListBuffer[(String, String, Double)] = ListBuffer()
    (0 until candidatePairs.size).par.foreach { i =>
      for (j <- (0 until i)) {
        if (j != i) {
          if (candidatePairs(i)(j) == 1) {
            val pairI = occurenceMatrix.map(_ (i))
            val pairJ = occurenceMatrix.map(_ (j))
            val sim = util.Math.jaccardSimilarity(pairI, pairJ)
            if (sim >= thr)
            result += ((docNames(i), docNames(j), sim))
          }
        }
      }
    }

    return result
  }
}
