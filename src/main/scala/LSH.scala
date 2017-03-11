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

  def findPrime(n: Int): Int = {
    val primes = util.Math.sieve(Stream.from(2))
    primes.take(n).toList.last
  }

  //  def occurrenceMatrix(docShinglePair: List[(String, Set[String])]): Array[Array[Int]] = {
  //    val universalSet = (SortedSet[String]() ++ docShinglePair.map(x => x._2).flatten.toSet).toSeq // n
  //    val occurrenceMatrix = Array.ofDim[Int](universalSet.size, docShinglePair.size)
  //    for(i <- (0 until universalSet.size)) {
  //      for(j <- (0 until docShinglePair.size)) {
  //        if(docShinglePair(j)._2.contains(universalSet(i)))
  //          occurrenceMatrix(i)(j) = 1
  //      }
  //    }
  //    return occurrenceMatrix
  //  }

  def occurrenceMatrix(docShinglePair: List[(String, Set[String])]): Array[Array[Int]] = {
    val universalSet = (SortedSet[String]() ++ docShinglePair.map(x => x._2).flatten.toSet).toSeq
    // n
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
    //    for(r <- (0 until occurrenceMatrix.size)) {
    //      for(c <- (0 until occurrenceMatrix(0).size)) {
    //        if(occurrenceMatrix(r)(c) == 1) {
    //          for(i <- (0 until hashValueMatrix(0).size)) {
    //            sig(i)(c) = Math.min(sig(i)(c), hashValueMatrix(r)(i))
    //          }
    //        }
    //      }
    //    }
    return sig
  }

  def lshBandedMatrix(band: Int, row: Int, minhashMatrix: Array[Array[Int]]): Array[Array[Int]] = {
    //    val bandedSigMat = Array.ofDim[Int](b, minhashMatrix(0).size)
    val docBandedSig = minhashMatrix.sliding(row, row).toList
    val candidatePairs = Array.fill(minhashMatrix(0).size, minhashMatrix(0).size)(0)

    for(b <- (0 until docBandedSig.size)) {
      val buckets =  new HashMap[Int, mutable.Set[Int]] with MultiMap[Int, Int]
      (0 until minhashMatrix(0).size).foreach { d =>
        val bandedRows = docBandedSig(b).map(r => r(d)).toList
//        println("Banded rows: " + bandedRows)
        buckets.addBinding(bandedRows.hashCode(), d)
      }
//      println("buckets: " + buckets)
//      println(buckets.values.toList)
      for(pairs <- buckets.values.toList) { // {12: [1,4, 2], 46: [3,5]}
//        println(pairs)
        for(i <- (0 until pairs.size)) {
          for(j <- (0 until pairs.size))
            candidatePairs(pairs.toList(i))(pairs.toList(j)) = 1
//            println(pairs.toList(i) + "->" + pairs.toList(j))
        }
//        for(pair <- pairs) {
//          for(i <- (0 until pair.))
//          }
//        println()
        }



//      println(docBandedSig.map(_.mkString(",")))
//      println(docBandedSig.hashCode())
    }
//    (0 until minhashMatrix.size by row).foreach { b =>
//      (0 until minhashMatrix(0).size).foreach { d =>
//        (b until b + row).foreach { r =>
//
//          println(docBandedSig.hashCode())
//          println(docBandedSig.toList.map(_.mkString(",")))
//          //        println(i + "," + j)
//        }
//      }
    return candidatePairs
  }

  def displayCandidatePairs(docNames: List[String], candidatePairs: Array[Array[Int]], occurenceMatrix: Array[Array[Int]]): ListBuffer[(String, String, Double)] = {
    val result : ListBuffer[(String, String, Double)] = ListBuffer()
    for(i <- (0 until candidatePairs.size)) {
      for (j <- (0 until i)) {
        if (j != i) {
          if (candidatePairs(i)(j) == 1) {
            val pairI = occurenceMatrix.map(_(i))
            val pairJ = occurenceMatrix.map(_(j))
            result += ((docNames(i),docNames(j),util.Math.jaccardSimilarity(pairI, pairJ)))
          }
        }
      }
    }
    return result.sortWith(_._3 > _._3)
  }

}
