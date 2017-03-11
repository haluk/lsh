import scala.collection.mutable

//def hashFunction(p: Int) = (x: Int) => {
//  val r = scala.util.Random
//  val a = r.nextInt(p)
//  val b = r.nextInt(p)
//  println(a)
//  println(b)
//
//  (a*x + b) % p
//}
//
//val hasha = hashFunction(5)
//
//val hashbucket = (x: List[Int]) => x.map(i => i*3).reduce(_+_)+4
//
//var buckets = new mutable.HashMap[List[Int], Int]()
val c1 = List(1, 3, 0)
val c2 = List(0, 2, 1)
val c3 = List(0, 1, 3)
val c4 = List(0, 2, 1)
val c5 = List(2, 2, 1)

//println(c1.hashCode())
//println(c2.hashCode())
//println(c3.hashCode())
//println(c4.hashCode())
//println(c5.hashCode())

"""
  |c2 c4 bucket 3 te (row 1)
  | if (result.get(hash(c2 veya c4)) == null
  |   result.put(hash(c2 veya c4), [2,4])
  | if (result.get(hash(c2 veya c4)) == null
  |
""".stripMargin


//(0 to 100).sliding(25,25).mkString("\n")

val tmp = Array(c1,c2,c3,c4,c5)

println(tmp.map(_.mkString(" ")).mkString("\n"))


println(tmp.sliding(2,2).toArray.map(_.toList.map(_(1)).mkString(",")).mkString("\n"))

List(3,1).permutations.mkString(",")

Set(1).size
Set(2,3).size

for (i <- (0 until Set(1).size)){
  println(i)
}

val lst1 = List(1,0,1,1,1)
val lst2 = List(1,0,0,1,1)

val union = lst1.zip(lst2).filterNot(_ == (0,0))
val intersection = union.filter{case (x,y) => x == y}

val jaccard = intersection.size / union.size.toDouble