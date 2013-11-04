import scala.collection.GenTraversableOnce

  class Mismatched(val kmer:String,val mismatched:String) {
    override def toString: String = kmer + "[" + mismatched + "]"

    override def hashCode(): Int = mismatched.hashCode

    override def equals(obj: scala.Any): Boolean = obj match {
      case obj:Mismatched => obj.mismatched.equals(this.mismatched)
      case _ => false
    }
  }

  def possibleMismatched(kmer: String, mismatchCount: Int): GenTraversableOnce[Mismatched] = {

    for (p <- kmer.zipWithIndex) yield new Mismatched(kmer,kmer.substring(0,p._2) + "*" + kmer.substring(p._2+1))
  }

def allMaxBy[KEY](someMap: Map[KEY, Int], function: ((KEY, Int)) => Int): Iterable[KEY] = {
  return someMap.groupBy(function).maxBy(_._1)._2.keys
}

  val genome = "ACGTTGCA"
  val kmerLength = 2
  val mismatchCount = 1
  println(genome.sliding(kmerLength).toList)
val allPossibleMismatched: List[Mismatched] = genome.sliding(kmerLength).toList.flatMap(kmer => possibleMismatched(kmer, mismatchCount))




val groupped: Map[Mismatched, List[Mismatched]] = allPossibleMismatched.groupBy(identity)



val mapped: Map[Mismatched, Int] = groupped.mapValues(_.size)

allMaxBy(mapped,(p:Pair[Mismatched,Int]) => p._2)





