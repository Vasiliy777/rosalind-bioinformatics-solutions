import org.scalatest._
import scala.collection.GenTraversableOnce


class ClumpsWithMismatchAndReverseComplementSpec extends FlatSpec with Matchers {

  "simple reversed mers" should "be found" in {
    val genome = "ATCGGAT"
    val kmerLength = 3
    val mismatchCount = 0

    val mostFrequentMismatchedKmers = mostFrequentWithComplement(genome,kmerLength,mismatchCount)
    assert(mostFrequentMismatchedKmers === Set("ATC", "GAT"))
  }

  "simple reversed and mismatched mers" should "be found" in {
    val genome = "ATCGGTT"
    val kmerLength = 3
    val mismatchCount = 1

    val mostFrequentMismatchedKmers = mostFrequentWithComplement(genome,kmerLength,mismatchCount)
    assert(mostFrequentMismatchedKmers === Set("ATC", "GAT"))
  }

  "example mismatched mers" should "be found" in {
    val genome = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
    val kmerLength = 4
    val mismatchCount = 1

    val mostFrequentMismatchedKmers = mostFrequentWithComplement(genome,kmerLength,mismatchCount)
    assert(mostFrequentMismatchedKmers === Set("ATGT", "ACAT"))
  }

  ignore should "sbe found" in {
    val genome = "CTTGCCGGCGCCGATTATACGATCGCGGCCGCTTGCCTTCTTTATAATGCATCGGCGCCGCGATCTTGCTATATACGTACGCTTCGCTTGCATCTTGCGCGCATTACGTACTTATCGATTACTTATCTTCGATGCCGGCCGGCATATGCCGCTTTAGCATCGATCGATCGTACTTTACGCGTATAGCCGCTTCGCTTGCCGTACGCGATGCTAGCATATGCTAGCGCTAATTACTTAT"
    val kmerLength = 9
    val mismatchCount = 3

    val mostFrequentMismatchedKmers = mostFrequentWithComplement(genome,kmerLength,mismatchCount)
    assert(mostFrequentMismatchedKmers === Set("AGCGCCGCT", "AGCGGCGCT"))
  }

  class Mismatched(val kmer:String,val reverse:Boolean) {
    override def toString(): String = kmer +"/" +revCompliment(kmer)

    override def hashCode(): Int = Math.abs(kmer.hashCode - revCompliment(kmer).hashCode)

    override def equals(obj: scala.Any): Boolean = obj match {
      case obj:Mismatched => obj.kmer.equals(this.kmer) || obj.kmer.equals(revCompliment(this.kmer))
      case _ => false
    }
  }

  def mostFrequentWithComplement(genome: String, kmerLength: Int, mismatchCount: Int) = {
    val allMismatched: Iterator[Mismatched] =
      for {
        kmer: String <- genome.sliding(kmerLength)
        _ = println(kmer)
        mismatched: Mismatched <- possibleMismatched(kmer, mismatchCount)
        _ = println(mismatched)
      } yield mismatched
    allMismatched.map(_ => 0)

    println("-----")
    val allPossibleMismatched: List[Mismatched] = genome.sliding(kmerLength).toList.flatMap(kmer => possibleMismatched(kmer, mismatchCount))
    println(allPossibleMismatched)
    val groupped: Map[Mismatched, List[Mismatched]] = allPossibleMismatched.groupBy(identity)
    println(groupped)
    val maxBy: Iterable[Mismatched] = allMaxBy(groupped,(p:Pair[Mismatched,List[Mismatched]]) => p._2.size)
    println(maxBy)
    val allMaxed: Map[Mismatched, List[Mismatched]] = groupped.filterKeys(maxBy.toList.contains(_))
    println(allMaxed)
    val result = allMaxed.collect { case entry:Pair[_,List[Mismatched]] => entry._2}.flatten
    println(result)
    result.collect { case m:Mismatched => m.kmer }.toSet
  }

  def substitute(string: String, index: Int, toSubstitute: Char): String = string.substring(0,index) + toSubstitute + string.substring(index+1)

  def possibleMismatchedRec(mismatched: List[Mismatched], mismatchCount: Int): List[Mismatched] = {
    if(mismatchCount == 0) {
      mismatched
    } else for (kmer:Mismatched <- mismatched; p <- kmer.kmer.zipWithIndex; nuc <- List('A','T','C','G');a <- possibleMismatchedRec(List(new Mismatched(substitute(kmer.kmer,p._2,nuc),false)),mismatchCount-1)) yield a
  }

  def possibleMismatched(kmer: String, mismatchCount: Int): Set[Mismatched] = {
    possibleMismatchedRec(List(new Mismatched(kmer,false)),mismatchCount).toSet
  }

  def revCompliment(genome: String) = {
    for (nuc <- genome.reverse) yield reverseNucleotide(nuc)
  }

  def reverseNucleotide(c: Char): Char = {
    c match {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
    }
  }

  def allMaxBy[KEY,VALUE](someMap: Map[KEY, VALUE], function: ((KEY, VALUE)) => Int): Iterable[KEY] = {
    return someMap.groupBy(function).maxBy(_._1)._2.keys
  }





}


