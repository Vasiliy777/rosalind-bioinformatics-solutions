import org.scalatest._
import scala.collection.immutable.IndexedSeq
import scala.collection.{immutable, GenTraversableOnce}


class ClumpsWithMismatchFindingSpec extends FlatSpec with Matchers {

  "example mismatched mers" should "be found" in {
    val genome = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
    val kmerLength = 4
    val mismatchCount = 1

    val mostFrequentMismatchedKmers = mostFrequent(genome,kmerLength,mismatchCount)
    assert(mostFrequentMismatchedKmers === Set("GATG", "ATGC", "ATGT"))
  }

  "simple example mismatched mers" should "be found" in {
    val genome = "AACAAGCTGATAAACATTTAAAGAG"
    val kmerLength = 5
    val mismatchCount = 1

    val mostFrequentMismatchedKmers = mostFrequent(genome,kmerLength,mismatchCount)
    assert(mostFrequentMismatchedKmers === Set("AAAAA"))
  }

  "simpler example mismatched mers" should "be found" in {
    val genome = "CAAGA"
    val kmerLength = 3
    val mismatchCount = 1

    val mostFrequentMismatchedKmers = mostFrequent(genome,kmerLength,mismatchCount)
    assert(mostFrequentMismatchedKmers === Set("AAA"))
  }

  "extra example mismatched mers" should "be found" in {
    val genome = "CACAGTAGGCGCCGGCACACACAGCCCCGGGCCCCGGGCCGCCCCGGGCCGGCGGCCGCCGGCGCCGGCACACCGGCACAGCCGTACCGGCACAGTAGTACCGGCCGGCCGGCACACCGGCACACCGGGTACACACCGGGGCGCACACACAGGCGGGCGCCGGGCCCCGGGCCGTACCGGGCCGCCGGCGGCCCACAGGCGCCGGCACAGTACCGGCACACACAGTAGCCCACACACAGGCGGGCGGTAGCCGGCGCACACACACACAGTAGGCGCACAGCCGCCCACACACACCGGCCGGCCGGCACAGGCGGGCGGGCGCACACACACCGGCACAGTAGTAGGCGGCCGGCGCACAGCC"
    val kmerLength =  10
    val mismatchCount = 2

    val mostFrequentMismatchedKmers = mostFrequent(genome,kmerLength,mismatchCount)
    assert(mostFrequentMismatchedKmers === Set("GCACACAGAC", "GCGCACACAC"))
  }

  "test case 1" should "be found" in {
    val genome = "CCCGTCACCCGTCCCCCTCATCATCTTCTCCCCGTCATTCTCCCTCACCCCCCCGCCTCCCCGTCCCTTCCCCCTCTCATCACCCCTCATCTCCCCGTCATCACCCGCCCGCCCCCGCCCGCCCGTTCCCCGTTCCCCCCGCCCCTCATCCCCGTTCTCATTCCCCGTCTTCCCCGCCCCCGTTCCCTCTCACCCGTCCCCGCCCGTCTTCTCTCATTCCCCGCCTCCCCCTCACCCGTTCCCTCACCCGCCCGTCTCTCTTCCCCGCCCCTCTCCCTTCCCCGTTCTCTCACCTCACCCGTCTTCTCTTCCCCGCCCGCC"
    val kmerLength =  10
    val mismatchCount = 3

    val mostFrequentMismatchedKmers = mostFrequent(genome,kmerLength,mismatchCount)
    println(mostFrequentMismatchedKmers.mkString(" "))
    assert(mostFrequentMismatchedKmers !== Set("CCCCCCCCCC"))
  }

  "test case 2" should "be found" in {
    val genome = "TGCATGCATGCGTCTGCAGGTAGGTAGGTAGTCTATGCAGTCGGTATGCAGTCTATGCGGTAGGTATGCATGCTATGCTAGTCTGCTGCATGCAGTCGGTATGCATGCGTCTAGTCGGTATGCGGTAGGTATGCTAGGTAGTCTGCGGTAGTCTGCAGTCTAGTCTGCTGCAGGTAGTCGTCGGTATGCAGTCTGCTGCTGCATGCAGTCTGCTGCATGCTGCGTCTGCTGCTGCATAGGTATATGCTATGCGTCGGTAGGTAGGTATGCATGCGTCGGTATGCTGCAGGTATATGCATGCTGCAGTCTGCGGTATGCGGTATGCA"
    val kmerLength =  9
    val mismatchCount = 3

    val mostFrequentMismatchedKmers = mostFrequent(genome,kmerLength,mismatchCount)
    println(mostFrequentMismatchedKmers.mkString(" "))
    assert(mostFrequentMismatchedKmers !== Set("CCCCCCCCCC"))
  }

  "simple mismatched mers" should "be found" in {
    val genome = "GCGT"
    val kmerLength = 2
    val mismatchCount = 1

    val mostFrequentMismatchedKmers = mostFrequent(genome,kmerLength,mismatchCount)
    assert(mostFrequentMismatchedKmers === Set("GG"))
  }

  class Mismatched(val kmer:String) {
    override def toString(): String = kmer

    override def hashCode(): Int = kmer.hashCode

    override def equals(obj: scala.Any): Boolean = obj match {
      case obj:Mismatched => obj.kmer.equals(this.kmer)
      case _ => false
    }
  }

  def mostFrequent(genome: String, kmerLength: Int, mismatchCount: Int) = {
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
    } else for (kmer:Mismatched <- mismatched; p <- kmer.kmer.zipWithIndex; nuc <- List('A','T','C','G');a <- possibleMismatchedRec(List(new Mismatched(substitute(kmer.kmer,p._2,nuc))),mismatchCount-1)) yield a
  }

  def possibleMismatched(kmer: String, mismatchCount: Int): GenTraversableOnce[Mismatched] = {
    possibleMismatchedRec(List(new Mismatched(kmer)),mismatchCount).toSet
  }

  def allMaxBy[KEY,VALUE](someMap: Map[KEY, VALUE], function: ((KEY, VALUE)) => Int): Iterable[KEY] = {
    return someMap.groupBy(function).maxBy(_._1)._2.keys
  }





}


