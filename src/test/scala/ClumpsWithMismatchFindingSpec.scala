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

  def mostFrequent(genome: String, kmerLength: Int, mismatchCount: Int) = {
    val allPossibleMismatched: List[String] = genome.sliding(kmerLength).toList.flatMap(kmer => possibleMismatched(kmer, mismatchCount))
    val groupped: Map[String, List[String]] = allPossibleMismatched.groupBy(identity)
    val maxBy: Iterable[String] = allMaxBy(groupped,(p:Pair[String,List[String]]) => p._2.size)
    val allMaxed: Map[String, List[String]] = groupped.filterKeys(maxBy.toList.contains(_))
    val result = allMaxed.collect { case entry:Pair[_,List[String]] => entry._2}.flatten
    result.toSet
  }

  def substitute(string: String, index: Int, toSubstitute: Char): String = string.substring(0,index) + toSubstitute + string.substring(index+1)

  def possibleMismatchedRec(mismatched: List[String], mismatchCount: Int): List[String] = {
    if(mismatchCount == 0) {
      mismatched
    } else for (kmer:String <- mismatched; p <- kmer.zipWithIndex; nuc <- List('A','T','C','G');a <- possibleMismatchedRec(List(substitute(kmer,p._2,nuc)),mismatchCount-1)) yield a
  }

  def possibleMismatched(kmer: String, mismatchCount: Int): GenTraversableOnce[String] = {
    possibleMismatchedRec(List(kmer),mismatchCount).toSet
  }

  def allMaxBy[KEY,VALUE](someMap: Map[KEY, VALUE], function: ((KEY, VALUE)) => Int): Iterable[KEY] = {
    return someMap.groupBy(function).maxBy(_._1)._2.keys
  }





}


