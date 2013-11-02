import org.apache.commons.lang3.StringUtils

class Occurences {

  def allOccurences(genome: String, pattern: String): Iterator[Int] = {
    genome.sliding(pattern.length).zipWithIndex.filter(p => pattern.equals(p._1))
      .collect {case a:(String,Int) => a._2}
  }

  def allMismatchedOccurences(genome: String, pattern: String, mismatchCount: Int): Iterator[Int] = {
    genome.sliding(pattern.length).zipWithIndex.filter(matchingPattern(pattern, mismatchCount))
      .collect {case a:(String,Int) => a._2}
  }


  def matchingPattern(pattern: String, mismatchCount: Int): ((String, Int)) => Boolean = {
    (p: Pair[String, Int]) => {
      p._1.zip(pattern).count((p:Pair[Char,Char]) => p._1 != p._2) <= mismatchCount
    }
  }
}
