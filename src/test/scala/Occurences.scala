class Occurences {

  def allOccurences(genome: String, pattern: String): Iterator[Int] = {
    genome.sliding(pattern.length).zipWithIndex.filter(p => pattern.equals(p._1))
      .collect {case a:(String,Int) => a._2}
  }

}
