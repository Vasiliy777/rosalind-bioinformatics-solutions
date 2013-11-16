package proteins

class CandidateWithScore(val localMasses:List[Int],val score:Int) extends Candidate(localMasses) {
  def append(mass:Int,spectrum:List[Int]):CandidateWithScore = new CandidateWithScore(masses :+ mass,calculateScore(masses :+ mass,spectrum))

  override def toString: String = localMasses.mkString("-") + " > " + score

  override def hashCode(): Int = localMasses.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match { case a:CandidateWithScore => localMasses.equals(a.localMasses); case _ => false }

  def calculateScore(candidate:List[Int],spectrum:List[Int]):Int = {
    if (candidate.sum > spectrum.sorted.last) -1 else  spectrum.intersect((for (i <- 1 to candidate.length; combination <- candidate.union(candidate).sliding(i)) yield(combination.sum))).size
  }
}