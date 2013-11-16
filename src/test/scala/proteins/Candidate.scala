package proteins

class Candidate(val masses:List[Int]) {
  def sum = masses.sum
  def length = masses.length
  def sliding(size:Int):Iterator[List[Int]] = masses.sliding(size)
  def cyclicSliding(size:Int):Iterator[List[Int]] = masses.union(masses).sliding(size)
  def append(mass:Int):Candidate = new Candidate(masses :+ mass)
  def asList = masses
  def isSubspectrum(spectrum:List[Int]):Boolean = sum<=spectrum.last &&
    (for (i <- 1 to length; combination <- sliding(i)) yield(combination.sum)).toSet.subsetOf(spectrum.toSet)
  def isSpectrum(spectrum:List[Int]):Boolean = {
    (sum == spectrum.last) && {
      val all = (for (i <- 1 to length; combination <- cyclicSliding(i)) yield(combination.sum)).toSet
      all.subsetOf(spectrum.toSet) && spectrum.toSet.subsetOf(all.toSet)
    }
  }
}