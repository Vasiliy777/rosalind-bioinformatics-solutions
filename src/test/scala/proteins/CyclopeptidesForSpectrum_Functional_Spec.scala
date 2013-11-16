
package proteins

import org.scalatest.{Matchers, FlatSpec}
import utils.ReadFrom
import org.assertj.core.api.Assertions
import scala.collection.JavaConverters._
import scala.collection.GenSeq

class CyclopeptidesForSpectrum_Functional_Spec extends FlatSpec with Matchers with ReadFrom  {

  def originalMasses = List(57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186).par

  "Peptides" should "be found for spectrum -example" in {
    val spectrum = List(0 ,113 ,128 ,186 ,241 ,299 ,314 ,427)

    Assertions.assertThat(peptidesForSpectrum(spectrum).toList.asJava).containsOnlyOnce(
      List(186,128,113), List(186,113,128), List(128,186,113), List(128,113,186), List(113,186,128), List(113,128,186)
    )
  }
  
  "Peptides" should "be found for spectrum - extra example" in {
    val spectrum = List(0, 71, 97, 99, 103, 113, 113, 114, 115, 131, 137, 196, 200, 202, 208, 214, 226, 227, 228, 240, 245, 299, 311, 311, 316, 327, 337, 339, 340, 341, 358, 408, 414, 424, 429, 436, 440, 442, 453, 455, 471, 507, 527, 537, 539, 542, 551, 554, 556, 566, 586, 622, 638, 640, 651, 653, 657, 664, 669, 679, 685, 735, 752, 753, 754, 756, 766, 777, 782, 782, 794, 848, 853, 865, 866, 867, 879, 885, 891, 893, 897, 956, 962, 978, 979, 980, 980, 990, 994, 996, 1022, 1093)

    val expectedString: String = "103-137-71-131-114-113-113-115-99-97 103-97-99-115-113-113-114-131-71-137 113-113-114-131-71-137-103-97-99-115 113-113-115-99-97-103-137-71-131-114 113-114-131-71-137-103-97-99-115-113 113-115-99-97-103-137-71-131-114-113 114-113-113-115-99-97-103-137-71-131 114-131-71-137-103-97-99-115-113-113 115-113-113-114-131-71-137-103-97-99 115-99-97-103-137-71-131-114-113-113 131-114-113-113-115-99-97-103-137-71 131-71-137-103-97-99-115-113-113-114 137-103-97-99-115-113-113-114-131-71 137-71-131-114-113-113-115-99-97-103 71-131-114-113-113-115-99-97-103-137 71-137-103-97-99-115-113-113-114-131 97-103-137-71-131-114-113-113-115-99 97-99-115-113-113-114-131-71-137-103 99-115-113-113-114-131-71-137-103-97 99-97-103-137-71-131-114-113-113-115"
    Assertions.assertThat(peptidesForSpectrum(spectrum).map(_.mkString("-")).asJava).containsOnlyOnce(expectedString.split(" "):_*)
  }

  "Peptides" should "be found for spectrum - simple" in {
    val spectrum = List(0 ,71, 142)

    Assertions.assertThat(peptidesForSpectrum(spectrum).toList.asJava).containsOnly(
      List(71,71)
    )
  }

  "Peptides" should "be found for spectrum - simplest" in {
    val spectrum = List(0 ,71)

    Assertions.assertThat(peptidesForSpectrum(spectrum).toList.asJava).containsOnly(
      List(71)
    )
  }
  
  "Peptides" should "be found for spectrum - test case" in {
    val spectrum = List(0, 113, 113, 114, 115, 128, 128, 131, 137, 227, 241, 241, 243, 246, 250, 251, 259, 355, 356, 364, 364, 372, 374, 374, 378, 470, 477, 487, 487, 492, 492, 502, 509, 601, 605, 605, 607, 615, 615, 623, 624, 720, 728, 729, 733, 736, 738, 738, 752, 842, 848, 851, 851, 864, 865, 866, 866, 979)
    println(peptidesForSpectrum(spectrum).map(_.mkString("-")).mkString(" "))
    val expected = "113-114-137-113-128-131-115-128 113-128-115-131-128-113-137-114 113-128-131-115-128-113-114-137 113-137-114-113-128-115-131-128 114-113-128-115-131-128-113-137 114-137-113-128-131-115-128-113 115-128-113-114-137-113-128-131 115-131-128-113-137-114-113-128 128-113-114-137-113-128-131-115 128-113-137-114-113-128-115-131 128-115-131-128-113-137-114-113 128-131-115-128-113-114-137-113 131-115-128-113-114-137-113-128 131-128-113-137-114-113-128-115 137-113-128-131-115-128-113-114 137-114-113-128-115-131-128-113"
    Assertions.assertThat(peptidesForSpectrum(spectrum).map(_.mkString("-")).asJava).containsOnly(expected.split(" "):_*)
  }

  def isSubspectrum(candidate:Candidate,spectrum:List[Int]):Boolean = candidate.sum<=spectrum.last &&
    (for (i <- 1 to candidate.length; combination <- candidate.sliding(i)) yield(combination.sum)).toSet.subsetOf(spectrum.toSet)

  def isSpectrum(candidate:Candidate,spectrum:List[Int]):Boolean = {
    (candidate.sum == spectrum.last) && {
      val all = (for (i <- 1 to candidate.length; combination <- candidate.cyclicSliding(i)) yield(combination.sum)).toSet
      all.subsetOf(spectrum.toSet) && spectrum.toSet.subsetOf(all.toSet)
    }
  }

  class Candidate(val masses:List[Int]) {

    def sum = masses.sum
    def length = masses.length
    def sliding(size:Int):Iterator[List[Int]] = masses.sliding(size)
    def cyclicSliding(size:Int):Iterator[List[Int]] = masses.union(masses).sliding(size)//union(candidate).
    def append(mass:Int):Candidate = new Candidate(masses :+ mass)
    def asList = masses
  }

  def peptidesForSpectrum(spectrum:List[Int]):List[List[Int]] = {
    val initialCandidates = originalMasses.map((firstMass:Int) => new Candidate(List(firstMass)))
    possiblePeptides(spectrum.tail,initialCandidates).map(_.asList).toList
  }

  def possiblePeptides(spectrum:List[Int], candidates:GenSeq[Candidate]):GenSeq[Candidate] = {
    (for (candidate <- candidates.par)
        yield peptidesForCandidate(spectrum,candidate)).flatten
  }

  def peptidesForCandidate(spectrum:List[Int],candidate:Candidate)= {
      if (isSubspectrum(candidate,spectrum)) {
        if (isSpectrum(candidate,spectrum)) List(candidate)
        else if (candidate.sum<spectrum.last) possiblePeptides(spectrum, originalMasses.map((a:Int) => candidate.append(a)))
        else List()
      } else List()

  }

}
