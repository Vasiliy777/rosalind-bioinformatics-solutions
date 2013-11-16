
package proteins

import org.scalatest.{Matchers, FlatSpec}
import utils.ReadFrom
import org.assertj.core.api.Assertions
import scala.collection.JavaConverters._
import scala.collection.GenSeq

class SpectrumWithErrorsSpec extends FlatSpec with Matchers with ReadFrom  {

  def originalMasses = List(57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186).par

  "Peptides" should "be found for spectrum with errors - example" in {
    val boardSize = 10
    val spectrum = "0 71 113 129 147 200 218 260 313 331 347 389 460".split(" ").map(_.toInt).toList

    println(peptidesForSpectrum(spectrum))
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
    if (candidate.isSubspectrum(spectrum)) {
      if (candidate.isSpectrum(spectrum)) List(candidate)
      else if (candidate.sum<spectrum.last) possiblePeptides(spectrum, originalMasses.map((a:Int) => candidate.append(a)))
      else List()
    } else List()
  }


}
