
package clocks

import org.scalatest.{Matchers, FlatSpec}
import utils.ReadFrom
import org.assertj.core.api.Assertions
import scala.collection.JavaConverters._
import org.apache.commons.lang3.StringUtils

class ProfileMostProbableKMerSpec extends FlatSpec with Matchers with ReadFrom  {

  "most probable kmer" should "be found in genome - example" in {
    val genome = "ACCTGTTTATTGCCTAAGTTCCGAACAAACCCAATATAGCCCGAGGGCCT"
    val kmerLength = 5
    val profile = "0.2 0.4 0.3 0.1\n     0.2 0.3 0.3 0.2\n     0.3 0.1 0.5 0.1\n     0.2 0.5 0.2 0.1\n     0.3 0.1 0.4 0.2".split("\n").map(_.trim).map(_.split(" ").map(_.toDouble).toList).toList

    println(mostProbable(kmerLength,genome,profile))
    Assertions.assertThat(mostProbable(kmerLength,genome,profile)).isEqualTo("CCGAG")
  }

  "most probable kmer" should "be found in genome - extra example" in {
    val genome = "GGTATGCGCACTTCCGAAGAAGGATGCTCAATCATACAAGACACATTCCATCGAGGTAGTTTGACTGGCGAAGTCCCGACTCGCTCACAACTAGTATCCTGTGAAGTCCAGCGTTGAACGACGTGTTGGCTTTAAGCGCCCTGCTTTTCACCAGTTTCTCTCCTAAGTTCGTTCCAGGTCCAAACTGTGGCACTGCAAAT"
    val kmerLength = 7
    val profile = "0.357 0.357 0.179 0.107\n0.393 0.179 0.143 0.286\n0.179 0.179 0.321 0.321\n0.214 0.179 0.321 0.286\n0.286 0.25 0.25 0.214\n0.286 0.25 0.179 0.286\n0.393 0.143 0.214 0.25".split("\n").map(_.trim).map(_.split(" ").map(_.toDouble).toList).toList

    println(mostProbable(kmerLength,genome,profile))
    Assertions.assertThat(mostProbable(kmerLength,genome,profile)).isEqualTo("CATTCCA")
  }

  "most probable kmer" should "be found in genome - test case" in {
    val genome = "ATGGTAAGTACACATTGCTCCAGGCGTGTGGTTCGCTTTGAGTAAGCGCCACAGTATCAAGACTGATGACCGGTCAGATGACCTCCGTTATACCTCTGCTTAAAGGTCTAGATAAATCAGTGGACACACTTTTACCCGCTGGCGGTGGCGGTCAGAGCCATAGGGACCCCGAGAGACTCCCGTTCGGTCAGTTGTCAACG"
    val kmerLength = 7
    val profile = "0.321 0.25 0.214 0.214\n0.143 0.286 0.179 0.393\n0.393 0.357 0.143 0.107\n0.25 0.143 0.214 0.393\n0.143 0.25 0.25 0.357\n0.143 0.25 0.25 0.357\n0.25 0.357 0.179 0.214".split("\n").map(_.trim).map(_.split(" ").map(_.toDouble).toList).toList

    println(mostProbable(kmerLength,genome,profile))
    Assertions.assertThat(mostProbable(kmerLength,genome,profile)).isEqualTo("ACATTGC")
  }

  def mostProbable(kmerLength:Int, genome:String, profile: List[List[Double]]):String = {
    genome.sliding(kmerLength,1).maxBy(probability(_,profile))
  }

  def probability(kmer:String,profile:List[List[Double]]):Double = {
    kmer.zipWithIndex.map((pair:Pair[Char,Int]) => profile(pair._2)(indexOf(pair._1))).reduce(_*_)
  }

  def indexOf(nucleotide:Char) = all.indexOf(nucleotide.toString)

  val all:List[String] = List[String]("A","C","G","T")

}
