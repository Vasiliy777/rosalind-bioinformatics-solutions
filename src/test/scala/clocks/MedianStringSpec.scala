
package clocks

import org.scalatest.{Matchers, FlatSpec}
import utils.ReadFrom
import org.assertj.core.api.Assertions
import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import org.apache.commons.lang3.StringUtils

class MedianStringSpec extends FlatSpec with Matchers with ReadFrom  {

  "median string" should "be found in genome - example" in {
    val kmerLength = 3
    val dnas = "AAATTGACGCAT\n     GACGACCACGTT\n     CGTCAGCGCCTG\n     GCTGAGCACCGG\n     AGTACGGGACAG".split("\n").map(_.trim).toList

    println(medianString(kmerLength,dnas).mkString(" "))
    Assertions.assertThat(medianString(kmerLength,dnas).asJava).containsOnlyOnce("GAC".split(" "):_*)
  }

  "median string" should "be found in genome - extra example" in {
    val kmerLength = 6
    val dnas = "TGATGATAACGTGACGGGACTCAGCGGCGATGAAGGATGAGT\nCAGCGACAGACAATTTCAATAATATCCGCGGTAAGCGGCGTA\nTGCAGAGGTTGGTAACGCCGGCGACTCGGAGAGCTTTTCGCT\nTTTGTCATGAACTCAGATACCATAGAGCACCGGCGAGACTCA\nACTGGGACTTCACATTAGGTTGAACCGCGAGCCAGGTGGGTG\nTTGCGGACGGGATACTCAATAACTAAGGTAGTTCAGCTGCGA\nTGGGAGGACACACATTTTCTTACCTCTTCCCAGCGAGATGGC\nGAAAAAACCTATAAAGTCCACTCTTTGCGGCGGCGAGCCATA\nCCACGTCCGTTACTCCGTCGCCGTCAGCGATAATGGGATGAG\nCCAAAGCTGCGAAATAACCATACTCTGCTCAGGAGCCCGATG".split("\n").map(_.trim).toList

    val medians = medianString(kmerLength,dnas)
    println(medians.mkString(" "))
    Assertions.assertThat(medians.asJava).containsOnlyOnce("CGGCGA".split(" "):_*)
  }

  "median string" should "be found in genome - test case" in {
    val kmerLength = 6
    val dnas = "TTCCGGGCGCGTGTGGTGTTAATTGCCGAGGTTATAGTTTCA\nTCGTGACTAACACTCCGGATACCTATCGGGTTACTGAGGCGA\nCTCCGGATGCATCTAGTCGCCAGTGCAGGACCGGTCAAGATC\nACATTGGTGATACCTAACCCAAATTTCCGGAACAGAACATAC\nGAAGCCGTCCGGCGCCTAGCCCCAGATGTTGTGAACTGATCT\nCGATCCAGGCGAAAGTGTCTACCGATCCGGGCCCAGTCGTAC\nGCTACTAATGCAATCCGGAAAAAATGGAGGCTCACTTAGCTC\nCGGCAGATGTGTTCCTATGTCCGGACTCGCACACAGGAATTA\nGGAAATCTCCGGAGGGGCTAACTCAGAAGTCAGGGGTAGCCC\nGACAAACAGTACAGGGCGGCAGCTAGTAACAGTGGGATCCGG".split("\n").map(_.trim).toList

    val medians = medianString(kmerLength,dnas)
    println(medians.mkString(" "))
    Assertions.assertThat(medians.asJava).containsOnlyOnce("TCCGGA".split(" "):_*)
  }

  "combinations" should "be generated" in {
    Assertions.assertThat(combinations(all,0).asJava).containsOnlyOnce("A T C G".split(" "):_*)
    Assertions.assertThat(combinations(all,1).asJava).containsOnlyOnce("AA TA CA GA AT TT CT GT AC TC CC GC AG TG CG GG".split(" "):_*)
  }

  val all = List("A","T","C","G")

  def medianString(kmerLength:Int, dnas:List[String]):Iterable[String] = {
      val comb:List[String] = combinations(all,kmerLength-1)
      val maxElement = comb.minBy((combination: String) => dnasScore(dnas, combination))
      val topScore = dnasScore(dnas,maxElement)
      comb.filter(dnasScore(dnas,_) == topScore)
  }

  def combinations(current:List[String],howMany:Int):List[String] ={
    if (howMany == 0) current else (for (c <- current; base <- all) yield combinations(List(c + base),howMany-1)).flatten
  }

  def dnasScore(dnas:List[String], combination:String):Int = {
//    println("dnas for " + combination + " is = " + dnas.map(dnaScore(_,combination)).sum)
    dnas.map(dnaScore(_,combination)).sum
  }

  def dnaScore(dna:String,combination:String):Int = {
//    println("dna score for " + dna + " : " + combination + " is " + dna.sliding(combination.length,1).map(StringUtils.getLevenshteinDistance(combination,_)).min)
      dna.sliding(combination.length,1).map(StringUtils.getLevenshteinDistance(combination,_)).min
  }

}
