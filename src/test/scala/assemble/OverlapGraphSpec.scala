package assemble

import org.scalatest._
import utils.ReadFrom
import org.assertj.core.api
import scala.collection.JavaConverters._
import java.util
import scala.collection.JavaConverters._

class OverlapGraphSpec extends FlatSpec with Matchers with ReadFrom {

  "graph" should "be calculated" in {
    val kmers="ATGCG\n     GCATG\n     CATGC\n     AGGCA\n     GGCAT"
    val kmerList = kmers.split("\n").map(_.trim)


    val graph = createGraph(kmerList.toList).sorted
    val expected = "AGGCA -> GGCAT\n     CATGC -> ATGCG\n     GCATG -> CATGC\n     GGCAT -> GCATG".split("\n").map(_.trim).toList
    api.Assertions.assertThat(graph).isEqualTo(expected)
  }

  "graph" should "be calculated - extra set" in {
    val kmerList = readAsList("overlapGraph_inp.txt")


    val graph = createGraph(kmerList.toList).sorted
    val expected = readAsList("overlapGraph_out.txt")
    api.Assertions.assertThat(graph.head).isEqualTo(expected.head)
    api.Assertions.assertThat(graph.toList.asJava).containsAll(expected.toList.asJava)
  }
  "graph" should "be calculated - testcase" in {
    val kmerList = readAsList("overlapGraph_test_inp.txt")

    val graph = createGraph(kmerList.toList).sorted
    val expected = readAsList("overlapGraph_test_out.txt")
    api.Assertions.assertThat(graph.toList.asJava).containsAll(expected.toList.asJava)
  }

  def createGraph(kmerList:List[String]) = {
    for (kmer <- kmerList; other <- kmerList.filterNot(_.equals(kmer)); if (kmer.drop(1).equals(other.dropRight(1)))) yield kmer + " -> " + other
  }

}


