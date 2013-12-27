package assemble

import org.scalatest._
import utils.ReadFrom
import org.assertj.core.api
import scala.collection.JavaConverters._

class DeBruijnGraphFromKmersSpec extends FlatSpec with Matchers with ReadFrom {

  "graph" should "be calculated" in {
    val kmerList="GAGG\n     GGGG\n     GGGA\n     CAGG\n     AGGG\n     GGAG".split("\n").map(_.trim).toList

    val graph = createGraph(kmerList).toList.sorted
    val expected = "AGG -> GGG\n     CAG -> AGG\n     GAG -> AGG\n     GGA -> GAG\n     GGG -> GGA,GGG".split("\n").map(_.trim).toList
    api.Assertions.assertThat(graph.asJava).containsExactlyElementsOf(expected.asJava)
  }

  "graph" should "be calculated in extra set" in {
    val kmerList=readAsList("debruijn_graph_kmers_inp.txt").toList

    val graph = createGraph(kmerList).toList.sorted
    val expected = readAsList("debruijn_graph_kmers_out.txt").toList
    api.Assertions.assertThat(graph.asJava).containsAll(expected.asJava)
    api.Assertions.assertThat(graph.asJava).containsExactlyElementsOf(expected.asJava)
  }

  "graph" should "be calculated in test case" in {
    val kmerList=readAsList("debruijn_graph_kmers_test_inp.txt").toList

    val graph = createGraph(kmerList).toList.sorted
    println(graph.mkString("\n"))
    val expected = readAsList("debruijn_graph_kmers_test_out.txt").toList
    api.Assertions.assertThat(graph.asJava).containsAll(expected.asJava)
  }

  def createGraph(kmerList:List[String]) = new DeBruijnGraph().createGraphFromKMers(kmerList)

}


