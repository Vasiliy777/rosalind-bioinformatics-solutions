package assemble

import org.scalatest._
import utils.ReadFrom
import org.assertj.core.api
import scala.collection.JavaConverters._
import scala.collection.parallel.mutable
import scala.collection

class EulerCycleSpec extends FlatSpec with Matchers with ReadFrom {

  "graph" should "have euler cycle" in {
    val adjacencyList="0 -> 3\n     1 -> 0\n     2 -> 1,6\n     3 -> 2\n     4 -> 2\n     5 -> 4\n     6 -> 5,8\n     7 -> 9\n     8 -> 7\n     9 -> 6".split("\n").map(_.trim).toList

    val cycle = aCycle(adjacencyList)
    val expected = "6->8->7->9->6->5->4->2->1->0->3->2->6".split("\n").map(_.trim).toList
    api.Assertions.assertThat(cycle).isEqualTo(expected)
  }

  def aCycle(adjacency:List[String]):String = {
    val aMap = adjacency.map((adj) => adj.take(left(adj))).zip(adjacency.map((adj) => adj.drop(right(adj)).split(",").toList)).toMap
    val mutableMap = collection.mutable.Map[String,List[String]](aMap.toSeq: _*)
    println(mutableMap)
    val solution = new scala.collection.mutable.MutableList[String]()
    while (mutableMap.isEmpty == false) {
      val nextNode: String = mutableMap.keySet.head
      solution += nextNode
      solution += mutableMap.get(nextNode).get.head
      mutableMap.remove(nextNode)
      println(solution)
    }
    "qwe"
  }

  def left(adj:String):Int = {
    adj.indexOf(" -> ")
  }

  def right(adj:String) = left(adj)+4

}


