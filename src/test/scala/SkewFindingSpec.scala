import org.scalatest._
import scala.collection.{mutable, GenTraversableOnce}


class SkewFindingSpec extends FlatSpec with Matchers {


  def diff(c: Char): Int = {
    c match {
        case 'C' => -1
        case 'G' => +1
        case _ => 0
    }
  }

  def skewDiff(s: String):String = {
    var i:Int = 0;
    var result:mutable.MutableList[Int] = new mutable.MutableList[Int]
    s.foreach(c => {                  // should have use list comprehensions but don't know how to use i with that
      i = i + diff(c);
      result += i;
    })
    return "0 " + result.mkString(" ")
  }

  "genome" should "return skew" in {
    val genome = "CATGGGCATCGGCCATACGCC"

    val skewDiffVal = skewDiff(genome)

    assert(skewDiffVal === "0 -1 -1 -1 0 1 2 1 1 1 0 1 2 1 0 0 0 0 -1 0 -1 -2")
  }

  "test case genome" should "return skew" in {
    val genome = "GAGCCACCGCGATA"

    val skewDiffVal = skewDiff(genome)

    assert(skewDiffVal === "0 1 1 2 1 0 0 -1 -2 -1 -2 -1 -1 -1 -1")
  }

}


