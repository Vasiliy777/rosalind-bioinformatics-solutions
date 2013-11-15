import org.scalatest._
import scala.collection.mutable
import utils.ReadFrom


class MinimizeSkewSpec extends FlatSpec with Matchers with ReadFrom {


  def diff(c: Char): Int = {
    c match {
        case 'C' => -1
        case 'G' => +1
        case _ => 0
    }
  }

  def skewDiff(s: String):String = {
    return "0 " + skewDiffValues(s).mkString(" ")
  }


  def skewDiffValues(s: String):mutable.MutableList[Int] = {
    var i: Int = 0;
    var result: mutable.MutableList[Int] = new mutable.MutableList[Int]
    s.foreach(c => {
      // should have use list comprehensions but don't know how to use i with that
      i = i + diff(c);
      result += i;
    })
    return result;
  }

  def allMin(list: mutable.MutableList[Int]):List[Int] = {
    println(list)
    println(list.zipWithIndex)
    println(list.zipWithIndex.groupBy(_._1))
    println(list.zipWithIndex.groupBy(_._1).minBy(_._1))
    println(list.zipWithIndex.groupBy(_._1).minBy(_._1)._2)
    println(list.zipWithIndex.groupBy(_._1).minBy(_._1)._2.collect({case pair:(Int,Int) => pair._2+1}))
    return list.zipWithIndex.groupBy(_._1).minBy(_._1)._2.collect({case pair:(Int,Int) => pair._2+1}).toList
  }

  "genome" should "find minimized skew index" in {
    val genome = "TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT"

    val skewDiffMins = allMin(skewDiffValues(genome))

    assert(skewDiffMins === List(11,24))
  }

  "test genome" should "find minimized skew index" in {
    val genome = readFrom("test-minimize-skew.txt")

    val skewDiffMins = allMin(skewDiffValues(genome))

    println(skewDiffMins.mkString(" "))
    assert(skewDiffMins === List(40439, 40440, 40441))
  }

}


