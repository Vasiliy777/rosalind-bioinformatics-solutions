
package proteins

import org.scalatest.{Matchers, FlatSpec}
import utils.ReadFrom
import org.assertj.core.api.Assertions
import com.twitter.storehaus.cache._
import scala.collection.immutable.::

class CountPeptidesWithGivenMassSpec extends FlatSpec with Matchers with ReadFrom  {

  "Count of peptides" should "be found for given mass - example " in {
    val mass = 1024L

    Assertions.assertThat(countPeptidesFor(mass)).isEqualTo(14712706211L)
  }

  "Count of peptides" should "be found for given mass - text case " in {
    val mass = 1222L

    Assertions.assertThat(countPeptidesFor(mass)).isEqualTo(14712706211L)
  }

  "Count of peptides" should "be found for given mass - simplest" in {
    val mass = 57L

    Assertions.assertThat(countPeptidesFor(mass)).isEqualTo(1L)
  }

  "Count of peptides" should "be found for given mass - four peptides" in {
    val mass = 57L + 71L

    Assertions.assertThat(countPeptidesFor(mass)).isEqualTo(3L)
  }

  "Count of peptides" should "be found for given mass - two same peptides" in {
    val mass = 71L + 71L

    Assertions.assertThat(countPeptidesFor(mass)).isEqualTo(1L)
  }

  def countPeptidesFor(mass:Long):Long = {
    return countMem(originalMasses,mass)
  }

  val cache = MapCache.empty[(List[Int],Long), Long].toMutable()

  def countMem(masses:List[Int],massToSolve:Long):Long = Memoize[List[Int],Long,Long](cache){ (masses:List[Int],massToSolve:Long) =>
    masses match {
      case head :: tail =>
        if(head > massToSolve) 0 else if ( head == massToSolve) 1 else countMem(originalMasses, massToSolve-head) + countMem(tail,massToSolve)
      case _ => 0
    }
  } (masses,massToSolve)

  def count(masses:List[Int],massToSolve:Long):Long = masses match {
    case head :: tail =>
       if(head > massToSolve) 0 else if ( head == massToSolve) 1 else count(originalMasses, massToSolve-head) + count(tail,massToSolve)
    case _ => 0
  }

  def originalMasses = List(57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186)

}
