
package proteins

import org.scalatest.{Matchers, FlatSpec}
import org.assertj.core.api.Assertions
import scala.collection.JavaConverters._
import utils.ReadFrom

class SubPeptidesSpec extends FlatSpec with Matchers with ReadFrom  {

  "Subpeptides" should "be counted for 2" in {
    val length = 2

    assertResult(2)(subpeptidesCount(length))
  }

  "Subpeptides" should "be counted for 3" in {
    val length = 3

    assertResult(6)(subpeptidesCount(length))
  }

  "Subpeptides" should "be counted for 4" in {
    val length = 4

    assertResult(12)(subpeptidesCount(length))
  }

  "Subpeptides" should "be counted for 31315" in {
    val length = 31315

    assertResult(980597910)(subpeptidesCount(length))
  }

  "Subpeptides" should "be counted for 46484" in {
    val length = 46484

    assertResult(2160715772L)(subpeptidesCount(length))
  }

  def subpeptidesCount(length:Long):Long = {
    length*(length-1)
  }
}
