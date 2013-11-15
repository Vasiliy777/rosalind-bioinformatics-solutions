
package proteins

import org.scalatest.{Matchers, FlatSpec}
import org.assertj.core.api.Assertions
import scala.collection.JavaConverters._

class DNAChainToProteinTranslationSpec extends FlatSpec with Matchers {

  "DNA" should "be transcripted and translated to all possible proteins" in {
    val dna: DNA = new DNA(DNADirection.FIVE_TO_THREE, "GTGAAACTTTTTCCTTGGTTTAATCAATAT")

    val proteins:Iterable[Protein] = new Proteins().allPossibleProteins(dna)

    val proteinsAsShortcuts:Iterable[String] = proteins.map(_.toShortcutString)

    Assertions.assertThat(proteinsAsShortcuts.asJava).contains(
      "HisPheLysLysArgProLysIleLeuIle","SerValLysGluLysThrXXXAspIle","PheSerLysGlyGlnAsnLeuXXXTyr",
      "GluThrPheSerLeuValXXXSerIle","XXXAsnPhePheLeuGlyLeuIleAsn","ValLysLeuPheProTrpPheAsnGlnTyr");
  }


}
