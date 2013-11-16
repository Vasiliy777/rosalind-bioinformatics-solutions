
package proteins

import org.scalatest.{Matchers, FlatSpec}
import utils.ReadFrom
import org.assertj.core.api.Assertions
import scala.collection.mutable
import scala.collection.JavaConverters._

class ConvolutionCyclopeptideSequencingSpec extends FlatSpec with Matchers with ReadFrom  {

  "Peptides" should "be found for spectrum with errors - example" in {
    val convolutionTop = 20
    val leaderboardSize = 60
    val spectrum = "57 57 71 99 129 137 170 186 194 208 228 265 285 299 307 323 356 364 394 422 493".split(" ").map(_.toInt).toList

    Assertions.assertThat(peptidesForSpectrum(spectrum,convolutionTop, leaderboardSize).map(_.mkString("-")).asJava).contains("99-71-137-57-72-57")
  }

  "Peptides" should "be found for spectrum with errors - extra example" in {
    val convolutionTop = 16
    val leaderboardSize = 184
    val spectrum = "672 669 1075 761 690 464 579 344 841 1142 1291 633 1204 438 474 882 399 853 879 301 637 766 303 1275 985 216 595 113 781 668 1191 1086 1249 458 87 273 601 1248 486 1176 729 511 417 975 546 224 1261 1116 682 596 71 214 938 87 295 1275 1138 966 1174 367 568 725 759 1233 816 1305 1263 896 658 129 1215 300 396 924 325 287 0 1067 171 945 1089 466 704 1154 434 881 1158 1305 995 898 99 580 1199 276 382 158 612 1029 509 693 851 603 928 186 767 888 1037 805 270 57 963 295 521 204 1092 680 360 876 377 783 208 147 794 1146 387 1225 483 980 1148 480 137 1061 581 246 188 417 750 101 694 1018 481 904 333 945 1067 1059 57 213 1362 1002 424 114 163 1149 1062 220 782 557".split(" ").map(_.toInt).toList

    Assertions.assertThat(peptidesForSpectrum(spectrum,convolutionTop, leaderboardSize).map(_.mkString("-")).asJava).contains("147-99-114-57-129-87-71-137-87-101-129-147-57")
  }

  "Peptides" should "be found for spectrum with errors - test case" in {
    val convolutionTop = 20
    val leaderboardSize = 353
    val spectrum = "97 528 1278 128 210 1377 553 214 113 750 541 289 725 186 113 417 147 652 952 853 838 842 893 436 499 1505 1079 564 325 778 115 342 1176 516 1065 1103 663 1319 1216 622 442 1048 749 759 755 1092 935 727 1406 1216 966 1255 413 438 874 1069 1191 103 646 1069 1006 964 756 1291 989 1275 883 1392 941 1295 0 1358 436 329 859 652 1289 1390 436 186 539 1319 667 313 862 115 1408 1402 966 1390 612 210 539 1192 853 1063 289 230 99 643 314 1295 343 780 570 1163 1180 250 440 228 1067 457 1277 631 426 227 1392 656 1088 977 549 849 1172 1069 402 216 103 746 956 333 1182 1402 323 1162".split(" ").map(_.toInt).toList

    Assertions.assertThat(peptidesForSpectrum(spectrum,convolutionTop, leaderboardSize).map(_.mkString("-")).asJava).contains("186-147-103-186-128-99-115-115-113-97-113-103")
  }

  def candidatesForSpectrum(spectrum:List[Int],convolutionTop:Int,leaderboardSize:Int):Seq[CandidateWithScore] = {
    val possibleMasses = new Convolution(spectrum).getTop(convolutionTop)
    println(possibleMasses)
    var candidates:mutable.MutableList[CandidateWithScore] = mutable.MutableList()
    var newCandidates = mutable.MutableList(possibleMasses.map((mass:Int) => new CandidateWithScore(List(mass),0)).toArray :_*)
    var topCandidates:mutable.MutableList[CandidateWithScore] = mutable.MutableList()
    while (newCandidates.nonEmpty) {
      topCandidates = topCandidates.union(newCandidates).sortBy(_.score * -1)
      if(topCandidates.size > leaderboardSize) {
        val cutScore: Int = topCandidates.get(leaderboardSize).get.score
        topCandidates = topCandidates.takeWhile(_.score >= cutScore)
      }
      candidates = topCandidates
      newCandidates = mutable.MutableList()
      while (candidates.nonEmpty) {
        val candidate = candidates.head
        val candidateCandidates: List[CandidateWithScore] = possibleMasses.map(candidate.append(_, spectrum)).filter(_.score >= 0 ).toList
        newCandidates.++=(candidateCandidates)
        candidates = candidates.tail
      }
    }
    topCandidates

  }

  def peptidesForSpectrum(spectrum:List[Int],convolutionTop:Int,leaderboardSize:Int) = {
    candidatesForSpectrum(spectrum,convolutionTop,leaderboardSize+100).map(_.asList)
  }


}
