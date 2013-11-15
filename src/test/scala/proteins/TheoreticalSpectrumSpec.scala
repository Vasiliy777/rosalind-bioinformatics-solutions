
package proteins

import org.scalatest.{Matchers, FlatSpec}
import utils.ReadFrom
import org.assertj.core.api.Assertions
import scala.collection.JavaConverters._

class TheoreticalSpectrumSpec extends FlatSpec with Matchers with ReadFrom  {

  "Peptide" should "have spectrum - example" in {
    val peptide = "LEQN"

    Assertions.assertThat(spectrum(peptide).asJava).containsExactly(0, 113 ,114 ,128 ,129 ,227 ,242 ,242 ,257 ,355 ,356 ,370 ,371 ,484)
  }

  "Peptide" should "have spectrum - extra example" in {
    val peptide = "IAQMLFYCKVATN"

    Assertions.assertThat(spectrum(peptide).asJava).containsExactly(0, 71, 71, 99, 101, 103, 113, 113, 114, 128, 128, 131, 147, 163, 170, 172, 184, 199, 215, 227, 227, 231, 244, 259, 260, 266, 271, 286, 298, 298, 310, 312, 328, 330, 330, 372, 385, 391, 394, 399, 399, 399, 401, 413, 423, 426, 443, 443, 470, 493, 498, 502, 513, 519, 526, 527, 541, 554, 556, 557, 564, 569, 590, 598, 616, 626, 640, 654, 657, 658, 665, 670, 682, 697, 697, 703, 711, 729, 729, 753, 753, 771, 779, 785, 785, 800, 812, 817, 824, 825, 828, 842, 856, 866, 884, 892, 913, 918, 925, 926, 928, 941, 955, 956, 963, 969, 980, 984, 989, 1012, 1039, 1039, 1056, 1059, 1069, 1081, 1083, 1083, 1083, 1088, 1091, 1097, 1110, 1152, 1152, 1154, 1170, 1172, 1184, 1184, 1196, 1211, 1216, 1222, 1223, 1238, 1251, 1255, 1255, 1267, 1283, 1298, 1310, 1312, 1319, 1335, 1351, 1354, 1354, 1368, 1369, 1369, 1379, 1381, 1383, 1411, 1411, 1482)
  }

  "Peptide" should "have spectrum - test case" in {
    val peptide = "GAIHNYGKEIAGLW"

    println(spectrum(peptide).mkString(" "))
    Assertions.assertThat(spectrum(peptide).asJava).containsExactly(0, 57, 57, 57, 71, 71, 113, 113, 113, 114, 128, 128, 128, 129, 137, 163, 170, 184, 184, 185, 186, 220, 241, 241, 241, 242, 243, 250, 251, 257, 277, 299, 313, 314, 314, 321, 334, 348, 354, 356, 356, 364, 370, 370, 378, 413, 414, 427, 427, 427, 427, 435, 441, 462, 471, 477, 483, 484, 484, 492, 498, 498, 527, 540, 540, 555, 555, 564, 584, 590, 591, 597, 597, 598, 599, 611, 655, 655, 661, 668, 668, 668, 669, 677, 678, 704, 712, 712, 718, 726, 728, 734, 775, 781, 783, 791, 797, 797, 805, 831, 832, 840, 841, 841, 841, 848, 854, 854, 898, 910, 911, 912, 912, 918, 919, 925, 945, 954, 954, 969, 969, 982, 1011, 1011, 1017, 1025, 1025, 1026, 1032, 1038, 1047, 1068, 1074, 1082, 1082, 1082, 1082, 1095, 1096, 1131, 1139, 1139, 1145, 1153, 1153, 1155, 1161, 1175, 1188, 1195, 1195, 1196, 1210, 1232, 1252, 1258, 1259, 1266, 1267, 1268, 1268, 1268, 1289, 1323, 1324, 1325, 1325, 1339, 1346, 1372, 1380, 1381, 1381, 1381, 1395, 1396, 1396, 1396, 1438, 1438, 1452, 1452, 1452, 1509)
  }

  def spectrum(peptide:String):Seq[Int] = {
    val cyclic = peptide+peptide;
    (0 :: (for (length <- 1 to peptide.length-1;
    index <- 0 to peptide.length-1;
    subpeptide = cyclic.substring(index,index+length)) yield mass(subpeptide)).toList.sortBy(identity)) :+ mass(peptide)
  }

  def mass(subpeptide:String):Int = {
    subpeptide.map(massAmino(_)).sum
  }

  def massAmino(amino:Char):Int = amino match {
    case  'G' => 57
    case  'A' => 71
    case  'S' => 87
    case  'P' => 97
    case  'V' => 99
    case  'T' => 101
    case  'C' => 103
    case  'I' => 113
    case  'L' => 113
    case  'N' => 114
    case  'D' => 115
    case  'K' => 128
    case  'Q' => 128
    case  'E' => 129
    case  'M' => 131
    case  'H' => 137
    case  'F' => 147
    case  'R' => 156
    case  'Y' => 163
    case  'W' => 186
  }

}
