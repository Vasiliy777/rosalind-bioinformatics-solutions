
package proteins

import org.scalatest.{Matchers, FlatSpec}
import utils.ReadFrom
import org.assertj.core.api.Assertions
import scala.collection.JavaConverters._
import scala.collection.mutable

class SpectrumWithErrors_Imperative_Spec extends FlatSpec with Matchers with ReadFrom  {

  def originalMasses = new Range.Inclusive(57,200,1).toList.par

  "Peptides" should "be found for spectrum with errors - example" in {
    val boardSize = 10
    val spectrum = "0 71 113 129 147 200 218 260 313 331 347 389 460".split(" ").map(_.toInt).toList

    assert(peptidesForSpectrum(spectrum,boardSize).map(_.mkString("-")).contains("129-113-147-71"))
  }

  "Peptides" should "be found for spectrum with errors - extra example" in {
    val boardSize = 26
    val spectrum = "0 71 97 101 103 113 113 113 113 114 114 115 128 128 128 128 129 131 131 131 156 156 184 186 186 200 214 227 227 228 230 231 241 242 242 243 244 244 256 257 262 269 270 287 298 299 301 328 331 340 340 343 345 345 356 358 359 370 370 372 375 383 385 397 400 401 429 430 442 453 454 454 459 462 468 471 472 473 474 485 486 487 498 499 501 512 514 514 542 561 567 570 573 575 581 583 585 590 599 600 600 601 602 610 615 615 616 627 627 630 658 695 696 698 698 698 701 703 704 713 723 728 728 728 728 730 730 731 741 744 747 758 761 769 799 810 817 827 829 831 832 841 841 844 844 851 854 854 857 859 862 872 882 884 886 889 928 928 944 945 947 955 955 958 959 960 966 967 972 972 982 985 990 996 997 1000 1000 1003 1041 1056 1059 1062 1068 1068 1068 1073 1075 1075 1084 1087 1089 1095 1097 1103 1113 1114 1128 1128 1131 1152 1172 1172 1181 1182 1184 1189 1190 1190 1196 1197 1199 1200 1202 1210 1212 1227 1231 1242 1259 1259 1283 1295 1298 1303 1303 1303 1303 1304 1311 1312 1317 1318 1325 1325 1328 1330 1338 1340 1345 1355 1356 1388 1396 1416 1426 1426 1427 1431 1432 1432 1434 1440 1442 1443 1445 1451 1453 1453 1454 1458 1459 1459 1469 1489 1497 1529 1530 1540 1545 1547 1555 1557 1560 1560 1567 1568 1573 1574 1581 1582 1582 1582 1582 1587 1590 1602 1626 1626 1643 1654 1658 1673 1675 1683 1685 1686 1688 1689 1695 1695 1695 1696 1701 1703 1704 1713 1713 1733 1754 1757 1757 1771 1772 1782 1788 1790 1796 1798 1801 1810 1810 1812 1817 1817 1817 1823 1826 1829 1844 1882 1885 1885 1888 1889 1895 1900 1903 1913 1913 1918 1919 1925 1926 1927 1930 1930 1938 1940 1941 1957 1957 1996 1999 2001 2003 2013 2023 2026 2028 2031 2031 2034 2041 2041 2044 2044 2053 2054 2056 2058 2068 2075 2086 2116 2124 2127 2138 2141 2144 2154 2155 2155 2157 2157 2157 2157 2162 2172 2181 2182 2184 2187 2187 2187 2189 2190 2227 2255 2258 2258 2269 2270 2270 2275 2283 2284 2285 2285 2286 2295 2300 2302 2304 2310 2312 2315 2318 2324 2343 2371 2371 2373 2384 2386 2387 2398 2399 2400 2411 2412 2413 2414 2417 2423 2426 2431 2431 2432 2443 2455 2456 2484 2485 2488 2500 2502 2510 2513 2515 2515 2526 2527 2529 2540 2540 2542 2545 2545 2554 2557 2584 2586 2587 2598 2615 2616 2623 2628 2629 2641 2641 2642 2643 2643 2644 2654 2655 2657 2658 2658 2671 2685 2699 2699 2701 2729 2729 2754 2754 2754 2756 2757 2757 2757 2757 2770 2771 2771 2772 2772 2772 2772 2782 2784 2788 2814 2885".split(" ").map(_.toInt).toList

    assert(peptidesForSpectrum(spectrum,boardSize).map(_.mkString("-")).contains("129-101-113-131-156-114-113-71-156-113-128-115-186-186-113-131-131-97-103-113-129-128-128"))
  }

//  "Peptides" should "be found for spectrum with errors - test case" in {
//    val boardSize = 417
//    val spectrum = "0 71 71 71 87 97 97 103 103 113 113 114 115 115 115 128 128 128 129 137 142 156 158 163 174 186 186 199 200 201 206 212 225 228 234 241 243 244 257 257 261 265 269 270 270 272 276 277 301 303 315 315 336 340 349 356 357 362 362 364 370 372 374 375 383 385 386 391 397 398 407 418 428 433 443 453 461 462 464 470 477 478 485 489 490 501 504 511 514 514 519 522 526 531 541 556 561 561 575 576 577 577 590 598 604 605 616 619 627 632 634 640 641 642 644 651 664 669 674 675 676 690 692 705 712 718 727 731 747 747 747 748 753 755 755 762 767 772 777 779 783 789 789 798 802 820 826 838 842 844 846 860 862 875 876 876 876 880 881 892 892 897 901 911 913 913 917 917 925 939 951 952 959 972 975 984 984 989 991 995 1004 1004 1014 1016 1020 1023 1025 1032 1038 1039 1048 1054 1055 1066 1067 1087 1087 1101 1104 1112 1117 1117 1119 1119 1123 1138 1138 1145 1145 1151 1152 1153 1158 1162 1182 1183 1190 1194 1195 1209 1216 1216 1220 1232 1232 1249 1253 1254 1259 1261 1267 1267 1273 1273 1279 1281 1286 1287 1291 1308 1308 1320 1324 1324 1331 1345 1346 1350 1357 1358 1378 1382 1387 1388 1389 1395 1395 1402 1402 1417 1421 1421 1423 1423 1428 1436 1439 1453 1453 1473 1474 1485 1486 1492 1501 1502 1508 1515 1517 1520 1524 1526 1536 1536 1545 1549 1551 1556 1556 1565 1568 1581 1588 1589 1601 1615 1623 1623 1627 1627 1629 1639 1643 1648 1648 1659 1660 1664 1664 1664 1665 1678 1680 1694 1696 1698 1702 1714 1720 1738 1742 1751 1751 1757 1761 1763 1768 1773 1778 1785 1785 1787 1792 1793 1793 1793 1809 1813 1822 1828 1835 1848 1850 1864 1865 1866 1871 1876 1889 1896 1898 1899 1900 1906 1908 1913 1921 1924 1935 1936 1942 1950 1963 1963 1964 1965 1979 1979 1984 1999 2009 2014 2018 2021 2026 2026 2029 2036 2039 2050 2051 2055 2062 2063 2070 2076 2079 2087 2097 2107 2112 2122 2133 2142 2143 2149 2154 2155 2157 2165 2166 2168 2170 2176 2178 2178 2183 2184 2191 2200 2204 2225 2225 2237 2239 2263 2264 2268 2270 2270 2271 2275 2279 2283 2283 2296 2297 2299 2306 2312 2315 2328 2334 2339 2340 2341 2354 2354 2366 2377 2382 2384 2398 2403 2411 2412 2412 2412 2425 2425 2425 2426 2427 2427 2437 2437 2443 2443 2453 2469 2469 2469 2540".split(" ").map(_.toInt).toList
//
//    assert(peptidesForSpectrum(spectrum,boardSize).map(_.mkString("-")).contains("115-71-71-128-137-97-128-115-113-163-186-115-97-103-103-71-87-114-156-113-128-129"))
//  }

  "Peptides" should "be found for tyrocidine with 25% errors" in {
    val boardSize = 2000
    val spectrum = "0 97 99 113 114 115 128 128 147 147 163 186 227 241 242 244 244 256 260 261 262 283 291 309 330 333 340 347 385 388 389 390 390 405 435 447 485 487 503 504 518 544 552 575 577 584 599 608 631 632 650 651 653 672 690 691 717 738 745 770 779 804 818 819 827 835 837 875 892 892 917 932 932 933 934 965 982 989 1039 1060 1062 1078 1080 1081 1095 1136 1159 1175 1175 1194 1194 1208 1209 1223 1322".split(" ").map(_.toInt).toList

    val candidates: Seq[CandidateWithScore] = candidatesForSpectrum(spectrum,boardSize)
    println(candidates.map(_.score).toSet)
    println(candidates)
  }

  class CandidateWithScore(val localMasses:List[Int],val score:Int) extends Candidate(localMasses) {
    def append(mass:Int,spectrum:List[Int]):CandidateWithScore = new CandidateWithScore(masses :+ mass,calculateScore(masses :+ mass,spectrum))

    override def toString: String = localMasses.mkString("-") + " > " + score

    override def hashCode(): Int = localMasses.hashCode()

    override def equals(obj: scala.Any): Boolean = obj match { case a:CandidateWithScore => localMasses.equals(a.localMasses); case _ => false }
  }

  def calculateScore(candidate:List[Int],spectrum:List[Int]):Int = {
    if (candidate.sum > spectrum.last) -1 else  spectrum.intersect((for (i <- 1 to candidate.length; combination <- candidate.union(candidate).sliding(i)) yield(combination.sum))).size
  }

  def candidatesForSpectrum(spectrum:List[Int],boardSize:Int):Seq[CandidateWithScore] = {
    val possibleMasses = originalMasses.intersect(spectrum)
    var candidates:mutable.MutableList[CandidateWithScore] = mutable.MutableList()
    var newCandidates = mutable.MutableList(possibleMasses.map((mass:Int) => new CandidateWithScore(List(mass),0)).toArray :_*)
    var topCandidates:mutable.MutableList[CandidateWithScore] = mutable.MutableList()
      while (newCandidates.nonEmpty) {
        topCandidates = topCandidates.union(newCandidates).sortBy(_.score * -1)
        if(topCandidates.size > boardSize) {
          val cutScore: Int = topCandidates.get(boardSize).get.score
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

  def peptidesForSpectrum(spectrum:List[Int],boardSize:Int) = {
    candidatesForSpectrum(spectrum,boardSize).map(_.asList)
  }


}
