import org.scalatest._
import scala.collection.{GenSet, GenTraversableOnce}


class ClumpsFindingSpec extends FlatSpec with Matchers {

  "indexes in window" should "be found" in {
    assert(hasindexesWithinWindow(List(1,3,5),6,3) === true)
    assert(hasindexesWithinWindow(List(1,6,11),4,2) === false)
  }

  def hasindexesWithinWindow(indexes:List[Int], window: Int, howMany: Int): Boolean = {
    indexes.sliding(howMany).count((list:List[Int]) => list.last-list.head < window) > 0
  }

  def allClumps(genome: String, kmerLength: Int, clumpSize: Int, howManyToFormAClump: Int):GenTraversableOnce[String] = {
    println(genome.sliding(kmerLength).toList)
    println(genome.sliding(kmerLength).toList.zipWithIndex)
    println(genome.sliding(kmerLength).toList.zipWithIndex.groupBy(_._1))
    println(genome.sliding(kmerLength).toList.zipWithIndex.groupBy(_._1).filter(_._2.size >= howManyToFormAClump))
    println(genome.sliding(kmerLength).toList.zipWithIndex.groupBy(_._1).filter(_._2.size >= howManyToFormAClump)
    .filter(pair => {
      hasindexesWithinWindow(pair._2.unzip._2, clumpSize,howManyToFormAClump)
    }))
    val result: Map[String, Int] = genome.sliding(kmerLength).toList.zipWithIndex.groupBy(_._1).filter(_._2.size >= howManyToFormAClump)
      .filter(pair => {
      hasindexesWithinWindow(pair._2.unzip._2, clumpSize, howManyToFormAClump)
    }).mapValues(_.size)
    println(result)

    return result.keySet
  }

  "example clumps" should "be found" in {
    val genome = "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGAGAAGAGGAAACATTGTAA"
    val kmerLength = 5
    val clumpSize = 50
    val howManyToFormAClump = 4

    val clumps = allClumps(genome,kmerLength,clumpSize,howManyToFormAClump)

    assert(clumps.toSet === Set("CGACA", "GAAGA"))
  }

  "test case" should "be found" in {
    val genome = "TCCTTAGGAGGCGCCCTAGGTACGTCCTTAGATAACACGCAGGCTACCGTACCCATACACGGTACGTACCATCTATTTATATTTGCTAGCCTATACTCCGGAACCCCGGGGCGTGTAACGGTGCTTCCGCCTTAGCTTGTTAGTGTTAGGCCACATGTCGGTAGTGGAGATCTTTTCACTGATGCATGGAAATCATAATCACATAGCCCATTCGCCTAATGACACATCAAGACCAGTGCGACAGTCCCGGGCGAACAATCGGAAGGACAATTTGCCCTCAAGTGCATGGACATAGTATGCGCCCTCTCTATAAGCAGATAGCATTATACGTGTTGGCGTATGGCCACCACGTTGACCTCACATGAGCCTATACTCCCAAAGGCTGCCATCATCTTTTCTGTGATTGTCCAGCGGCTGGAGCGGCTCAACGGTAAGGACAACATGTAAGTGCCGCAGACAACATATAGGATACCATGGTGATCGAATCCAGATTACGTGGAGGCACAGATCGCCATGCTTACAATTGCCAATGATAATCGTCGGTGTTGATGGTCCCGCCTATGTGGGGAGACTCGTGGTCCTAAGAGATCTTTACGAGGTTCCCTATGGGGTATTCGCTGCTCTCTACGCCAGGGTGGGTCAGCACTTTATAGCCTCGGACTTCATTATAGCCTCTCGCAAATTATGTTCAGCCCGACCCCTTCTTTGACCGCGGGCAGGGCCTATAGCCTCATATAGCTATAGCCTCGTATAGCCTCTTCGGGCTGTGTATAGCCTCTATAGCCTCGCGCGCTTATTCATTGTCGTGCATCAGTATAGCCTCGCCTCACCCATATATAGCCTCCTGCACGCTTATAGCCTCCCTCTCACTATAGCCTCCATTCAAAGGAGTCTATAGTATAGCCTCCCGTTAGTCGCATAGCTCCTTTACCCCTGTTTTGCGTCACTCAGACCTATAGCCTATAGCCTCAGTGAATGGGTTATAGCCTCGCTGCTTTATAGCCTCCTACCTATAGCCTCATATAGCCTCAAATTCCGTATAGCCTTATAGCCTCATAGCCTCAAGGCTACAACAGTCGCGCACCAGTGAGTTATGGTCGGCGACTTAGAGTGTTCGTTATAGCCTCTCTATGACACCCTCCTATATATAGCCTCATATCGTTATATATAGCCTCTATATAGCTATAGCCTCTAGCCTCTCTTTGCTTACTCTCAACTAACGCAAGGGTGATCTGTATGTATATAAGCGCTATACGGCTCACAATGCGAATTTGCGGTCTCGGTACTGACAGCGCGCGCCCCCCGTTATAATGTAGGTTCGTATATACTCTAATTTCTCCTAAGATATTAGCCTCCGAATACACGCTGGGCCGGTGAGGGATGGATCCTCGAATTCTTCGACCGTTGCTGCCGACGGTCATGCGCATGAAAACACGGAAATGTCGTCAGACATAACTAACACTCTTATAACCTCTGTTGGCCCGAGAAGTACCGTGAGCTAAGGCAGACGGCGATCTTTCTGCATGCATGGGGAGAAACCGCGAACTGGTCATCGAACAGAAACTAGACGCCACCCAGCAGCCATGCGTCCCCTCTTTAGCATGGAAGGATGTGTTGGGAGAATCACATCTCGTTCGATTTCTATCAGTCGTCAGCGGTCAACTGCCGACAGTCTCCCGTTGTTACTTAATAGTACATTTATAGTCCTCCACCCGGAGGGACTGACTAACCCTACGGGTGGAGACTCCGATCAGTGTTGACTAGGCTCTTTGTCCCAACGAATACGAGGCATTAAACAGGGAGGCGTCAACATACTATGCATCGTCAACGTCGGCATCGGTCATGTTCACGAGTTGGGAAGTCCAATTGTCAGTTGTGTCTTCTGCGCTCCACCGTTGACGAACGCGTGACGGCAGTGTTACCTAGATGAAAAATCTCTGCCTTTTCTACATACTTTCGATGTCAGTTAGACGAGTAAGTAAAGCTCGAGATTGTTCTCATCACGAGACCCGCCCAAATGTGTGATGCCTGATTTTAGCTGCCTTGAGTGTAATGGCGTCATCAGCAATAACGATACACATGTACCACGTCCAGAAGACGAGACCCACGGTAGCTGTGTAACTACTTCTCGGGGTAAGTAACTAATCAGGGACTACCTGTGCTATGTGCTCTGAGTTTAGGCTGAGCGACTAGAAGTTAAGCTTGTTGAATCATACGTGGTTAATCAGTCGTGCGACTCTATCGATATTTTTATCGCAAGGCGGAAACACACAACGACGAACCAACCCATCTAACTACGACTGCTGACCATTGTCGAGGCTCGGTATCCGACAACCGGCGGTGTATCCCTAGCACGGCAATGAGGCTGAACTCTCGAATGTGCGTAACTGCAACGGGGCACTGTAAACACAGTAATAACGGGTAGTTTATTAGCCCGTATAGTTACTTAGCGGGTGCTTCTAATAACTACGATGATCTTGACCTTGACGGTGGCGGGCCCGAGCCGATTGGCCAAAAGAGCTGGGACCGGCCCAACGGATAGTCAGGCGGCTAATGCACTGGGATTGGCCTCAAATTCGGCGTTTGGGCTGTTCGCTGGCATGCCTCGCCGTTATTCGGGTTTAATTCCCCGTCGGCACCAGGCGCTCTTCTTATTTTTTACCTGGACATATCTCAGGCATTGAAGGCTCTGATAACCCGTCTGCGCTGGTTATGTGCTGGAACCTAGATTCGACATCATGACTTCTATGAAAAGGTGCCGTACTCAGCATGTTTGTACTCTTTTGCAGTGCTTAAGAGGTGCCGATCTTGGTACAGTCAGCGTTGGAATTGCGGGTCAATATAGGTAAAGCGAACTACTAAAGTAAGACCCCGTTTGACCTAAGTGCATTAAACCGCGGTAGCTCGCTGTACGAGGTCCGTCAGAATCACCCCACTAAAGTATCAGGCCCCGAACCCATTATTAGAACTTGGGTTAGGGCAGCCGAAAAAATCCAAAAAGACGATTTAGAAGCAACGGTAAAATCTGCCCGGTGGGTAGGCATATGAGCCGTTGTCCGGAGACGTCGCCAAAACAAGTCACACGGGTACCATCCAAGGAAACACCCACGCATGGGCTAGCTCGGAAGCAGAAACAAACAACGCTCGTTGCGCAAGGAGTTGGTGAAGTGTATGTGGACTCCAACTGATGTCCAGCTGGCATTTGATCTCTGGAATAATGACACTCCGACTGTCGTCTTCCACGTTGCTAGGCTAGACCGCCTAGCAGAGGAAGAACGTGCTGTTCCGCTCGAACAACTAGCACGTCGGGAAATCGGGAGTAGAAGCACAAGTGGGAACGCACAAACTAGTCCTTGCGGACGCCTTCTGTAAGTTCGATACACGGTACCCGTCATACACAGACCCTCGCCTAATGACTAGGAACTTCCATCATGGCGATGACAGCTGACAAGCAGTACGCCTACAGCTGACCTCAGCTGCCGTGCATCTGCGTAAACCCCCAGCTGACAGCTCAGCTGACAGCTGCACAGCTGACGGGAATCATTGACTTTTATCTACCAGCTGACCTGACGCCCAATCAGCTGACGGATCAGCTGACCGATAAATTATTGTCAAGTACGTCGGGTTGGCAATCAGCATAAACCCTGGTTAGTAACCAGTGTGGGTGTTACTGGAGAGGGTAACTATTCAGCTGACCGCTTCAGTGACTGCTGACTGCCGCTGACGTTGACTGCACCAGCGAAAGCGTGACTGCTCATGTCCTATGTCTCTCGGTGACTTGACTGCCTGCGCTGACACGCCTTGACTGCGACAGCTGACCGTTCTTGACTGCGACTGCGCATGACTGCAGCAGTGACTGCTTGACTGCGCTGATGACTGCCCCAACAGCTGACTTCCACGCCATTGACTGCTGACATGTCGCTATACTAGATTGACTGCGCTGACTTTGCCAAGGCTGAGCTTGACTGCGAAGGCTGACCAGTATTTATTGACTGCAGCCAAGGCTGCAGAGAAGGCCAATATTCCACCAAGGCTGCTCTCCACCAAGGCTGTGAGCCCGCGTGACCAAGGTGACTGCAAGGCTGACTGCAGCCCCGTGGCTGTGACTGCAAGGCTGCCCAAGGCTGACTGCGCTGGTTTACCGGCGCCACCAAGGCTGCTGACCAATGACTGCTGACTGCGTTAACATCTCTGACTGCCTGCGCTGTACCAATGACTGCTGCTCTAGCGTGACTTGACTGCAGCTGCCTGACTGCATTCGGACCAAGGCTGGGAATGACTGAGTCCCAAGGCTGCTGGCCATACTCAACAAACCAAGGCTGACGGGAGAATTGCACCAAGGCTGACTAATACCCAAGGCTGAGGCTGGAAAACCAAGGCTGAAAAAAAGTGCCGATTCCAGCGTCCACCGCCGGCCACAAAAGTGGAGAGTTTAACGGCCTTTTCGATTTAACCCGGGTATGAAGGTCGGCACTGTCGTGGATTACGACTCCTGCAGGTAAAGGGACATTGAGCGAGCGCCATAAGGGGTTGGAATAGCCCCATTTAACGACGTCCCCTGACGAGGCAGCGTCGGAAACCTCTTAAATGCTTTCCATGGTGGTGGCCGATGTAAAATATATGATTACGAGGACTGGTTTGCTATGCGTTGAAAAGTAACCGTAACCGTAACCGCTCGTTCGGTAACTAACCGCTCGACAGCGGACTCTGGTCTAAAGATGTTAACCGCTCGAAAGGGGTAACCGCTCTGGGTAACATAATATAATTTAACCGCTCGATAATTTCTCTCGCACTAACCATAATTTCTCCTCCTCTTAACCGCTCATAATTTGGAAATGTACCAGAATAATTTATAATTTCTCGCTCCGAGACCCCCTCATGTCTGGGCCATAATTTCTTATAATAATTTTCAATAAATAATTTTTATAATTTTACACCCCCAACTTAATATAACCGCATAATTTTAATAATTTGGCCCTAACCGCTCATAATTTCCGCTCCGTAACCGCTCCGCTCACCGATAATTTCATTTGATAATAATTTCCGTAACCGCTCCTATAATAATTTTATAATTTTGATAGTTTGATAGATAATTTATAATATAATTTTAACCATAATTTAAATAATTTTAATTTCGCTCAGGCACGCGAGGTTTGATAGGGATAATAATTTTTTCATAATTTTTGATAGGGGTCCTATAATTTTGAATAATTTATCTGAGGATATTTGATAGGGAGCATAATTTTTGATAGGCCCGTCATGAGGAATTTGATAGGGGGTTAATCTTATAATTTAGATAAATTTGATAGTACACCACTGGGTGTCTTTGATATTTGATAGAAAAACCCGCGATACGCTGAGGTCGAACCGATGCAGTACCTCGCCTTTTTGATAGGATAGGATAGGCCGACTTTGATAGTTATTCATAGTTTGATAGGGTTTGATAGCGATGTAGTGTCGAATTTTTTTGTTTGATAGCCTTGGGCGTACGTTTGATAGTCCATAGGTTTGATAGTCTATTTGATAGGGTTTGATAGTGGTAATGATTTAGTAGACCCTGGGGTAAGCCCTTCGAACGAGATGCTGAAAGCCGAACCTGGAGTGATGAGGTGTCCAGTCAAGTACGACTCCCATTGCTTGGATCCCATATGCTAGTATCCGGGCTGCAATTCATCCTCCGCACTATACGAATCAGGTGGCACTCCTCTTGAAACACCATTGCATCCTATGCTATACAGGTTTATTCCTAAAACTAGATCAGTCTCATTCGAGGTTATGTCCGTCGAAAGCTCATGCCGAGATCATCACAACAGGTACCACGTATTCTCGCTGAGCCATCATCTAGCTGCCCTTAGAACTGGGATAGAGAGACTATACGTGGCGGGTTTCTCACTCGTGCGCATTCCGGAAAACGACTAGGGTCACATGGCGTGGGGCCGTGAGATCTTTGAGCCAGATCTTAATCCACGTACCTAAGCGCGCTCCCCATGGCCACGGTCACCTATCGAGTGTTCCTGGCTATCGTCGTTCCACGGTGAGACTCCCATCTGTCGGCACGCGGAAAGATAACTATGTCCAAGAACAAAGCCGTTGGTGGACTAATTGCTGCTGTAAGCTGATGATTACTATCCAGATTATGGAACGTCGATTGGCTCAGCATTAGAATGCAAATTGAGAAACGTCTTTCCGGCAGGTCTGGTAGACCCGAGATGACGAGACCAGGTGATTAACAAGTCCACTCGACAGCGATAGTTAGACGCAGGAGTCTGCACCCCCATGCCATAACCCCAAAGGCTTTTTATGGGATGTGGGAATATTTAACGAACCAAAGGTAAGTTTATTTTTGGAAGAGGGCTGCAGCAGACGATTCAAATGCAATGGTAGGGTCTGACCTAGGCAGATGCTAGGAACTACTTAAAAGTCAGTTTGGATAAATAGTATTAAAACACAGTCCCCTCTGCAGGGGCCACTAGACTCGCGGGTTGAGTGCTCAGCATGCTTTAGTGAGGGGCGAAAGAGTGCGGCGTTGGCAATGCTGAAGGTGAGCGTATGCCTTATTTGTATCCAGGAACGTACCTGACGACTATGATCGAGCAAGCAAGACCGAAGATTTCGACACACTATGGTAGGATTGAGTACGCCAGTACTATGAGTAGAGGGATGTGACTCTATCTGAGTCGACACGTCCCGTCGCGTTTAAAAACAGAGTAGTCCTGTTAGTCGGGTCGGGTTTCGCACCACTACATTCTATAGAGGTCGGGCCTTAGGCCATTAAGCATGCTCCAGATGAAGGTTAGTGGGCCGAGGATCAATGATCATGGCCAGGGTGTTAGTCAGGACCTCAATGATAAGGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACGACGCCACCTTCTCGCGGTCAGCGGTCAAATTGCGGGCGGTCAAAGGTCAAATCTCACTTCTCACTTCTCACTTCTCACTTCGCGGTCAAAACTTCTCACTTCTCACTTCTCACTTCTCACTGCGGTCAAATCACTTCTCAGCGGTCAAATCTCGCGGTCAAATTCTCACTTCTCAGCGGTCAAAGGTCAAATCTCACTTCTCACTTCTCACTTCTCACTTCTCACGCGGTCAAACTCACTTCTCACTTCTCACTTCTCACTTCTGCGGGCGGTCAAAAGCGCGGTCAAAGCAGCTTTGGGCGGTCAAAGCGGTCAAACAGCTTTGGCAGCTTTGGCAGCTTTGGGCGGTCAAACAGCTTTGGCAGCTTTGGCAGCTGCGGTCAAAGCGGCGGTCAAAGCAGGCGGTCAAACTTTGGCAGCTTTGGCAGCTTTGGCAGCTTTGGCAGCTTGCGGTCAAAGTCAAACTTTGGCGGTCAAGCGGTCAGCGGTCAAGCGGTCAAATTTGGCAGCTTTGGCAGCTTGCGGTCAAAGCGGTCAAATGGCAGCGGTGCGGTCAAACGGTCAAAGCGGTCAAAAAACAGCTTTGGCAGCTTTGGCAGCTTTGGCAGCTTTGGCTGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAATGGCAGGAA"
    val kmerLength = 9
    val clumpSize = 540
    val howManyToFormAClump = 19

    val clumps = allClumps(genome,kmerLength,clumpSize,howManyToFormAClump)

    val clumpsSet: GenSet[String] = clumps.toSet
    assert(clumpsSet === Set("GCAGGAATG", "AGGAATGGC", "ATGGCAGGA", "GCGGTCAAA", "TGGCAGGAA", "ACGCCACGA", "CACGACGCC", "GCCACGACG", "GGCAGGAAT", "CGACGCCAC", "CAGGAATGG", "CGCCACGAC", "AATGGCAGG", "GACGCCACG", "TATAGCCTC", "GAATGGCAG", "GGAATGGCA", "ACGACGCCA", "CCACGACGC"))
  }

}

