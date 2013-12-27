package assemble

class DeBruijnGraph {

  def createGraph(k:Int, genome:String) = {
    val kmerList = (genome + "X").sliding(k,1).toList
    createGraphFromKMers(kmerList)
  }

  def createGraphFromKMers(kmerList: List[String]): Iterable[String] = {
    val simpleGraph = for (kmer <- kmerList) yield new Binding(kmer.dropRight(1), kmer.drop(1))
    simpleGraph.toList.groupBy((a: Binding) => a.left).mapValues((bindingList) => bindingList.map(_.right).toSet.toList.sorted.mkString(",")).map((entry) => entry._1 + " -> " + entry._2)
  }

  class Binding(val left:String,val right:String) {
      override def toString = left + " -> " + right
  }

}
