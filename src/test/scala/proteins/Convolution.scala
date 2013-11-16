package proteins

class Convolution(val spectrum:List[Int]) {

  def getAll:List[Int] = {
    combinations(0 :: spectrum).map(pair => Math.abs(pair._1-pair._2)).filter(_>0)
  }

  def getTop(howMany:Int) = {
    top(getAll.filter(_>=57).filter(_<=200).groupBy((a:Int) => a).mapValues(_.size).toList.sortBy(_._2 * -1),howMany)
  }

  def top(masses:List[(Int,Int)],howMany:Int):List[Int] = {
    if(masses.size > howMany) masses.take(howMany).union(masses.drop(howMany).takeWhile(_._2 == masses(howMany)._2)).map(_._1) else masses.map(_._1)
  }

  def combinations(spectrum:List[Int]):List[Pair[Int,Int]] = {
    for (firstFromSpectrumWithIndex <- spectrum.zipWithIndex; b <- spectrum.drop(firstFromSpectrumWithIndex._2)) yield (firstFromSpectrumWithIndex._1,b)
  }

}
