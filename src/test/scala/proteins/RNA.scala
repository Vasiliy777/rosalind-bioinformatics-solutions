package proteins

class RNA(val genome:String) {

  def toProteinString():String = {
    genome.sliding(3,3).map(new Codons().codonToAmino(_)).mkString("").trim()
  }

}
