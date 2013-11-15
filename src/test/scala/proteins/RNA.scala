package proteins

import proteins.DNADirection._

class RNA(val direction:DNADirection, val genome:String) {

  def toProtein:Protein = {
    new Protein(genome.sliding(3,3).filter(_.length==3).map(triple => new Codons().codonToAmino(direction match {
      case FIVE_TO_THREE => triple
      case THREE_TO_FIVE => triple.reverse
    })))
  }

  def toProteinString():String = {
    toProtein.aminoAcids.map(_.letter).mkString("").trim
  }

}
