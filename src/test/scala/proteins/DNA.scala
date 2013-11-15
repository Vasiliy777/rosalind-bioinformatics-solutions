package proteins

import proteins.DNADirection.DNADirection

class DNA(val direction:DNADirection, val genome:String) {

      def secondStrand:DNA = new DNA(DNADirection.oposite(direction), complement(genome))

      def toRna:RNA = new RNA(direction,genome.replace('T','U'))

      def shiftedUpTo(window:Int) = for (shift <- 0 to window) yield new DNA(direction,genome.substring(shift))


  private def complement(c: Char): Char = {
    c match {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
    }
  }

  private def complement(genome:String): String = {
    return genome.map(complement(_)).mkString("")
  }
 }
