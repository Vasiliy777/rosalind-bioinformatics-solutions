package proteins

import proteins.DNADirection.DNADirection

class DNA(val direction:DNADirection, val genome:String) {

      def secondStrand:DNA = new DNA(DNADirection.oposite(direction), reverseComplement(genome))

      def toRna:RNA = new RNA(direction,genome.replace('T','U'))

      def shiftedUpTo(window:Int) = for (shift <- 0 to window) yield new DNA(direction,genome.substring(shift))


  private def reverseNucleotide(c: Char): Char = {
    c match {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
    }
  }

  private def reverseComplement(genome:String): String = {
    return genome.reverse.foldRight("")((nucleotide:Char,reversed:String) => reversed + reverseNucleotide(nucleotide));
  }
 }
