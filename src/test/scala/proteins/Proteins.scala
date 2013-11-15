package proteins

class Proteins {

  def allPossibleProteins(dna:DNA):IndexedSeq[Protein] = {
     dna.shiftedUpTo(2).union(dna.secondStrand.shiftedUpTo(2)).map(_.toRna.toProtein)

  }

}
