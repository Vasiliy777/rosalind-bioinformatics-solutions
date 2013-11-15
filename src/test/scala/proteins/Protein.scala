package proteins

import scala.collection.GenTraversableOnce

class Protein(val aminoAcids:Iterator[AminoAcid]) {

  def toShortcutString = aminoAcids.map(_.shortcut).mkString("").trim()

}
