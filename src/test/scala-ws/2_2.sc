val genome = "ATGGCCATGGCCCCCAGAACTGAGATCAATAGTACCCGTATTAACGGGTGA"
val peptide = "MA"
val nucleotidesToEncode = peptide.length()*3
val output = genome.sliding(nucleotidesToEncode)
output.mkString(" ")



//output.map(new Codons().codonToAmino(_)).mkString("").trim()

print(output)
val expected = List("ATGGCC","GGCCAT","ATGGCC")

output.equals(expected)



