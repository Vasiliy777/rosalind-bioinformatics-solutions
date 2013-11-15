
package proteins

import org.scalatest.{Matchers, FlatSpec}

class ProteinTranslationSpec extends FlatSpec with Matchers {

  "DNA" should "be translated to Protein_example" in {
    val input = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    val output = input.sliding(3,3).map(new Codons().codonToAmino(_)).mkString("").trim()

    val expected = "MAMAPRTEINSTRING"
    assertResult(expected)(output)
  }


  "DNA" should "be translated to Protein_testCase" in {
    val input = "AUGGGGUUAGCGGUCCGAGACAACCGCAUGGCCGUCAGAGCCGGGCCAGUUAUUCUGUCUCGUAGUCGAACUGUUAUCAGGCGAAGAGAUGAUUAUGGGCAGCUCAGCGGGGUUCCUUGUCGCAACUCCCCCCGCAAUAGAGCGGCAGCCAAGCCACAGGUACGCUGUGCCUAUGAAGUCAGAAUAGCAGUGCACGUCACAGUUUAUGGAUUAGUCCCUACCAGACACAGUGUGGGCUCCCGCCGACACAGAGUUACAAUCCUAAUGACCCCUCUCUGCAUUGAACCUAAUCUAUCGUCACAAGGUUAUUCAGCGUGGGCACAAGUGGCGCUAAAGUUGAGCGAGACGCGAUCACAGUGUCCAUGCUGCCAGGCGGCCUACGUGUACAUCGCACAAUUAUUAGAGCUACGAACAAAAUUAGUGGAAAGCGAAGAUGGAGGCGGCUGGGGUCCGAACUUGUGGACAGAAUACUCCGAUAUGUAUCCGCGGAAUAGAUUAUUCGCAGUACCACCGACGGUUGGACGAUCACCAUUUAGGGUCGGGACACUCACUUGGCACUUAGGCUCAGGUCUCAGAGCGCACAAGAAUAAUUUUCCACACGAUCCAGAUCCAGGCAUGUGUCACCCACGGUGUAAAACGGUAUAUUUCUCUGAGCCAGCAUCCCUAUGGCAGCUAUCCAAGUCCACCACCUAUAAUAUAGGACCGAGCUUCCGAUUAUUUGGGCUGGCGGAGCCGUUACAUAUGACGCUUCCCGAAACGAUGAUGUGCCUUGUAAAGGCUAAGAAGUGCAGGAGAUCACCCACAUCAGUAGUAACUAGUGCUCAAUCCGGGGCUGAUAUAGCUUGUGUUGUGUAUAAACUAUUCGCUUUAUUUAUAGGGGAGCGGCUUACGGGAAUACUUGGCGCUCGUCGUCUAGUCGAGUUUACACUCUUCGUAAUGCGUAUGGUUGUCGAAACAUCAUGUCCACUUUUACUAAUCCAAGGCUGUGAUAUGCCGCUGUGGAACGCUCUCAAUUAUACUACCAUGUCGUAUGCAACCUGGACGAUAGGCGAAUGUCCGAUCCGCCGAGUGCUAACUAAGAAGAUAUACUAUCGUUCGCUGGAUCAAAUCGGAGACGAAACGUGUACGUACCUUACGAUUCCCUCGAAAAUCCAGCACCAGGAGAGAUACGAGCUUUGUCGGCCGCCGUCGAGGGAUAGUCACUCACUCCCCCGACGGUUUUGGAAAGAUAGUACCGAAUGGUGCACCACGGACCGUACGCCCCCCUAUUGCACUUUCUUUACAUAUUUCUGCAGCUUACGUGAGCAUUCCGAUGAGCGUGGUUGGACUCUGGUGUGUGGUACCCCGGGUAUUUGUACGCGGACCAGUAUCACAUAUGCCACCUUGCGCUGCUACGACGUUCGACCAAAACAGGGCAUGACUAACCGCCGGCUGUGUAACAUUUCGACUAAUAGUUUAGUACUUAGACUGCAGUUUGGUGUCUGGUGCACCCACGUUGGUAUAAACCGCGAAAAAGGAGUAGCGACCGAAUGCCCUUGGCAGACAUGCUGUAGAAGGAACAGCAUAACGACGUUAAGUAUAUUCAUUACAAUUUUAGUGGGGCAGCCAAACAUCGACAAGGUUGACGAUUUCAGAUUGGCGCUACUAGCCAUACAUUCAUUCGGUGGGAGUCUUCAGCAUAUAACACCCAUAAGUGCUUAUUUACCGCGACUUUUUAUCAGGCGGUGGGGCACCGUUGACGCCUUGUUCAGCUCUCGGUGGUUUAGACCGUUCUGCGAAGCAUCCUCCCGGCUAAACAGGCAGGUUGCAAGAAAUAGUUCGGUACAGGACUAUUUCAUGUUAUCCGUGUGGGGAAGACUAAAGCCGCAACGAUCGCCGAAAGUGGGGUCACAACCAUACUUCAAGCUCCCGGUAUUCCCGCCGGCUUGCCAUUGCAGGCGGCUUAGCGAUUGCGCUACAGAGGGAUCGUGCACGGGUACGCAUGCGUUGUCAAGUACCUGUGUGGUGGUAAAAUAUGCAUGGAUCGACAAAUGGCAUGACUCCCUCAAGACGGCCUCGUUGCCUGUGGUGGGAGCUACCCAUGCAGUUCUACCCAACCCGGGGCCCGCCCCGGAACAGCCCAUAAAGCCCCGUGUUCCGACGGCACUGUCGGCCAGUUCACGAGAGCGUCUUAGUUGUAUUCGGAUUGUAUUUGAAGCAACCUGUUUACAUACAACCCGGCGGUGUUGGCCUCCAAGAAAGCAGAUAGCCCUAACGGAGGCCCUGUGGGAACCUCUGCUCGUGAGGAUUAACCAACCGUGUAUCGUUAAGAAACCCACACAACUAAUGCCCCAGUGGGCCGGUGCUGAGCUGCCUCUAAUAUUCAGCACGACACUUCGGCGACUUACAUCUGCUAUACGAUGUAUCGACGUUCCUACAUUGAUGAAUGACCUUUAUUCGCACUGGUUGUGGGUUUCAUGUAUAGAGCGCCUUUUCCUAGGGUUCGUCUAUAUCAAGGGUAGACAUGCGGAUAGCGGGCCGAGGCCUAACACCUACUUUCGUUCACGGGAUCGGAAGGCCUCAAGAACCUAUGACGGGGCACGAUAUAUAGCCGGGCUGCCUACGUGCACAGGCGAAGCUGGCCACACCCACGCCCUAGGGAUGAAAACCGGUCUGCGUUUCACCCAGCUAUCGCCCCAGUCACAUGCGUCCGUUAGCAGUUUUAUCACCCGCUUACAGUACUUUCACAGAUACGAUGGUUCCGCCCGAGAGUAUACGUGGUUGUCCGAACGCGUGGGCCUACAAGGGAUCGUUAUCGCAGACUUAGAUGCGCGCGUAUGCACCCUUUUUGCGGCUGAGUGCGACGGAGCUCCAGAUACCAGAUGUUGGAGAGCUACCAUGUGCUUGCGUCCCUAUAUCGUCACCGAGGGGCAACGGCAAUUUCCCAGCAUGUGUAUUCAGUCUCUCCUUCUCCAACCUGCUACCAAUACACCGAAUCCUAGUAGCACGUUGAGUUCCAUCGUGUACGCCUUUACCACGAAUGUUGGGCAGCUAGAUAUGAUACAUAGAGUAGUUACUGGAGGGGAGCACCAAUAUACCAGACCCGCGACCCGCCCAGGCCAAAAGGGGUUCUCGAAUGAAGAACCUCUCCGGGGAACAGGCAUAGAAGGUUCGUUCCGUGCAACAUGCAUACGUACGUAUACCAUCAAAACAAAAAUAAGGUUCUACGGCAGACAAAAUAGCGGCAACCGUCAUGUAGAUUCCCAGUCGCGAUGGGAGGUCGUGUGGCGUCAGGAAAUGAACGUAGCGUGUAAACUUUCGUGCCUGCUUGCAGUACCUCCUACACAAAUUCAACUUAGACGCUCUAAACGUAUGUCCGUAUGUCUGUAUGGUACGUGUGGCCGCUCAUACGACCUCAUACGCAGGUCGGCCGUGACAGGGUUAUCCAAUACACUAACGGGCUACCUGUACCGUUUCCCAGCAAUACUAUGUAGACUAAAAUGGAGGGGACGAGUAAGUGGACUUAGCCGAUGUCGUGUGUUCACUCGUUUCGGCUUGGUCGAGUGCCAAUACUCCGACGUAACAACGGUGAGGCAUCGGGCGCUUAGCCAACCUUCGUCGAGUCAAUAUGGCCCAAUUCCACGGCGUGCUCAGGUGCUCGCGCUUGGGAAAGGUGCAGAAGCGAAAGGUGGAGUACAAUGGUCCCCGGCGGGGUUAUUCUUUCACAUCAUGGCAUCUUGCACAACAGAGUACAAGCUAUCUGUGCUUACGCAUGGUAGUUACUAUUGCCACCUAACUACAUUUUUGGCGCGACGACCAGGGCAGCGACAGCUGACGCGCAGUAACCGGGGGGGUCCACGAGUAUCGGGCACGCAAUUUAAGUGCCAAUGUCUCAAAGUAAGGGCUAAAUUCGGGGGCACCAAAAGUAGUUGUGGCAGCAUGAUAAUGUACGAGAUUACACCUGAGAUCCCCUCCCUCCCGGAACAAGUUGCCUCUAGAUACCAAAGAUCUGAAUCCAAGGUGGCGAACGCUUCUGAAAUAACUCAUUCCUCCGCCAUCGUAACUUAUGCGUCCUCUCCGCGAAUGGUUAGUUGGAUAGGCCCUUCAACCGGUACAAGAUUUAAUCAGGGUUUUGUCACUCAGACGAGUGUUACCAUUGCGCCUACAGUGUCACGGGCUAGGACGGCGAUGCGUGUACAUGAGUUUACCGAUAUUGCAGCUCUGGUUGUUCUUAACGGCCUUAUGGUCCAGCUGAAACGGUGCCACGCUGUCACCAGACCAAGACUCAUGGGGAUGUUUGAUACAUACAGGCUAGGCUGUAAUGACAGACGUUAUUGGCGAGUUCACGUGUUUGUUGACCCGAGCGCGAAUCACAACACUCGAGCUUGCCAAGAAGUUAUUCCUUGGAGAGCUGGUGUCCAUCGACUUGCAUCGUACCUGGCCUUACACUUCCCUCUGCUAAACUGUAUACCAACUUAUAGUAGCAUGAUGAUAGCAUCUACGGUGGAUAACUCUAUUAUUGUAAAGCUCAACCAUUCCGCUUGCUGGGCACCGCCGAGUCAGCUAGGUGUCACCGUUGUUACUCGUUCCAUAUCCCACGAAAUCGCGGUGCCGGAACCAUUGGUGACCUUCCAGCGGGGCGUAGGAGUCCCAAAGACCUCCCUCAUAGCAUAUUGCGGAGAAGUCCCAGGUAUGCGAUUUGAUCCGCUGUGGAAGGUAUUCUAUCAGGAGCAAAACGUCCGGGCCUAUGACGGAAUAGAUAACCAACUUUAUGAAUCUGUGAAGACUCGCAUCCCUACUCGAAGCCUGCUUUUAGCAUUCAGAAAUCGGCUUAUAUCGUGGUUGGUAUUACGAUGGGCUACCUGUGCAGAUAAAAGCGGCUUUGGGGCGUGCCUGUUGAGGCUCAGGAUGAGCUAUUUGUUGGUGCGUCACUGUCAGGAAUGCGCCGAUCUAACGGCCACAUUGUCUGGAUUGGGCAACGAGCAAUAUUCUUGUUCCGAGGCAUUCACAACAGACGAUGUCCACGGCUCGCUGAACUCUACCCUUACCUUGCGGUUACCGCGUGUGUUGGACUUGCCUACCGCGUCUCCCCGCACACGGCACUCCACAAUUCAUCCCCCGCGUCGAACAUCGCCAAACUAUAGGGACAGAGGCUUACUGUGGGGGAAUACAGCUCAUAGCCCGGAUUUCACUAAUCAUGCGCACAUUACAUCCUAUUUUCGGGAUGUCGAGCUAGGACUUUUCUAUACAUACUGUCUUGACGGACCCAUAGGCAGUUUAACGGUGUGUAGCGUAGUGUGCAGAGUGUAUGUAGCGAUGCCUCAUCUGCGUGUCGUGACGUCGGGUACCUGUACAGCGCCUAGUUGUUUCUGCACACGAAUCACAGGGGUUGCCAGUCGACAGACACGGGCGACGUGGUGCAGUAGUUUCUGGGGACUGUGGGCGGUGCAAAAAGAGAAGUUAGUUCUGUAUGCGCCGCAGUCUACGUCUGAACCGCGUAUGGUCAUCUCUAUAAUGUUCCGCACAAGUUCAGGGGUAUGCAACCAGUGUGGCACUGGGCUCGCCUGCUUCAAACGACUCGCCAUAGAGUCCCCUGAUAAUGUCUCGUAUUAUUGGUGUUGCCAUCGAGAUCAAUGCGUUACAUCCAUUCUGUUUUUGCUAUACGUGCCCGUAUACCGUGAAUGGGCCCGAUUUAUGCGAUCAUACCCGGUGGUGGAGUCACAUCAGCAUCGUAUUCAAAGUAGGCCGACCUGCUGGAUGUUAAUCCACGUCGUUGGACAAAACUCAGCGGGGACGACAGUAUCUCCGGAGGAGGGAACGGAUAGGGCACCUCCGUUCCUUUGCACCCGCUUGAUGUCUAAUCAAACCUAUGUAACCACCCCUGUAGGACACUUUAACAUUUUUCAUACUGGAAGGCCAGUCCGCUGGUUGUAUCGGAAAAGUACUAUUUGGCAGGCACACAUAUACGAAUAUAAAACGGUACUUGGCCUGCCCUGCGUAUUCGAAAUGAUUCCCUUGUUCUCCGCCAGGCAGGCUGUUAUUGAACGUAUAAUGGCUAUUAGAAGGGACUACAAAACUGGAACGGGUGGUCCUUGUCAUCGACCGGCGCAGGUAAUUGCGAAUCAUGGGACCACGGGCAAAAGUGGAUUGGCAACCCGGCGCCUCCCUCGCUGCUUGAUCUACGGGUAUCCUAUCCUCAACGGGACUCCUCUACUCGGGGGGAGAGGCGGUUUAAUGAACGGCGUGCGACGCACCAUUCAGGACAUCCCAAGGAACCGGCUUCGCUACUUCCACCUUGCCACUUUAAGGUUGAGAUGGCUUAUAGGGUCUCUAGACUAUACCCAGAUGUCGCGAGGAGAUCACUCCCGAGCUUCCAUGAUAACCCCUCUACCGGCACAGCACAGAGGUACCAGCCUGCCAACGGUAAUAGUAGGGGUUUUCGAGAAGAAAGGCUAUCCAGAAGACGUAUCAAUUGUCAAUCCGAGAGUGCACUACAGGUAUCCCAGCGCCGCAUACACGCUGCUUAUUCAUCGUAUCAGCUGGUGGUGUCAUAACACAACGUAUGAACGUUGUGACCAAUCAACCGUGCGGAGAAUAACGAUGUUCCUAUCGUCUUUACUAUAUGGGGGGAAGACGUUUGCUACCACACAUGGGUCGUCUCGAGGGCUAACGCCACAGGCACGGGGCGUAUUUUUUCAGAGUAUUCACUCAAAUUUCGUUAUAGAGGUAGUCCGACCCCAUUUCAUAGGCCGUGAACUGGAGCCUAGGACCUACCACCAAGAUUUCAUCGACGAUCUUCACGAAAUAUUAGAGUUCAACGAGGACACUGACCAGGCCCGUCGUACAUACAAGGUUGCAACUGUCAGCAUUGACCCGAUCACGCUACCCGAACCCAGACACAUAGGGCCCGACAACACCCACCGGGGCUCAUAUUGGGCUCUUAAGACAAUGGGUCAUUCAUGUGGCCACGACAUUCUAGCUAUCGGAAUCCCAGUAACCGGGCCGGUAUUAUCCAUCUUACAUGCUCUUGGUCAUAGCCCCGUUGGAUCACGUGGCAGCAUCCAUCUACGAUGUCCGACUAAUGUGUGUCUAAGAGUGAGAUUUCAUUCUGCGGAGGUUCGAAACGUUAACACAUACUCCCCGAGGUAUCGUUCAAAAGUAAUUGAUCACAUCCAUGCAGAGGUGGUACAGCAUGUCCUAGUGAGGUUAGUUCUCUCCCUGACGACCACCGCGUUCAAUCAUGCAGUGGUGGAAUGCUUCGCGUGCGUUCACUGUAGGGGACGGGAUGUUCUCGACAUGAGCAGCUUGACGUAUAUAUCCAGCGUAAUUUGGACCUCAGUAUUGAAGUUGGGGGCGCUACUAAAAAGUUCUUCGCUGUUGGCAGGUAUGCUCUGGGUAACCAACGCAAGCAAGUUUGUAUAUCUCUUCUUGGCGAAGGUGGGCGAAAAGGAUCUGGUUUGUGGGCACGUAAGGGCUCGCGCAGCGUGCUCGGCCACACCUCUUUGCGUCCAUCAUAUUUUUGGAGAACGAACAUCUCUAUCCGCCUCUCAAAUGAGAUUUCUCGACUCUUUGUUUGUCGGGGGGAUCUUGUUCCUUUCCAUGUUCCUGUAUGACUUACGCAAUACGGCAGUUUGCUGGGUCCGGAGCCACGUCUUGGUCGUGCUACAGGGAAGAGGCUGUGGCUUCGCCGACGCGCCGACGUACGACCCUUUCAACAUUGCCUGCGCUGAUACAAUCUUAACAUUAGAAAAAUUUGACUCCAGUAUGGUUCGAAAUAAGCCCGGACGAGUCGGAGCCAUGCGGCAACCGGUCAAUCGAUUAUGUUUUCCAGGACAAGAUAUGCUAUUGAAGGGCGAAGAUGCUUCAGGCGGAGGGCAACGCCUAAGGUCCUCAAAGCUUGGGCAGCCCCCCUCUGAAAACGCGUUAGCCCACACGCCAAUACGGGACAAUUUAGGUGUUGAAGUAUCUAAUGGUUGGUUUUCCUUUAAACUCUGCUAUGGAGGUGGUGUCGUGACGAAUUUAACACAGACUCGAACAACCAGACAGUUCUACCGAUGCGUGUCGGUCACCCAUUCAAAGUGGGAUUCGGGGCUCACCUCCUUCGUCCACUUGUUCUUUCAGUGGGCCAUCUCGGACGCCGGACAGCAGCCGCUGAACCGUGGGCCUCUAGCCGUUGACGGACCACACGGUCUCCUGUGUGGCAUAAGAUAG"
    val output = input.sliding(3,3).map(new Codons().codonToAmino(_)).mkString("").trim()
    val expected = "MGLAVRDNRMAVRAGPVILSRSRTVIRRRDDYGQLSGVPCRNSPRNRAAAKPQVRCAYEVRIAVHVTVYGLVPTRHSVGSRRHRVTILMTPLCIEPNLSSQGYSAWAQVALKLSETRSQCPCCQAAYVYIAQLLELRTKLVESEDGGGWGPNLWTEYSDMYPRNRLFAVPPTVGRSPFRVGTLTWHLGSGLRAHKNNFPHDPDPGMCHPRCKTVYFSEPASLWQLSKSTTYNIGPSFRLFGLAEPLHMTLPETMMCLVKAKKCRRSPTSVVTSAQSGADIACVVYKLFALFIGERLTGILGARRLVEFTLFVMRMVVETSCPLLLIQGCDMPLWNALNYTTMSYATWTIGECPIRRVLTKKIYYRSLDQIGDETCTYLTIPSKIQHQERYELCRPPSRDSHSLPRRFWKDSTEWCTTDRTPPYCTFFTYFCSLREHSDERGWTLVCGTPGICTRTSITYATLRCYDVRPKQGMTNRRLCNISTNSLVLRLQFGVWCTHVGINREKGVATECPWQTCCRRNSITTLSIFITILVGQPNIDKVDDFRLALLAIHSFGGSLQHITPISAYLPRLFIRRWGTVDALFSSRWFRPFCEASSRLNRQVARNSSVQDYFMLSVWGRLKPQRSPKVGSQPYFKLPVFPPACHCRRLSDCATEGSCTGTHALSSTCVVVKYAWIDKWHDSLKTASLPVVGATHAVLPNPGPAPEQPIKPRVPTALSASSRERLSCIRIVFEATCLHTTRRCWPPRKQIALTEALWEPLLVRINQPCIVKKPTQLMPQWAGAELPLIFSTTLRRLTSAIRCIDVPTLMNDLYSHWLWVSCIERLFLGFVYIKGRHADSGPRPNTYFRSRDRKASRTYDGARYIAGLPTCTGEAGHTHALGMKTGLRFTQLSPQSHASVSSFITRLQYFHRYDGSAREYTWLSERVGLQGIVIADLDARVCTLFAAECDGAPDTRCWRATMCLRPYIVTEGQRQFPSMCIQSLLLQPATNTPNPSSTLSSIVYAFTTNVGQLDMIHRVVTGGEHQYTRPATRPGQKGFSNEEPLRGTGIEGSFRATCIRTYTIKTKIRFYGRQNSGNRHVDSQSRWEVVWRQEMNVACKLSCLLAVPPTQIQLRRSKRMSVCLYGTCGRSYDLIRRSAVTGLSNTLTGYLYRFPAILCRLKWRGRVSGLSRCRVFTRFGLVECQYSDVTTVRHRALSQPSSSQYGPIPRRAQVLALGKGAEAKGGVQWSPAGLFFHIMASCTTEYKLSVLTHGSYYCHLTTFLARRPGQRQLTRSNRGGPRVSGTQFKCQCLKVRAKFGGTKSSCGSMIMYEITPEIPSLPEQVASRYQRSESKVANASEITHSSAIVTYASSPRMVSWIGPSTGTRFNQGFVTQTSVTIAPTVSRARTAMRVHEFTDIAALVVLNGLMVQLKRCHAVTRPRLMGMFDTYRLGCNDRRYWRVHVFVDPSANHNTRACQEVIPWRAGVHRLASYLALHFPLLNCIPTYSSMMIASTVDNSIIVKLNHSACWAPPSQLGVTVVTRSISHEIAVPEPLVTFQRGVGVPKTSLIAYCGEVPGMRFDPLWKVFYQEQNVRAYDGIDNQLYESVKTRIPTRSLLLAFRNRLISWLVLRWATCADKSGFGACLLRLRMSYLLVRHCQECADLTATLSGLGNEQYSCSEAFTTDDVHGSLNSTLTLRLPRVLDLPTASPRTRHSTIHPPRRTSPNYRDRGLLWGNTAHSPDFTNHAHITSYFRDVELGLFYTYCLDGPIGSLTVCSVVCRVYVAMPHLRVVTSGTCTAPSCFCTRITGVASRQTRATWCSSFWGLWAVQKEKLVLYAPQSTSEPRMVISIMFRTSSGVCNQCGTGLACFKRLAIESPDNVSYYWCCHRDQCVTSILFLLYVPVYREWARFMRSYPVVESHQHRIQSRPTCWMLIHVVGQNSAGTTVSPEEGTDRAPPFLCTRLMSNQTYVTTPVGHFNIFHTGRPVRWLYRKSTIWQAHIYEYKTVLGLPCVFEMIPLFSARQAVIERIMAIRRDYKTGTGGPCHRPAQVIANHGTTGKSGLATRRLPRCLIYGYPILNGTPLLGGRGGLMNGVRRTIQDIPRNRLRYFHLATLRLRWLIGSLDYTQMSRGDHSRASMITPLPAQHRGTSLPTVIVGVFEKKGYPEDVSIVNPRVHYRYPSAAYTLLIHRISWWCHNTTYERCDQSTVRRITMFLSSLLYGGKTFATTHGSSRGLTPQARGVFFQSIHSNFVIEVVRPHFIGRELEPRTYHQDFIDDLHEILEFNEDTDQARRTYKVATVSIDPITLPEPRHIGPDNTHRGSYWALKTMGHSCGHDILAIGIPVTGPVLSILHALGHSPVGSRGSIHLRCPTNVCLRVRFHSAEVRNVNTYSPRYRSKVIDHIHAEVVQHVLVRLVLSLTTTAFNHAVVECFACVHCRGRDVLDMSSLTYISSVIWTSVLKLGALLKSSSLLAGMLWVTNASKFVYLFLAKVGEKDLVCGHVRARAACSATPLCVHHIFGERTSLSASQMRFLDSLFVGGILFLSMFLYDLRNTAVCWVRSHVLVVLQGRGCGFADAPTYDPFNIACADTILTLEKFDSSMVRNKPGRVGAMRQPVNRLCFPGQDMLLKGEDASGGGQRLRSSKLGQPPSENALAHTPIRDNLGVEVSNGWFSFKLCYGGGVVTNLTQTRTTRQFYRCVSVTHSKWDSGLTSFVHLFFQWAISDAGQQPLNRGPLAVDGPHGLLCGIR"

    assertResult(expected)(output)
  }

}