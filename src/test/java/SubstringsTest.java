import com.google.common.base.Splitter;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

public class SubstringsTest {

    @Test
    public void shouldFindAllSubstrings() {
        String genome="GATATATGCATATACTT";
        String substring = "ATAT";

        List<Integer> substringIndexes = getSubstringIndexes(genome,substring);

        assertThat(substringIndexes).containsExactly(2, 4, 10);

    }

    @Test
    public void shouldFindAllSubstrings_testCase() {
        String genome="TTTATGTCCCGTCCTAAGTTCCCGTCCGCCCTAGCCCGTCCCGCACCGGTATACCCGTCCCCCCGTCCATTAAAAAGCCCGTCCAGCCCGTCCCCCGTCCCCCGTCCGTCCCCGTCCGCCCGTCCGGCCCGTCCCCCCGTCCCCCGTCCCCCGTCCTGCCCGTCCAGCGCACGGCCCGTCCATACCAGACCCCGTCCCCCGTCCTCCCCCGTCCAGATAGACCCGTCCCCCGTCCCCCGTCCGCCCGTCCAGACCCGTCCTCCCGTCCGCGAAGCCCGTCCTCCCGTCCCCCCGTCCGTGTCCCGTCCACCCGTCCAGTCCCGTCCCCCCGTCCTAACCCGTCCACCCGTCCAACCTGCCCGTCCCCCGTCCCCCGTCCCGGGCCCCGTCCCCCCGTCCGCCCGTCCGCCCGTCCGCCCGTCCACCCGTCCAATCCCGTCCACCCCGTCCCCCGTCCCCCGTCCTCCCGTCCCAGCCCCGTCCGTACCGTCCCGTCCCTCCCGTCCTATCCCCCGTCCGCCCGTCCCCCGTCCTCCCTAGCCCGTCCGCTCCCGTCCTCCCGTCCCCCGTCCCCCGTCCGTCCCGTCCCCAAACATCCCCGTCCCCCGTCCCCTGCTGCCCACCCGTCCCCCGTCCGGGAAACCCCCGTCCCCCGTCCATCGCCGGCAATCAGCCCGTCCTAGCGCAACCCGTCCCGCCCGTCCGTCTGCATCGGAAAGGCCCGTCCCACCCCGTCCGAGCCCGTCCGCCCGTCCTCCCGTCCACGTCCCGTCCGACCCGTCCGTGTAAACCCGTCCCTCCCGTCCGATATCCCCGTCCCCCGTCCATTGCCCCCGTCCGCCCGTCC";
        String substring = "CCCGTCCCC";

        List<Integer> substringIndexes = getSubstringIndexes(genome,substring);

        assertThat(substringIndexes).containsExactly(54, 87, 94, 128, 136, 143, 191, 222, 229, 283, 320, 359, 366, 385, 444, 451, 520, 559, 566, 582, 598, 605, 623, 645, 813);
        
        assertThat(StringUtils.join(substringIndexes," ")).isEqualTo("54 87 94 128 136 143 191 222 229 283 320 359 366 385 444 451 520 559 566 582 598 605 623 645 813");
    }

    private List<Integer> getSubstringIndexes(String genome, String substring) {

        int indexOf = StringUtils.indexOf(genome, substring);
        List<Integer> substringIndexes = new LinkedList<>();
        while(indexOf!=-1) {
            substringIndexes.add(++indexOf);
            indexOf = StringUtils.indexOf(genome,substring,indexOf);
        }
        return substringIndexes;

    }

}


