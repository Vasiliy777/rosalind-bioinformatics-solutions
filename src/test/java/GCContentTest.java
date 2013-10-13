import com.sun.xml.internal.ws.util.StringUtils;
import org.assertj.core.data.Offset;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import static org.apache.commons.lang3.StringUtils.*;
import static org.assertj.core.api.Assertions.assertThat;

public class GCContentTest {

    @Test
    public void shouldCalculateGCContent() {
        String genome = "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT";

        double gcContentPercentage = calculateGCPercentage(genome);
        assertThat(gcContentPercentage).isEqualTo(60.919540, Offset.offset(0.001d));
    }

    private double calculateGCPercentage(String genome) {
        double gcCount=0;
        for (char c : genome.toCharArray()) {
            if(isGC(c)) {
                gcCount ++;
            }
        }
        return 100.0 * gcCount/genome.length();
    }

    @Test
    public void shouldFindHighestGCCOntent() throws Exception {
        String inputGenomes=">Rosalind_6404\n" +
                "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC\n" +
                "TCCCACTAATAATTCTGAGG\n" +
                ">Rosalind_5959\n" +
                "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT\n" +
                "ATATCCATTTGTCAGCAGACACGC\n" +
                ">Rosalind_0808\n" +
                "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC\n" +
                "TGGGAACCTGCGGGCAGTAGGTGGAAT";

        String[] genomes = inputGenomes.split(">");
        double max = 0d;
        String maxName="";
        for (String genomeInfo : genomes) {
            String genome = replace(substringAfter(genomeInfo, "\n"), "\n", "");
            if(calculateGCPercentage(genome)>max) {
                max = calculateGCPercentage(genome);
                maxName = substringBefore(genomeInfo, "\n");
            }
        }

        String response = maxName + "\n" + max;
        assertThat(response).startsWith("Rosalind_0808\n");
        assertThat(Double.valueOf(substringAfter(response,"\n"))).isEqualTo(60.919540,Offset.offset(0.001));

    }

    private boolean isGC(char c) {
        return c =='G' || c== 'C';
    }

}
