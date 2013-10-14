import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

import static org.apache.commons.lang3.StringUtils.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.offset;

public class DominantAlleleTest {

    @Test
    public void shouldCalculateProbabilityOfIndividualWithDominantAllele() {
        int dominant = 2;
        int heterozygous = 2;
        int recessive = 2;

        double hammingDistance = probabilityOfMatedIndividualWillHaveDominantAllele(dominant, heterozygous, recessive);

        assertThat(hammingDistance).isEqualTo(0.78333,offset(0.0001));
    }

    @Test
    public void shouldCalculateProbabilityOfIndividualWithDominantAllele_TestSet() {
        String dominantHeteroRecessive = "24 16 22";
        int dominant = Integer.parseInt(substringBefore(dominantHeteroRecessive, " "));
        int heterozygous = Integer.parseInt(substringBefore(substringAfter(dominantHeteroRecessive, " "), " "));
        int recessive = Integer.parseInt(substringAfterLast(dominantHeteroRecessive, " "));

        double hammingDistance = probabilityOfMatedIndividualWillHaveDominantAllele(dominant, heterozygous, recessive);

        assertThat(hammingDistance).isEqualTo(0.768905,offset(0.0001));
    }

    private double probabilityOfMatedIndividualWillHaveDominantAllele(double dominant, double heterozygous, double recessive) {
        double all = dominant + recessive + heterozygous;
        double secondAll = all - 1.0;
        return 2.0 * dominant/all - dominant/all*(dominant-1.0)/secondAll +
                (heterozygous/all) * (recessive/ secondAll) * 0.5 +
                (recessive/all) * (heterozygous/ secondAll) * 0.5
                + (heterozygous/all) * ((heterozygous-1.0)/ secondAll) * 0.75;
    }


}
