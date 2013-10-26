import com.google.common.base.Splitter;
import com.google.common.collect.*;
import com.google.common.primitives.Chars;
import org.apache.commons.lang3.StringUtils;
import org.junit.Ignore;
import org.junit.Test;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

public class FrequentKMersTest {

    @Test
    @Ignore
    public void shouldFindFrequentKMers() {
        String genome="ACGTTGCATGTCGCATGATGCATGAGAGCT";
        String length = "4";

        List<String> mostFrequentMers = getMostFrequentKMers(genome, length);

        assertThat(mostFrequentMers).containsExactly("CATG","GCAT");

    }

    private List<String> getMostFrequentKMers(String genome, String lengthString) {
//        Integer length = Integer.valueOf(lengthString);
//        Stream<String> map = ContiguousSet.create(Range.closed(0, genome.length() - length), DiscreteDomain.integers()).stream()
//                .map(index -> genome.substring(index, index + length));
//        Map<String, Long> collected = map.collect(Collectors.collectingAndThen(Collectors.groupingBy(Function.<String>identity()),);
//        System.out.println(collected);
        return null;


    }







}


