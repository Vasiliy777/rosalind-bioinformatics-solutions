import com.google.common.base.Splitter;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.primitives.Chars;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

public class FrequentKMersTest {

    @Test
    public void shouldFindFrequentKMers() {
        String genome="ACGTTGCATGTCGCATGATGCATGAGAGCT";
        String length = "4";

        List<String> mostFrequentMers = getMostFrequentKMers(genome, length);

        assertThat(mostFrequentMers).containsExactly("CATG","GCAT");

    }

    private List<String> getMostFrequentKMers(String genome, String length) {
        Iterable<String> splitted = Splitter.fixedLength(4).split(genome);
        Map<String, List<? super String>> collected = (Map<String, List<? super String>>)
                Lists.newArrayList(splitted).stream().collect(Collectors.groupingBy(Function.<String>identity()));
        System.out.println(collected);
        Chars.asList(genome.toCharArray()).forEach(c -> System.out.print(c));
        return null;
    }

    @Test
    public void shouldFindAllSubstrings_testCase() {
        Map<String, List<? super String>> a = (Map<String, List<? super String>>) Arrays.asList("a").stream().collect(Collectors.groupingBy(Function.<String>identity()));

        String genome="ACGTTGCATGTCGCATGATGCATGAGAGCT";
        String length = "4";

        List<String> mostFrequentMers = getMostFrequentKMers(genome, length);

        assertThat(mostFrequentMers).containsExactly("CATG","GCAT");

    }


}


