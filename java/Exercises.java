import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;
import java.util.function.Predicate;
import javax.swing.plaf.synth.SynthLookAndFeel;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.security.PublicKey;

public class Exercises {
    static Map<Integer, Long> change(long amount) {
        if (amount < 0) {
            throw new IllegalArgumentException("Amount cannot be negative");
        }
        var counts = new HashMap<Integer, Long>();
        for (var denomination : List.of(25, 10, 5, 1)) {
            counts.put(denomination, amount / denomination);
            amount %= denomination;
        }
        return counts;
    }

    public static Optional<String> firstThenLowerCase(List<String> strings, Predicate<String> predicate) {
        return strings.stream()                     
                .filter(predicate)                   
                .map(String::toLowerCase)         
                .findFirst();                    
    }

    // Write your say function here
    public static SayChainable say() {
        return new SayChainable() {
            @Override
            public SayChainable and (String word) {
                return say(word);
            }

            @Override
            public String phrase() {
                return "";
            }
        };
    }
    public static SayChainable say(String initialWord) {
        return new SayChainable() {
            private final String currentPhrase = initialWord;

            @Override
            public SayChainable and (String word) {
                return say(currentPhrase + " " + word);
            }

            @Override
            public String phrase() {
                return currentPhrase;
            }
        };
    }
    interface SayChainable {
        SayChainable and(String word);
        String phrase();
    }


    // Write your line count function here
    public static long meaningfulLineCount(String filename) throws IOException {
        long count = 0;
        
        try(BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String trimmed = line.trim();
                if (!trimmed.isEmpty() && !trimmed.startsWith("#")) {
                    count++;
                }
            }
        }
        return count;
    }
    
}
// Write your Quaternion record class here


// Write your BinarySearchTree sealed interface and its implementations here
