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

record Quaternion(double a, double b, double c, double d) {
    public static final Quaternion ZERO = new Quaternion(0.0, 0.0, 0.0, 0.0);
    public static final Quaternion I = new Quaternion(0.0, 1.0, 0.0, 0.0);
    public static final Quaternion J = new Quaternion(0.0, 0.0, 1.0, 0.0);
    public static final Quaternion K = new Quaternion(0.0, 0.0, 0.0, 1.0);

 
    public Quaternion {
        if (Double.isNaN(a) || Double.isNaN(b) || Double.isNaN(c) || Double.isNaN(d)) {
            throw new IllegalArgumentException("Coefficients cannot be NaN");
        }
    }

    public List<Double> coefficients() {
        return List.of(a, b, c, d);
    }

    public Quaternion conjugate() {
        return new Quaternion(a, -b, -c, -d);
    }

    public Quaternion plus(Quaternion other) {
        return new Quaternion(
                this.a + other.a,
                this.b + other.b,
                this.c + other.c,
                this.d + other.d
        );
    }

    public Quaternion times(Quaternion other) {
        return new Quaternion(
                this.a * other.a - this.b * other.b - this.c * other.c - this.d * other.d,
                this.a * other.b + this.b * other.a + this.c * other.d - this.d * other.c,
                this.a * other.c - this.b * other.d + this.c * other.a + this.d * other.b,
                this.a * other.d + this.b * other.c - this.c * other.b + this.d * other.a
        );
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (a != 0.0) sb.append(a);

        if (b != 0.0) {
            if (b > 0 && sb.length() > 0) sb.append("+");
            if (b == -1.0) sb.append("-");
            else if (b != 1.0) sb.append(b);
            sb.append("i");
        }

        if (c != 0.0) {
            if (c > 0 && sb.length() > 0) sb.append("+");
            if (c == -1.0) sb.append("-");
            else if (c != 1.0) sb.append(c);
            sb.append("j");
        }

        if (d != 0.0) {
            if (d > 0 && sb.length() > 0) sb.append("+");
            if (d == -1.0) sb.append("-");
            else if (d != 1.0) sb.append(d);
            sb.append("k");
        }

        if (sb.length() == 0) sb.append("0");
        return sb.toString();
    }
}

sealed interface BinarySearchTree permits Empty, Node {
    BinarySearchTree insert(String value);
    boolean contains(String value);
    int size();
    @Override
    String toString();
}

final class Empty implements BinarySearchTree {
    Empty() {}

    @Override
    public BinarySearchTree insert(String value) {
        return new Node(value, this, this);
    }

    @Override
    public boolean contains(String value) {
        return false;
    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public String toString() {
        return "()";
    }
}


final class Node implements BinarySearchTree {
    private final String value;
    private final BinarySearchTree left;
    private final BinarySearchTree right;

    public Node(String value, BinarySearchTree left, BinarySearchTree right) {
        this.value = value;
        this.left = left;
        this.right = right;
    }

    @Override
    public BinarySearchTree insert(String value) {
        if (value.compareTo(this.value) < 0) {
            return new Node(this.value, left.insert(value), right);
        } else if (value.compareTo(this.value) > 0) {
            return new Node(this.value, left, right.insert(value));
        } else {
            return this; 
        }
    }

    @Override
    public boolean contains(String value) {
        if (value.compareTo(this.value) < 0) {
            return left.contains(value);
        } else if (value.compareTo(this.value) > 0) {
            return right.contains(value);
        } else {
            return true; 
        }
    }

    @Override
    public int size() {
        return 1 + left.size() + right.size();
    }

    @Override
    public String toString() {
        String leftString = left instanceof Empty ? "" : left.toString();
        String rightString = right instanceof Empty ? "" : right.toString();
        return "(" + leftString + value + rightString + ")";
    }
}


