import java.io.BufferedReader
import java.io.FileReader
import java.io.IOException

fun change(amount: Long): Map<Int, Long> {
    require(amount >= 0) { "Amount cannot be negative" }
    val counts = mutableMapOf<Int, Long>()
    var remaining = amount
    for (denomination in listOf(25, 10, 5, 1)) {
        counts[denomination] = remaining / denomination
        remaining %= denomination
    }
    return counts
}

// Write your first then lower case function here

fun firstThenLowerCase(strings: List<String>, predicate: (String) -> Boolean): String? {
    return strings.firstOrNull(predicate)?.lowercase()
}

// Write your say function here

// Write your meaningfulLineCount function here

fun meaningfulLineCount(fileName: String): Long {
    BufferedReader(FileReader(fileName)).use { reader ->
        return reader.lineSequence()
        .filter { line -> line.isNotBlank() && !line.trimStart().startsWith("#")}
        .count().toLong()
    }
}


// Write your Quaternion data class here

// Write your Binary Search Tree interface and implementing classes here
