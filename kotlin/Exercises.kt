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

fun say(initialWord: String = "" ): SayChainable {
    val words = mutableListOf(initialWord)

    return object : SayChainable {
        override fun and(word: String): SayChainable {
            words.add(word)
            return this
        }
        override val phrase: String
            get() = words.joinToString(" ") 
    }
}

interface SayChainable{
    fun and(word: String): SayChainable
    val phrase: String
}


// Write your meaningfulLineCount function here

fun meaningfulLineCount(fileName: String): Long {
    BufferedReader(FileReader(fileName)).use { reader ->
        return reader.lineSequence()
        .filter { line -> line.isNotBlank() && !line.trimStart().startsWith("#")}
        .count().toLong()
    }
}


// Write your Quaternion data class here

data class Quaternion(val a: Double, val b: Double, val c: Double, val d: Double) {

    companion object {
        val ZERO = Quaternion(0.0, 0.0, 0.0, 0.0)
        val I = Quaternion(0.0, 1.0, 0.0, 0.0)
        val J = Quaternion(0.0, 0.0, 1.0, 0.0)
        Quaternion(0.0, 0.0, 0.0, 1.0)
    }

    fun coefficients(): List<Double> {
        return listOf(a, b, c, d)
    }

    fun conjugate(): Quaternion {
        return Quaternion(a, -b, -c, -d)
    }
}

// Write your Binary Search Tree interface and implementing classes here
