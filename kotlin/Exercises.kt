import java.io.BufferedReader
import java.io.FileReader

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

<<<<<<< HEAD
=======

>>>>>>> c7b19e3791ba7d13f44d5690a6f93b4313e5a56f

fun firstThenLowerCase(strings: List<String>, predicate: (String) -> Boolean): String? {
    return strings.firstOrNull(predicate)?.lowercase()
}

<<<<<<< HEAD
=======

>>>>>>> c7b19e3791ba7d13f44d5690a6f93b4313e5a56f

fun say(initialWord: String? = null): SayChainable {
    if (initialWord == null) {
        return object : SayChainable {
            override fun and(word: String): SayChainable {
                return say(word)
            }
            override val phrase: String
                get() = ""
        }
    }
    return object : SayChainable {
        private val currentPhrase: String = initialWord
        override fun and(word: String): SayChainable {
            return say("$currentPhrase $word")
        }
        override val phrase: String
            get() = currentPhrase
    }
}

interface SayChainable {
    fun and(word: String): SayChainable
    val phrase: String
}

<<<<<<< HEAD
=======


>>>>>>> c7b19e3791ba7d13f44d5690a6f93b4313e5a56f
fun meaningfulLineCount(fileName: String): Long {
    BufferedReader(FileReader(fileName)).use { reader ->
        return reader.lineSequence()
        .filter { line -> line.isNotBlank() && !line.trimStart().startsWith("#")}
        .count().toLong()
    }
}


<<<<<<< HEAD
=======


>>>>>>> c7b19e3791ba7d13f44d5690a6f93b4313e5a56f
data class Quaternion(val a: Double, val b: Double, val c: Double, val d: Double) {

    companion object {
        val ZERO = Quaternion(0.0, 0.0, 0.0, 0.0)
        val I = Quaternion(0.0, 1.0, 0.0, 0.0)
        val J = Quaternion(0.0, 0.0, 1.0, 0.0)
        val K = Quaternion(0.0, 0.0, 0.0, 1.0)
    }

    fun coefficients(): List<Double> {
        return listOf(a, b, c, d)
    }

    fun conjugate(): Quaternion {
        return Quaternion(a, -b, -c, -d)
    }

    operator fun plus(other: Quaternion): Quaternion {
        return Quaternion(
            a + other.a,
            b + other.b,
            c + other.c,
            d + other.d
        )
    }

    operator fun times(other: Quaternion): Quaternion {
        return Quaternion(
            a * other.a - b * other.b - c * other.c - d * other.d,
            a * other.b + b * other.a + c * other.d - d * other.c,
            a * other.c - b * other.d + c * other.a + d * other.b,
            a * other.d + b * other.c - c * other.b + d * other.a
        )
    }

    override fun toString(): String {
        return buildString {
            if (a != 0.0) append(a)
            
            if(b != 0.0) {
                if (b > 0 && isNotEmpty()) append("+")
                if (b == -1.0) append("-")
                else if (b != 1.0) append(b)
                append("i")
            }

            if(c != 0.0) {
                if (c > 0 && isNotEmpty()) append("+")
                if (c == -1.0) append("-")
                else if (c != 1.0) append(c)
                append("j")
            }

            if(d != 0.0) {
                if (d > 0 && isNotEmpty()) append("+")
                if (d == -1.0) append("-")
                else if (d != 1.0) append(d)
                append("k")
            }
            
            if (isEmpty()) append("0")
        }.toString()
    }
}

<<<<<<< HEAD
=======


>>>>>>> c7b19e3791ba7d13f44d5690a6f93b4313e5a56f
sealed interface BinarySearchTree {
    fun insert(value: String): BinarySearchTree
    fun contains(value: String): Boolean
    fun size(): Int
    override fun toString(): String

    object Empty : BinarySearchTree {
        override fun insert(value: String): BinarySearchTree {
            return Node(value, Empty, Empty)
        }

        override fun contains(value: String): Boolean = false
        override fun size(): Int = 0
        override fun toString(): String = "()"
    }

    data class Node(val value: String, val left: BinarySearchTree, val right: BinarySearchTree) : BinarySearchTree {
        override fun insert(value: String): BinarySearchTree {
            return when {
                value < this.value -> Node(this.value, left.insert(value), right)
                value > this.value -> Node(this.value, left, right.insert(value))
                else -> this
            }
        }
        
        override fun contains(value: String): Boolean {
            return when {
                value < this.value -> left.contains(value)
                value > this.value -> right.contains(value)
                else -> true
            }
        }

        override fun size(): Int {
            return 1 + left.size() + right.size()
        }

        override fun toString(): String {
            val leftString = if (left is Empty) "" else left.toString()
            val rightString = if (right is Empty) "" else right.toString()
            return "($leftString$value$rightString)"
        }
    }
}