import Foundation

struct NegativeAmountError: Error {}
struct NoSuchFileError: Error {}

func change(_ amount: Int) -> Result<[Int: Int], NegativeAmountError> {
    if amount < 0 {
        return .failure(NegativeAmountError())
    }
    var counts: [Int: Int] = [:]
    var remaining = amount
    for denomination in [25, 10, 5, 1] {
        let quotientAndRemainder = remaining.quotientAndRemainder(dividingBy: denomination)
        counts[denomination] = quotientAndRemainder.quotient
        remaining = quotientAndRemainder.remainder
    }
    return .success(counts)
}

func firstThenLowerCase(of array: [String], satisfying predicate: (String) -> Bool) -> String? {
    return array.first(where: predicate)?.lowercased()
}

// Define the Say class
class Say {
    private var words: [String] = []

    func and(_ word: String) -> Say {
        words.append(word)
        return self
    }

    var phrase: String {
        return words.joined(separator: " ")
    }
}

// Provide a function that returns an instance of Say
func say(_ word: String = "") -> Say {
    let instance = Say()
    if !word.isEmpty {
        return instance.and(word)
    }
    return instance
}



// Write your say function here

// Write your meaningfulLineCount function here

// Write your Quaternion struct here

// Write your Binary Search Tree enum here
