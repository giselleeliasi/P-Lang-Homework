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
// // Write your say function here

// // Define the Say class
// class Say {
//     private var words: [String] = []

//     func and(_ word: String) -> Say {
//         words.append(word)
//         return self
//     }

//     var phrase: String {
//         return words.joined(separator: " ")
//     }
// }

// // Provide a function that returns an instance of Say
// func say(_ word: String = "") -> Say {
//     let instance = Say()
//     if !word.isEmpty {
//         return instance.and(word)
//     }
//     return instance
// }




// Write your meaningfulLineCount function here
func meaningfulLineCount(_ filePath: String) -> Result<Int, NoSuchFileError> {
    let fileURL = URL(fileURLWithPath: filePath)

    do {
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        let lines = content.components(separatedBy: .newlines)
        
        // Count meaningful lines (non-empty and not just whitespace)
        let meaningfulLinesCount = lines.filter { 
            let trimmedLine = $0.trimmingCharacters(in: .whitespaces)
            return !trimmedLine.isEmpty && !trimmedLine.hasPrefix("#")
        }.count
        
        return .success(meaningfulLinesCount)
    } catch {
        // Return a failure if the file does not exist or cannot be read
        return .failure(NoSuchFileError())
    }
}
// Write your Quaternion struct here

struct Quaternion {
    var a: Double  // Real part
    var b: Double  // i component
    var c: Double  // j component
    var d: Double  // k component

    // Original initializer with four parameters
    init(a: Double, b: Double, c: Double, d: Double) {
        self.a = a
        self.b = b
        self.c = c
        self.d = d
    }

    // Initializer for b and c
    init(b: Double, c: Double) {
        self.a = 0.0
        self.b = b
        self.c = c
        self.d = 0.0
    }

    // Initializer for b only
    init(b: Double) {
        self.a = 0.0
        self.b = b
        self.c = 0.0
        self.d = 0.0
    }
    
    // New initializer to allow for single a value
    init(a: Double) {
        self.a = a
        self.b = 0.0
        self.c = 0.0
        self.d = 0.0
    }

    var coefficients: [Double] {
        return [a, b, c, d]
    }

    static var ZERO: Quaternion {
        return Quaternion(a: 0, b: 0, c: 0, d: 0)
    }

    static var I: Quaternion {
        return Quaternion(a: 0, b: 1, c: 0, d: 0)
    }

    static var J: Quaternion {
        return Quaternion(a: 0, b: 0, c: 1, d: 0)
    }

    static var K: Quaternion {
        return Quaternion(a: 0, b: 0, c: 0, d: 1)
    }

    static func + (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
        return Quaternion(
            a: lhs.a + rhs.a,
            b: lhs.b + rhs.b,
            c: lhs.c + rhs.c,
            d: lhs.d + rhs.d
        )
    }

    static func * (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
        return Quaternion(
            a: lhs.a * rhs.a - lhs.b * rhs.b - lhs.c * rhs.c - lhs.d * rhs.d,
            b: lhs.a * rhs.b + lhs.b * rhs.a + lhs.c * rhs.d - lhs.d * rhs.c,
            c: lhs.a * rhs.c - lhs.b * rhs.d + lhs.c * rhs.a + lhs.d * rhs.b,
            d: lhs.a * rhs.d + lhs.b * rhs.c - lhs.c * rhs.b + lhs.d * rhs.a
        )
    }

    var conjugate: Quaternion {
        return Quaternion(a: a, b: -b, c: -c, d: -d)
    }

    var description: String {
        var result = ""
        if a != 0 {
            result += "\(a)"
        }
        if b != 0 {
            result += (b > 0 ? "+" : "") + "\(b)i"
        }
        if c != 0 {
            result += (c > 0 ? "+" : "") + "\(c)j"
        }
        if d != 0 {
            result += (d > 0 ? "+" : "") + "\(d)k"
        }
        return result.isEmpty ? "0" : result
    }
}

extension Quaternion: Equatable {
    static func ==(lhs: Quaternion, rhs: Quaternion) -> Bool {
        return lhs.a == rhs.a && lhs.b == rhs.b && lhs.c == rhs.c && lhs.d == rhs.d
    }
}



// Write your Binary Search Tree enum here