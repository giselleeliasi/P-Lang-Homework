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

<<<<<<< HEAD
=======

>>>>>>> c7b19e3791ba7d13f44d5690a6f93b4313e5a56f
func say(_ word: String) -> (String) -> (() -> String) {
    var currentPhrase = word
    return { nextWord in
        currentPhrase += " \(nextWord)"
        return { currentPhrase }
    }
}

func say(_ word: String? = nil) -> SayChainable {
    return SayChainable(phrase: word ?? "")
}

struct SayChainable {
    private(set) var phrase: String
    func and(_ word: String) -> SayChainable {
        return SayChainable(phrase: "\(phrase) \(word)")
    }
}

<<<<<<< HEAD
=======



>>>>>>> c7b19e3791ba7d13f44d5690a6f93b4313e5a56f
func meaningfulLineCount(_ filePath: String) async -> Result<Int, NoSuchFileError> {
    let fileURL = URL(fileURLWithPath: filePath)

    do {
        let data = try await Task { () -> Data in
            return try Data(contentsOf: fileURL)
        }.value
        
        guard let content = String(data: data, encoding: .utf8) else {
            return .failure(NoSuchFileError())
        }
        
        let lines = content.components(separatedBy: .newlines)
        let meaningfulLinesCount = lines.filter {
            let trimmedLine = $0.trimmingCharacters(in: .whitespaces)
            return !trimmedLine.isEmpty && !trimmedLine.hasPrefix("#")
        }.count
        
        return .success(meaningfulLinesCount)
    } catch {
        return .failure(NoSuchFileError())
    }
}

<<<<<<< HEAD
=======


>>>>>>> c7b19e3791ba7d13f44d5690a6f93b4313e5a56f
struct Quaternion: CustomStringConvertible, Equatable {
    let a: Double  
    let b: Double 
    let c: Double  
    let d: Double 

    init(a: Double, b: Double, c: Double, d: Double) {
        self.a = a
        self.b = b
        self.c = c
        self.d = d
    }

    init(b: Double, c: Double) {
        self.a = 0.0
        self.b = b
        self.c = c
        self.d = 0.0
    }

    init(b: Double) {
        self.a = 0.0
        self.b = b
        self.c = 0.0
        self.d = 0.0
    }
    
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
        if result.count > 0 {
            result += b > 0 ? "+" : ""
        }
        result += b == 1 ? "i" : (b == -1 ? "-i" : "\(b)i")
    }

    if c != 0 {
        if result.count > 0 {
            result += c > 0 ? "+" : ""
        }
        result += c == 1 ? "j" : (c == -1 ? "-j" : "\(c)j")
    }

    if d != 0 {
        if result.count > 0 {
            result += d > 0 ? "+" : ""
        }
        result += d == 1 ? "k" : (d == -1 ? "-k" : "\(d)k")
    }

    return result.isEmpty ? "0" : result
    }

}

extension Quaternion {
    static func ==(lhs: Quaternion, rhs: Quaternion) -> Bool {
        return lhs.a == rhs.a && lhs.b == rhs.b && lhs.c == rhs.c && lhs.d == rhs.d
    }
}

<<<<<<< HEAD
=======


>>>>>>> c7b19e3791ba7d13f44d5690a6f93b4313e5a56f
indirect enum BinarySearchTree: CustomStringConvertible {
    case empty
    case node(value: String, left: BinarySearchTree, right: BinarySearchTree)

    func insert(_ value: String) -> BinarySearchTree {
        switch self {
        case .empty:
            return .node(value: value, left: .empty, right: .empty)
        case .node(let currentValue, let left, let right):
            if value < currentValue {
                return .node(value: currentValue, left: left.insert(value), right: right)
            } else if value > currentValue {
                return .node(value: currentValue, left: left, right: right.insert(value))
            } else {
                return self 
            }
        }
    }

    func contains(_ value: String) -> Bool {
        switch self {
        case .empty:
            return false
        case .node(let currentValue, let left, let right):
            if value == currentValue {
                return true
            } else if value < currentValue {
                return left.contains(value)
            } else {
                return right.contains(value)
            }
        }
    }


    var size: Int {
        switch self {
        case .empty:
            return 0
        case .node(_, let left, let right):
            return 1 + left.size + right.size
        }
    }


    var description: String {
        switch self {
        case .empty:
            return "()"
        case .node(let value, let left, let right):
            let leftString = left.description == "()" ? "" : left.description
            let rightString = right.description == "()" ? "" : right.description
            return "(\(leftString)\(value)\(rightString))"
        }
    }
}