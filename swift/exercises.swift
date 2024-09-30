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


// Write your say function here
// func say(_ first: String? = nil) -> (String?) -> String {
//     return { second in
//         if let first = first {
//             if let second = second {
//                 // Call `say` with the concatenated string and return the inner function
//                 return say("\(first) \(second)")()
//             } else {
//                 return first // Return the first word if no second word is provided
//             }
//         } else {
//             return "" // Return an empty string if no words are provided
//         }
//     }
// }


// Write your meaningfulLineCount function here
func meaningfulLineCount(_ filename: String) async -> Result<Int, NoSuchFileError> {
    let fileURL = URL(fileURLWithPath: filename)

    do {
        let content = try String(contentsOf: fileURL)
        let lines = content.split(whereSeparator: \.isNewline)
        let meaningfulLines = lines.filter { line in
            let trimmedLine = line.trimmingCharacters(in: .whitespacesAndNewlines)
            return !trimmedLine.isEmpty && !trimmedLine.hasPrefix("#")
        }
        return .success(meaningfulLines.count)
    } catch {
        return .failure(NoSuchFileError())
    }
}


// Write your Quaternion struct here
import Foundation

struct Quaternion {
    var a: Double
    var b: Double
    var c: Double
    var d: Double

    static func + (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
        return Quaternion(a: lhs.a + rhs.a, b: lhs.b + rhs.b, c: lhs.c + rhs.c, d: lhs.d + rhs.d)
    }

    static func * (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
        return Quaternion(
            a: lhs.a * rhs.a - lhs.b * rhs.b - lhs.c * rhs.c - lhs.d * rhs.d,
            b: lhs.a * rhs.b + lhs.b * rhs.a + lhs.c * rhs.d - lhs.d * rhs.c,
            c: lhs.a * rhs.c - lhs.b * rhs.d + lhs.c * rhs.a + lhs.d * rhs.b,
            d: lhs.a * rhs.d + lhs.b * rhs.c - lhs.c * rhs.b + lhs.d * rhs.a
        )
    }

    var coefficients: (Double, Double, Double, Double) {
        return (a, b, c, d)
    }

    var conjugate: Quaternion {
        return Quaternion(a: a, b: -b, c: -c, d: -d)
    }

    var description: String {
        var s = ""
        let coeffs = coefficients
        let units = ["", "i", "j", "k"]
        
        for index in 0..<4 {
            let coeff = coeffs[index]
            if coeff == 0 { continue }
            let unit = units[index]
            s += coeff < 0 ? "-" : (s.isEmpty ? "" : "+")
            s += abs(coeff) == 1 && !unit.isEmpty ? "" : "\(abs(coeff))"
            s += unit
        }
        return s.isEmpty ? "0" : s
    }
}

func main() {
    let q1 = Quaternion(a: 1, b: 3, c: 5, d: 2)
    let q2 = Quaternion(a: -2, b: 2, c: 8, d: -1)

    let qSum = q1 + q2
    let qProduct = q1 * q2

    print("Sum: \(qSum.description)")
    print("Product: \(qProduct.description)")
    print("Conjugate of q1: \(q1.conjugate.description)")
}

main()


// Write your Binary Search Tree enum here
