import exp from "node:constants"

import { readSync } from "node:fs"
import { open } from "node:fs/promises"

export function change(amount) {
  if (!Number.isInteger(amount)) {
    throw new TypeError("Amount must be an integer")
  }
  if (amount < 0) {
    throw new RangeError("Amount cannot be negative")
  }
  let [counts, remaining] = [{}, amount]
  for (const denomination of [25, 10, 5, 1]) {
    counts[denomination] = Math.floor(remaining / denomination)
    remaining %= denomination
  }
  return counts
}

export function firstThenLowerCase(sequence, predicate) {
  for (let element of sequence ?? []) {
    if (predicate(element) && typeof element === "string") {
      return element.toLowerCase()
    }
  }
  return undefined
}

export function* powersGenerator({ ofBase, upTo }) {
  let power = 1
  while (power <= upTo) {
    yield power
    power *= ofBase
  }
}

export function say(word) {
  let result = ""
  let isFirstWord = true

  function next(input) {
    if (input === undefined) {
      return result
    }
    if (input === " ") {
      if (!isFirstWord && result.length > 0) {
        result += " "
      }
      isFirstWord = false
    } else {
      if (!isFirstWord) {
        result += " "
      }
      result += input
      isFirstWord = false
    }
    return next
  }

  return next(word)
}

export async function meaningfulLineCount(filePath) {
  try {
    const fileHandle = await open(filePath, "r")
    const reader = fileHandle.createReadStream()
    let lineCount = 0
    let buffer = ""

    for await (const chunk of reader) {
      buffer += chunk.toString()
      let lines = buffer.split("\n")
      buffer = lines.pop()

      for (const line of lines) {
        const strippedLine = line.trim()
        if (
          strippedLine.length > 0 &&
          strippedLine.length > 1 &&
          !strippedLine.startsWith("#")
        ) {
          lineCount++
        }
      }
    }

    if (
      buffer.trim().length > 0 &&
      buffer.length > 1 &&
      !buffer.trim().startsWith("#")
    ) {
      lineCount++
    }

    await fileHandle.close()
    return lineCount
  } catch (error) {
    throw new Error(`Error reading file: ${error.message}`)
  }
}

// Write your Quaternion class here
export class Quaternion {
  constructor(a, b, c, d) {
    this.a = a
    this.b = b
    this.c = c
    this.d = d
    Object.freeze(this) // Make the instance immutable
  }

  plus(other) {
    return new Quaternion(
      this.a + other.a,
      this.b + other.b,
      this.c + other.c,
      this.d + other.d
    )
  }

  times(other) {
    const a1 = this.a,
      b1 = this.b,
      c1 = this.c,
      d1 = this.d
    const a2 = other.a,
      b2 = other.b,
      c2 = other.c,
      d2 = other.d

    return new Quaternion(
      a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2,
      a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2,
      a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2,
      a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
    )
  }

  equals(other) {
    return (
      this.a === other.a &&
      this.b === other.b &&
      this.c === other.c &&
      this.d === other.d
    )
  }

  toString() {
    const terms = []

    if (this.a !== 0) terms.push(`${this.a}`)
    if (this.b !== 0) {
      const bTerm = this.b === 1 ? "i" : this.b === -1 ? "-i" : `${this.b}i`
      terms.push(bTerm)
    }
    if (this.c !== 0) {
      const cTerm = this.c === 1 ? "j" : this.c === -1 ? "-j" : `${this.c}j`
      terms.push(cTerm)
    }
    if (this.d !== 0) {
      const dTerm = this.d === 1 ? "k" : this.d === -1 ? "-k" : `${this.d}k`
      terms.push(dTerm)
    }

    if (terms.length === 0) return "0"

    // Join terms with appropriate '+' and '-' signs
    return terms
      .map((term, index) => {
        if (index === 0) return term // The first term is not prefixed with '+'
        return term.startsWith("-") ? term : `+${term}`
      })
      .join("")
  }

  toJSON() {
    return `Quaternion(${this.a}, ${this.b}, ${this.c}, ${this.d})`
  }

  get coefficients() {
    return [this.a, this.b, this.c, this.d]
  }

  get conjugate() {
    return new Quaternion(this.a, -this.b, -this.c, -this.d)
  }

  get norm() {
    return Math.sqrt(this.a ** 2 + this.b ** 2 + this.c ** 2 + this.d ** 2)
  }
}
