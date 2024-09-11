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

// Write your say function here
export function say(word) {
  let result = ""

  function next(input) {
    if (input === undefined) {
      return result
    }
    if (input !== "") {
      if (result.length > 0) {
        result += " "
      }
      result += input
    }
    return next
  }

  return next(word)
}

// export function say(word) {
//   let words = ""

//   function next(input) {
//     if (input === undefined) {
//       return words.trim()
//     }
//     words = words ? `${words} ${input}` : input
//     return next
//   }

//   return next(word)
// }

// Write your line count function here

// Write your Quaternion class here
