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

// Write your line count function here
export async function meaningfulLineCount(filePath) {
  try {
    const fileHandle = await open(filePath, "r")
    const reader = fileHandle.createReadStream()
    let lineCount = 0

    for await (const chunk of reader) {
      const lines = chunk.toString().split("\n")
      for (const line of lines) {
        if (line.trim().length > 0) {
          lineCount++
        }
      }
    }
    await fileHandle.close()
    return lineCount
  } catch (error) {
    throw new Error(`Error reading file: ${error.message}`)
  }
}

// Write your Quaternion class here
