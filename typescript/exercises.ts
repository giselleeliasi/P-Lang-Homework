import exp from "node:constants"
import { open } from "node:fs/promises"
type Predicate<T> = (element: T) => boolean
type Mapper<T, U> = (element: T) => U

export function change(amount: bigint): Map<bigint, bigint> {
  if (amount < 0) {
    throw new RangeError("Amount cannot be negative")
  }
  let counts: Map<bigint, bigint> = new Map()
  let remaining = amount
  for (const denomination of [25n, 10n, 5n, 1n]) {
    counts.set(denomination, remaining / denomination)
    remaining %= denomination
  }
  return counts
}

export function firstThenApply<T, U>(
  arr: T[],
  predicate: Predicate<T>,
  mapper: Mapper<T, U>
): U | undefined {
  const foundElement = arr.find(predicate)
  return foundElement ? mapper(foundElement) : undefined
}

export function* powersGenerator(base: bigint): Generator<bigint> {
  let exponent = 0n
  while (true) {
    let result: bigint = 1n
    for (let i = 0n; i < exponent; i++) {
      result *= base
    }
    yield result
    exponent++
  }
}

export async function meaningfulLineCount(filePath: string): Promise<number> {
  let count = 0
  const fileHandle = await open(filePath, "r")
  try {
    const readStream = fileHandle.createReadStream({ encoding: "utf-8" })
    let data = ""

    for await (const chunk of readStream) {
      data += chunk
      const lines = data.split("\n")
      if (data[data.length - 1] !== "\n") {
        data = lines.pop()!
      } else {
        data = ""
      }

      for (const line of lines) {
        const trimmedLine = line.trim()
        if (trimmedLine.length > 0 && !trimmedLine.startsWith("#")) {
          count++
        }
      }
    }

    if (data.length > 0) {
      const trimmedLine = data.trim()
      if (trimmedLine.length > 0 && !trimmedLine.startsWith("#")) {
        count++
      }
    }
  } finally {
    await fileHandle.close()
  }

  return count
}

// Write your shape type and associated functions here

interface Sphere {
  kind: "Sphere"
  radius: number
}

interface Box {
  kind: "Box"
  width: number
  length: number // Change 'height' to 'length' to match the test case
  depth: number
}

export type Shape = Sphere | Box

export function surfaceArea(shape: Shape): number {
  switch (shape.kind) {
    case "Sphere":
      return 4 * Math.PI * shape.radius * shape.radius
    case "Box":
      const { width, length, depth } = shape
      return 2 * (width * length + length * depth + width * depth) // Adjust for 'length' instead of 'height'
  }
}

export function volume(shape: Shape): number {
  switch (shape.kind) {
    case "Sphere":
      return (4 / 3) * Math.PI * Math.pow(shape.radius, 3)
    case "Box":
      const { width, length, depth } = shape
      return width * length * depth // Adjust for 'length' instead of 'height'
  }
}

export function toString(shape: Shape): string {
  switch (shape.kind) {
    case "Sphere":
      return `Sphere with radius ${shape.radius}`
    case "Box":
      return `Box with width ${shape.width}, length ${shape.length}, and depth ${shape.depth}`
  }
}

// Write your binary search tree implementation here
