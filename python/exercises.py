from dataclasses import dataclass
from collections.abc import Callable
from math import sqrt


def change(amount: int) -> dict[int, int]:
    if not isinstance(amount, int):
        raise TypeError('Amount must be an integer')
    if amount < 0:
        raise ValueError('Amount cannot be negative')
    counts, remaining = {}, amount
    for denomination in (25, 10, 5, 1):
        counts[denomination], remaining = divmod(remaining, denomination)
    return counts




# Write your first then lower case function here
def first_then_lower_case(sequence, predicate, / ):
    for element in sequence:
        if predicate(element) and isinstance(element,str):
            return element.lower()
    return None





def powers_generator(*, base: int, limit: int):
    power = 1
    while power <= limit:
        yield power
        power *= base




# Write your say function here
# def say(first_word=""):
#     def next_word(word=None):
#         if word is None:
#             return first_word
#         # Simply append the new word with exactly one space in between
#         return say(first_word + " "+ word)
#     return next_word




def say(word=None):
    result = []

    def next_word(input_word=None):
        if input_word is None:
            return ''.join(result)
        
        if input_word == " ":
            if result and result[-1] != " ":
                result.append(" ")
        else:
            if result and result[-1] != " ":
                result.append(" ")
            result.append(input_word)
        
        return next_word

    return next_word(word)




def meaningful_line_count(filename: str) -> int:
    try:
        with open(filename, 'r') as file:
            count = 0
            for line in file:
                stripped_line = line.strip()
                if stripped_line and len(stripped_line) > 1 and not stripped_line.startswith('#'):
                    count += 1
            return count
    except FileNotFoundError:
        raise FileNotFoundError(f"No such file: '{filename}'")






class Quaternion:
    def __init__(self, a:float, b:float, c:float ,d:float) -> None:
        self.a = a
        self.b = b
        self.c = c
        self.d = d

    def __add__(self, other):
        return Quaternion(
            self.a + other.a,
            self.b + other.b,
            self.c + other.c,
            self.d + other.d,
        )


    def __eq__(self, other):
        return (self.a == other.a and 
                self.b == other.b and
                self.c == other.c and
                self.d == other.d)

    def __mul__(self, other):
        a1, b1, c1, d1 = self.a, self.b, self.c, self.d
        a2, b2, c2, d2 = other.a, other.b, other.c, other.d
        return Quaternion(
            a1*a2 - b1*b2 - c1*c2 - d1*d2,
            a1*b2 + b1*a2 + c1*d2 - d1*c2,
            a1*c2 - b1*d2 + c1*a2 + d1*b2,
            a1*d2 + b1*c2 - c1*b2 + d1*a2
        )


    def __str__(self):
        terms = []
        if self.a != 0:
            terms.append(f"{self.a}")
        if self.b != 0:
            terms.append(f"{'+' if self.b > 0 else ''}{self.b}i")
        if self.c != 0:
            terms.append(f"{'+' if self.c > 0 else ''}{self.c}j")
        if self.d != 0:
            terms.append(f"{'+' if self.d > 0 else ''}{self.d}k")
        return ''.join(terms) if terms else '0'


    def __repr__(self):
        return f"Quaternion({self.a}, {self.b}, {self.c}, {self.d})"

    @property
    def coefficients(self):
        return(self.a, self.b, self.c, self.d)

    @property
    def conjugate(self):
        return Quaternion(self.a, -self.b, -self.c, -self.d)

    @property
    def norm(self):
        return sqrt(self.a**2 + self.b**2 + self.c**2 + self.d**2)


