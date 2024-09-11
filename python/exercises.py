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
def first_then_lower_case(sequence, predicate):
    for element in sequence:
        if predicate(element) and isinstance(element,str):
            return element.lower()
    return None




# Write your powers generator here
def powers_generator(*, base: int, limit: int):
    power = 1
    while power <= limit:
        yield power
        power *= base




# Write your say function here
def say(first_word=" "):
    def next_word(word=None):
        if word is None:
            return first_word
        return say(first_word + " " + word)
    return next_word

    # words = []
    # def chain(next_word=None):
    #     if next_word is not None:
    #         words.append(next_word)
    #         return chain
    #     else:
    #         return " ".join(words)
    # return chain if word == "" else chain(word)




# Write your line count function here
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





# Write your Quaternion class here
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


