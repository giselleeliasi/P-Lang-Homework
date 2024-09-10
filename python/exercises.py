from dataclasses import dataclass
from collections.abc import Callable


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
def say(first_word=""):
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


# Write your Quaternion class here
