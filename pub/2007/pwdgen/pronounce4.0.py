#!/usr/bin/env python3
"""Random pronounceable password generator"""
__author__ = "Michael Leonhard"
__copyright__ = "Copyright 2007-2022, Michael Leonhard"
__license__ = "MIT"
__version__ = "4.0"
__maintainer__ = "Michael Leonhard"
__email__ = "uic@tamale.net"
__status__ = "Production"

import random
import string
import sys

def is_valid_template(template):
    """Checks if the string would be a legal template for building a password"""
    if template.startswith("bb"): # two leading consonants
        return False
    if "bbb" in template: # three consecutive consonants
        return False
    if "aaa" in template: # three consecutive vowels
        return False
    if template.endswith("bb"): # two final consonants
        return False
    return True

def make_templates(n_vowels, n_consonants, prefix = ""):
    """Returns all template strings with n_vowels 'a' and n_consonants 'b'"""
    templates = []
    if n_vowels > 0:
        new_templates = make_templates(n_vowels - 1, n_consonants, prefix + "a")
        templates.extend(new_templates)
    if n_consonants > 0:
        new_templates = make_templates(n_vowels, n_consonants - 1, prefix + "b")
        templates.extend(new_templates)
    if 0 == n_vowels and 0 == n_consonants:
        if is_valid_template(prefix):
            templates.append(prefix)
    return templates

def make_password(prng, templates):
    template = prng.choice(templates)
    vowels = ["a","e","i","o","u"]
    consonants = ["b","c","ch","d","f","g","h","j","k","l","m","n","p","ph",
                  "r","s","st","v","w","x","y","z"]
    digits = ["0","1","2","3","4","5","6","7","8","9"]
    pronounceable_string_chars = []
    for x in template:
        if x == "a":
            random_vowel = prng.choice(vowels)
            pronounceable_string_chars.append(random_vowel)
        elif x == "b":
            random_consonant = prng.choice(consonants)
            pronounceable_string_chars.append(random_consonant)
        else:
            raise "ERROR"
    pronounceable_string = "".join(pronounceable_string_chars)
    capitalized_pronounceable_string = pronounceable_string.capitalize()
    random_digit = prng.choice(digits)
    password = capitalized_pronounceable_string + random_digit
    return password

if __name__ == '__main__':
    prng = random.SystemRandom()
    templates = make_templates(4, 4)
    for n in range(1,21):
        password = make_password(prng, templates)
        print(f"{n}. {password}")
    print("Press enter.")
    sys.stdin.readline()
    sys.exit(0)
