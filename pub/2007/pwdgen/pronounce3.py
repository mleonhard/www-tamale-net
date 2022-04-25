#!python
#
# pronounce3.py - a random pronounceable password generator
# Copyright (C) 2006 Michael Leonhard
import random, string, sys

# recursively builds a list of all possible strings containing NVowels 'a'
# and NConsonants 'b'.
def makeTemplates(PartialString, NVowels, NConsonants):
    Result = []
    if NVowels > 0:
        Result.extend(makeTemplates(PartialString+"a", NVowels - 1, NConsonants))
    if NConsonants > 0:
        Result.extend(makeTemplates(PartialString+"b", NVowels, NConsonants - 1))
    if 0 == NVowels and 0 == NConsonants:
        Result.append(PartialString)
    return Result

# checks if the string would be a legal template for building a password
def templateFilter(Template):
    A = ""
    B = ""
    for C in Template:
        # disallow two leading consonants
        if (A,B,C)==("","b","b"):
            return False
        # disallow three consecutive consonants
        if (A,B,C)==("b","b","b"):
            return False
        # disallow three consecutive vowels
        if (A,B,C)==("a","a","a"):
            return False
        A = B
        B = C
    
    # disallow two final consonants
    if (B,C)==("b","b"):
        return False
    return True

Templates = filter(templateFilter, makeTemplates("", 4, 4))
Vowels = ["a","e","i","o","u"]
Consonants = ["b","c","ch","d","f","g","h","j","k","l","m","n","p","ph" \
,"r","s","st","v","w","x","y","z"]

def genPwd():
    Password = ""
    Template = random.choice(Templates)
    for C in Template:
        if "a"==C:
            Password = Password + random.choice(Vowels)
        elif "b"==C:
            Password = Password + random.choice(Consonants)
        else:
            raise "ERROR"
    return Password

for N in range(1,21):
    print "%d. %s" % (N, genPwd())
