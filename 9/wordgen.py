import argh
import random

consonants = [
    'n', 'ng',
    't', 'd', 'c', 'g', '\'',
    'sh', 'ch',
    'x', 'k',
    'l', 'r', 'rh']
vowels = ['a', 'a', 'aa', 'y', 'y', 'yy', 'e', 'e', 'ee', 'i', 'i', 'ii']
cons_nuc = ['l', 'r', 'rh']
cons_nuc_oc = [cons for cons in consonants if cons not in cons_nuc]
cons_nuc_chance = 0.1


def gen_syllable():
    if random.uniform(0, 1.0) < cons_nuc_chance:
        nuc = random.choice(cons_nuc)
        ons = random.choice(cons_nuc_oc) if random.uniform(0, 1) < 0.6 else ""
        coda = random.choice(cons_nuc_oc) if random.uniform(0, 1) < 0.6 else ""
        return ons + nuc + coda
    else:
        nuc = random.choice(vowels)
        ons = random.choice(consonants) if random.uniform(0, 1) < 0.6 else ""
        coda = random.choice(consonants) if random.uniform(0, 1) < 0.6 else ""
        return ons + nuc + coda


def gen_word(num, max_syllables=4):
    for _ in range(int(num)):
        word = "".join([
            gen_syllable()
            for _ in range(random.randint(1, max_syllables))
        ])
        print(word)


argh.dispatch_command(gen_word)
