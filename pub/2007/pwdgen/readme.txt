Source code for paper "A Comparative Study of Three Random Password Generators" by Michael D. Leonhard and V.N. Venkatakrishnan.
Paper webpage: http://tamale.net/pub/2007/pwdgen/

Submitted to eit 2007!

=={ AlphaNum }==
Simply run the python script at the prompt:

$ python alphanum.py
1. SP3u5Z
2. wclmcJ
3. PCrbB2
4. Ws9ASE
 ...

=={ Diceware }==

Before using diceware.py, you must download and prepare Arnold G. Reinhold's wordlist (see http://www.diceware.com/).

$ wget http://world.std.com/~reinhold/diceware.wordlist.asc
 ...
$ cat diceware.wordlist.asc | grep "^[123456].*" |cut -f 2 > words
$ python diceware.py
1. lloyd may bayda
2. 59 craze vk
3. smash tithe beast
4. clan sonic toxic
 ...

=={ Pronounce3 }==
Run it at the prompt:

$ python pronounce3.py
1. edociwfa
2. upwokaro
3. iebxoygi
4. ukzolluu
 ...
