# Ocaml Polynomial Simplifier

Code Authored by: Kyle Bassignani Daniel Tymecki

Features impllemented: add terms, multiply terms, raise terms to power. Project will parse polynomial text and recursively expand and simplify the polynomial.

(x+1)^17 is the maximum power that can be calculated without a stack overflow.

21 tests have been written and are in the tests/ folder.

Tests are run with the given command - make tests

Bonus: divide can work with two terms being divided. Test 21 shows this. Dividing a Times or Plus type will not work.

To run, simply call "make tests" at it will cat the contents of all files to the terminal.
