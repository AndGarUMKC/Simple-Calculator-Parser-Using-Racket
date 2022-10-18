# Simple-Calculator-Parser-Using-Racket
Racket is a functional language. Parser was created off a simple calculator grammar in order to create a lexer and parser to see if some input file would be accepted or call an error whenever there is a syntax error and output **"Syntax erron on N line"**

Mostly built using recursive decent in order to determine if the inputs can parse.

### The Following Grammar was used:
LL(1) grammar for a simple calculator language. Top down predictive parser using this grammar.
#

<img width="481" alt="image" src="https://user-images.githubusercontent.com/70826986/196506145-2ac3d208-79dd-4c99-94a0-a7a168d0c9fd.png">

### The First, Follow and Predict set for this simple Calculator Language:
#
<img width="950" alt="image" src="https://user-images.githubusercontent.com/70826986/196510815-6c22ae60-d745-458f-ab38-e92642e911f5.png">

