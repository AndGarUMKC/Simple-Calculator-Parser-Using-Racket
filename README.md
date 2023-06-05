# Simple-Calculator-Parser-Using-Racket
This project demonstrates the use of functional programming principles and the Racket programming language to build a simple calculator parser. The parser is designed to take an input and determine if it follows the grammar rules specified for the calculator language. It can identify syntax errors and output an error message in the form of **"Syntax error on Line N"** when necessary.

## Functional Programming and Racket

**Racket** is a functional programming language that supports various paradigms, including imperative, object-oriented, and functional programming styles. Functional programming focuses on writing programs by composing pure functions, avoiding mutable state, and emphasizing immutability and recursion.

In this project, functional programming principles are utilized to implement the parser. The recursive descent technique is primarily employed to parse the input and determine if it adheres to the grammar rules. Recursive descent is a top-down parsing approach where each rule in the grammar corresponds to a function. These functions are recursively called to traverse the input and validate its syntax.


### The Following Grammar was used:
**LL(1) grammar for a simple calculator language.** The grammar defines the valid syntax and structure of the calculator expressions. The rules in the grammar specify how different elements such as numbers, operators, and parentheses can be combined to form valid calculator expressions. Top down predictive parser using this grammar.
#

<img width="481" alt="image" src="https://user-images.githubusercontent.com/70826986/196506145-2ac3d208-79dd-4c99-94a0-a7a168d0c9fd.png">

### The First, Follow and Predict set for this simple Calculator Language:
#
<img width="950" alt="image" src="https://user-images.githubusercontent.com/70826986/196510815-6c22ae60-d745-458f-ab38-e92642e911f5.png">

