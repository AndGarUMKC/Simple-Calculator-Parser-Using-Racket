#lang racket

; [SOURCES] used to help with building the parser

; Demonstrates basic idea behing building a lexer using lexer library in racket
; couldn't figure it out so I wrote it all out without the library
; https://matt.might.net/articles/lexers-in-racket/

; Demonstrated sample logic for Top-Down Parser for Expressions
; https://www.cs.rochester.edu/u/nelson/courses/csc_173/grammars/parsing.html

; Demonstrated parse logic
; https://checkoway.net/teaching/cs275/2020-fall/minischeme.html

; Based Token defintion of example shown
; https://www.reddit.com/r/Racket/comments/qa70fn/need_help_with_lexerparser_for_calc_language/

; More detailed example for Parse program in racket for this assignment
; https://www.youtube.com/watch?v=sL6WL4VK5Ac





; token definitions
(define (getToken currentCharacter)
  ; Conditions for token definitions:
  (cond
    ; compare current character if you find a left parentheses return boolean true
    ; else false
    [(equal? #\( currentCharacter)
     ; if we return true then we found a left parentheses as the currentcharacter
     ; so then we make it a valid token
     'leftParentheses]             
    ; compare current character if you find a right parentheses return boolean true
    ; else false
    [(equal? #\) currentCharacter)
     ; if we return true then we found a right parentheses as the currentcharacter
     ; so then we make it a valid token
     'rightParentheses]
    ; compare current character if you find a '+' symbol return boolean true
    ; else false
    [(equal? #\+ currentCharacter)
     ; if we return true then we found a '+' symbol as the currentcharacter
     ; so then we make it a valid token
     'additionOperator]
    ; compare current character if you find a '-' symbol return boolean true
    ; else false
    [(equal? #\- currentCharacter)
     ; if we return true then we found a '-' symbol as the currentcharacter
     ; so then we make it a valid token
     'additionOperator]
    ; compare current character if you find a '/' symbol return boolean true
    ; else false
    [(equal? #\/ currentCharacter)
     ; if we return true then we found a '/' symbol as the currentcharacter
     ; so then we make it a valid token
     'multiplicationOperator]
    ; compare current character if you find a '*' symbol return boolean true
    ; else false
    [(equal? #\* currentCharacter)
     ; if we return true then we found a '*' symbol as the currentcharacter
     ; so then we make it a valid token
     'multiplicationOperator]
    ; compare current character if you find a '$' symbol return boolean true
    ; else false
    [(equal? #\$ currentCharacter)
     ; if we return true then we found a '$' symbol as the currentcharacter
     ; so then we make it a valid token
     'endOfFile]
    ; compare current character if you find a ':' symbol return boolean true
    ; else false
    [(equal? #\: currentCharacter)
     ; if we return true then we found a ':' symbol as the currentcharacter
     ; so then we make it a valid token
     'startOfAssignment]
    ; compare current character if character has the Unicode “Alphabetic” property return boolean true
    ; else false
    [(char-alphabetic? currentCharacter)
     ; if we return true then we found a ID (a,b,c,etc...) as the currentcharacter
     ; so then we make it a valid token
     'startOfIdentifier]
    ; compare current character if character has a Unicode “Numeric_Type” property value that is not None return true
    ; else false
    [(char-numeric? currentCharacter)
     ; if we return true then we found a number as the currentcharacter
     ; so then we make it a valid token
     'startOfNumber]
    ; compare current character if character has the Unicode “White_Space” property return true
    ; else false
    [(char-whitespace? currentCharacter)
     ; if we return true then we found a whitespace as the currentcharacter
     ; so then we make it a valid token
     'whitespace]
    ; else return invalid (error) because it is not part of the language
        [else
         ; call the error function
         'invalid]))


; lexer function that creates tokens according to the defintions
(define (tokenize line)
  ; function that iterates through the tokens according to the defined conditions
  (define (i++ characters currentToken tokens)
    (cond 
      ; check if characters are in the current token list
      [(empty? characters)
       ; check if currentToken is in a list
       (if (list? currentToken)  
           (cond
             ; check if we have a end of file token in the file
             ; else if no token found then we found an invalid program
             [(equal? (car currentToken)
                      'endOfFile)
              'invalid]
             ; check if we have a assignment token in the file
             ; else if no token found then we found an invalid program
             [(equal? (car currentToken)
                      'startOfAssignment)
              'invalid]
             ; check if we have a number token in the file
             ; else if no token found then we found an invalid program
             [(equal? (car currentToken)
                      'startOfNumber) (cons'num tokens)]
             ; check if we have a identifier token in the file
             ; else if no token found then we found an invalid program
             [(equal? (car currentToken)
                      'startOfIdentifier)
              (cond
                ; check if we have a read token in the file
                ; else if no token found then we found an invalid program
                [(equal? (getTokenInfo currentToken) "read")
                 (cons 'read tokens)]
                ; check if we have a write token in the file
                ; else if no token found then we found an invalid program
                [(equal? (getTokenInfo currentToken) "write")
                 (cons 'write tokens)]
               ; else add the id into the tokens list
               [else
                (cons 'id tokens)])]
             ; else invalid token
             [else
              'invalid])tokens)]
      ; check if the current token is a symbol
      [(symbol? currentToken)
       ; check the first token in characters list
       (let ([token (getToken (car characters))])
         (cond
           ;check if the token is an identifier 
           [(equal? token
            'startOfIdentifier)
            ;iterate through the pair of characters to find an identifier that matches the first one
            (i++ (cdr characters)
                 (list 'startOfIdentifier (list (car characters))) tokens)]
           ; check if the token is a number
           [(equal? token
            'startOfNumber)
            ; iterate through the list to find a number
            (i++ (cdr characters)
                 (list 'startOfNumber (list (car characters))) tokens)]
           ; check if the token is an EOF token
           [(equal? token
            'endOfFile)
            ; iterate to find an EOF token
            (i++ (cdr characters)
                 '(endOfFile) tokens)]
           ; check if the token is an assignment token
           [(equal? token
            'startOfAssignment)
            ; iterate through to find the second pair of the token
            (i++ (cdr characters)
                 '(startOfAssignment) tokens)]
           ; if no token found then it is invalid
           [(equal? token
            'invalid)
            'invalid]
           ; check if the token has a white space
           [(equal? token
            'whitespace)
            ; iterate through whitespace tokens
            (i++ (cdr characters)
                 'whitespace tokens)]
           ; else iterate through tokens
           [else
            (i++ (cdr characters)
                 token (cons token tokens))]))]
         ; if no invalid whitespaces then return a valid token                    
         [else
          (validToken characters currentToken tokens)]))


  ; function that determines if a token is valid
  (define (validToken characters currentToken tokens)
    (cond
      ; valid tokens according to the terminals
      [(equal? (car currentToken)
               'startOfIdentifier)
       (if (terminal? (car characters))
           ; check for read terminal then iterate
           (cond
             [(equal? (getTokenInfo currentToken) "read")
              (i++ (cdr characters)
                   'read (cons'read tokens))]
             ; check for write terminal then iterate
             [(equal? (getTokenInfo currentToken) "write")
              (i++ (cdr characters)
                   'write (cons'write tokens))]
             ; else no read or write then continue to the next tokens
             [else
              (i++ characters
                   'id (cons'id tokens))])
           ; if the token is a valid alphabetic letter then iterate through but if not found return invalid
           (if (char-alphabetic? (car characters))
               (i++ (cdr characters)
                    (list
                     'startOfIdentifier (cons (car characters) (second currentToken))) tokens)
               'invalid))]
      ; check if we are at a number
      [(equal? (car currentToken)
               'startOfNumber)
       ; if it is a terminal then iterate through the num tokens
       (if (terminal? (car characters))
           (i++ characters
                'num (cons'num tokens))
           ; if the token is numeric then check the next token else invalid
           (if (char-numeric? (car characters))
               (i++ (cdr characters)
                    (list
                     'startOfNumber (cons (car characters) (second currentToken))) tokens)
               'invalid))]
      ; check if we reached the end of file 
      [(equal? (car currentToken)
               'endOfFile)
       ; if we encounter $ then we reached the end of a valid program else invalid
       (if (equal? #\$ (car characters))
           (cons'EOF tokens)
           'invalid)]
      ; if we find the : symbol
      [(equal? (car currentToken)
               'startOfAssignment)
       ; if the : matches with a = then it is a valid assignment operator
       (if (equal? #\= (car characters))
           ; iterate through to find the pair
           (i++ (cdr characters)
                'assignmentOperator (cons 'assignmentOperator tokens))
           'invalid)]
      ; else invalid if previous conditons return false
      [else
       'invalid]))
  ; iterate through the string and assign it a line
  (let ([tokens
         (i++ (string->list line)
              'start '())])
    ; reverse the list
    (if (list? tokens)
        (reverse tokens)tokens)))


; defines if we found a valid terminal characters
(define (terminal? char)
  (not (not (member (getToken char)
                    '(leftParentheses rightParentheses endOfAssignment additionOperator multiplicationOperator whitespace endOfFile)))))


; tokens added to list in reverse to fix the order of the tokens
(define (getTokenInfo token)
  (list->string (reverse (second token))))


; parse logic to determine if it accepts the line if not through a syntax error for the line with the error
(define (parseLineFromFiles lines)
  (define (i++ lines lineNumber)
    (cond
      [(let ([lineResult (parseLineFromFile (car lines))])
         (cond
           ; if the line result is accepted iterate to the next line 
           [(equal? lineResult
                    'accept)(i++ (cdr lines) (+ lineNumber 1))]
           ; if the line has an error then we call a syntax error on that line and return the number of the line
           ; and return it in the right format
           [(equal? lineResult
                    'invalid) (~a "Syntax error found on line " lineNumber)]
           ; if we reach the end of file then we accept the program
           [(equal? lineResult
                    'eof)"Accept"]
           ; if lines list is empty then keep iterating until a line is found
           [else
            (i++ lines 1)]))]))
  ; if the lines list is empty then we found a empty file
  (if (empty? lines)
      "File is empty no program found. Cannot parse a empty file."(i++ lines 1)))


; parse line by line instead of the whole file
(define (parseLineFromFile line)
  (parseTokens (tokenize line)))


; parse token line by line by checking it against the calculator grammar
(define (parseTokens tokens)
  (define (parseRead tokens)
    ; conditions check if tokens are empty and if it is then invalid
    (cond
      [(empty? tokens)'invalid]
      ;check first token to id and if true then find a pair
      [(equal? (car tokens)'id)
       (if (empty? (cdr tokens))
           ;if pair found then accept
           'accept
           ;else invalid
           'invalid)]))

  
  ; function that parses the write function according to the calculator grammar
  (define (parseWrite tokens)
    ; conditions if no tokens then invalid
    (cond
      [(empty? tokens)
       'invalid]
      ; else if there is a write then call the expression function
      [else
       (parseExpression tokens)]))

  
  ; function that parses the assignment operator according to the calculator grammar
  (define (parseAssignment tokens)
    (cond
      [(empty? tokens)
       'invalid]
      ; if the first token is an assignment operator :=
      [(equal? (car tokens)
               'assignmentOperator)
       ; then check for the pair
       (parseExpression (cdr tokens))]
      ; else no asignment operator found so violates grammar
      [else
       'invalid]))

  
  ; function that parses the expression according to the calculator grammar
  (define (parseExpression tokens)
    ; find a pair of id or num or Left parentheses
    (if (or (equal? (car tokens) 'id)
            (equal? (car tokens) 'num)
            (equal? (car tokens) 'leftParentheses))
        ; evaluates the remaining tokens from left to right then check the tokens if they are a term
        (let ([remainingTokens (parseTerm tokens)])
          ; if the previous tokens are not found then it violates the grammar
          (if (equal? remainingTokens 'invalid)
              'invalid (parseTermTail remainingTokens)))
        'invalid))

  
  ; function that parses the term according to the calculator grammar
  ; same logic as parse expression
  (define (parseTerm tokens)
    (if (or (equal? (car tokens) 'id)
            (equal? (car tokens) 'num)
            (equal? (car tokens) 'leftParentheses))
        (let ([remainingTokens (parseFactor tokens)])
          (if (equal? remainingTokens 'invalid)
              'invalid (parseFactorTail remainingTokens)))
        'invalid))

  
  ; function that parses term tail according to the calculator grammar
  (define (parseTermTail tokens)
    
    (cond
      ; check if the tokens list is empty if so then add the tokens
      [(empty? tokens) tokens]
      ; checks if tokens according to the grammar are found and which is after previous token
      [(or (equal? (car tokens)'rightParentheses)
           (equal? (car tokens) 'id)
           (equal? (car tokens) 'num)
           (equal? (car tokens) 'read)
           (equal? (car tokens) 'write)
           (equal? (car tokens) 'EOF)) tokens]
      ; checks if the next token is an addition operator
      [(equal? (car tokens)'additionOperator)
       ; check remaining tokens from left to right and see if it is a term that can be parsed
       (let ([remainingTokens (parseTerm (cdr tokens))])
         ; if no remaining tokens then nothing to parse
         (if (equal? remainingTokens 'invalid)
             'invalid (parseTermTail remainingTokens)))]
      [else
       'invalid]))

  
  ; function that parses the factor according to the calculator grammar
  (define (parseFactor tokens)
    ; checks if the token is an id or number and check the pair
    (cond
      [(or (equal? (car tokens) 'id)
           (equal? (car tokens) 'num))
       (cdr tokens)]
      ; check if there is left parentheses that completes the pair
      [(equal? (car tokens)'leftParentheses)
       ; from left to right check token pairs
       (let ([remainingTokens (parseExpression (cdr tokens))])
         (cond
           [(equal? remainingTokens 'invalid)
            'invalid]
           [(empty? remainingTokens)'invalid]
           ;[(equal? (remainingTokens)'invalid)]
           [(equal? (car remainingTokens)'rightParentheses)
            (cdr remainingTokens)]))]
      [else
       'invalid]))

  
  ; function that parses the factor tail according to the calculator grammar
  (define (parseFactorTail tokens)
    (cond
      ; checks if the token list is empty if it is then add token
      [(empty? tokens) tokens]
      ; check if the factor is followed by the right terminals
      [(or (equal? (car tokens)'rightParentheses)
           (equal? (car tokens)'id)
           (equal? (car tokens) 'num)
           (equal? (car tokens) 'read)
           (equal? (car tokens) 'write)
           (equal? (car tokens) 'additionOperator)
           (equal? (car tokens) 'EOF))
       tokens]
      ; check if there is a multiplication operator
      [(equal? (car tokens)
               'multiplicationOperator)
       ; read left to right and check the pairs of factors
       (let ([remainingTokens (parseFactor (cdr tokens))])
         ; if nothing found or false then return error
         (if (equal? remainingTokens 'invalid)
             'invalid (parseFactorTail remainingTokens)))]
      [else
       'invalid]))

  
  ; function that parses the statement according to the calculator grammar
  (define (parseStatement tokens)
    ; check if the statement is valid
    (cond
      [(empty? tokens)'accept]
      ; check to see if there is a token after read then 
      [(equal? (car tokens)'read)
       ; then parse the pair
       (parseRead (cdr tokens))]
      ; check if there is an id in the statement
      [(equal? (car tokens)'id)
       ; if no valid statement then prompt error
       (if (empty? (parseAssignment (cdr tokens)))'accept
           'invalid)]
          ; check if the first token in the statement is a number or a write
          [(equal? (car tokens)'num)'invalid]
          [(equal? (car tokens)'write)
              ; if there is no write then accept otherwise invalid
              (if (empty? (parseWrite (cdr tokens)))'accept
                  'invalid)]
                  ; check if the first token is any of the following if not then grammar is not followed
                  [(equal? (car tokens)'assignmentOperator)
                   'invalid]
                  [(equal? (car tokens)'multiplicationOperator)
                   'invalid]
                  [(equal? (car tokens)'additionOperator)
                   'invalid]  
                  [(equal? (car tokens)'EOF)
                   'eof]
                  
                      [else
                       (parseStatement (cdr tokens))]))
  ; if no tokens found then invalid statement
  (if (equal? tokens' invalid)
      'invalid (parseStatement tokens)))


; function to call parse on a file
(define (parse file)
  (parseLineFromFiles (readLinesFromFile file)))


;function to open the file and check the contents and check if it is closed
(define (fileContents file)
  (string-trim (port->string (open-input-file file) #:close? #t)))


; split strings using regex on whitespace
(define (readLinesFromFile file)
  ; A #rx or #px starts a regular expression.
  ; https://www.reddit.com/r/Racket/comments/uat33g/unexpected_regex_behaviour/
  (string-split (fileContents file) #px"(\r\n|\r|\n)"))


;Just uncomment each line individually and included an empty txt file

(parse "input01.txt") ; correct syntax so it should accept
(parse "input02.txt") ; correct syntax so it should accept (some semantic problems)
(parse "input03.txt") ; extra right parentheses (error on line 3)
(parse "input04.txt") ; extra left parentheses (error on line 3)
(parse "input05.txt") ; two operators (error on line 1)
(parse "emptyInput.txt") ; empty file with no $$ termination


