;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module that transforms a logical expression into a parsed syntax object ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module logicalc-reader racket

  ;;;;;;;;;;;;;;;;
  ;; Submodules ;;
  ;;;;;;;;;;;;;;;;

  (require brag/support)             ;; For transforming a BNF grammar into a parser
  (require "logicalc-grammar.rkt")   ;; Defines BNF grammar for logical expressions

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Procedures exported from module ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (provide logicalc-reader)
  (provide logicalc-debugger)
  
  ;; Transforms a string into a parsed syntax object:
  (define (logicalc-reader str)
    ;; Transforms tokens into a syntax object to be "expanded" into an evaluated expression:
    (parse 
     ;; Creates list of brag tokens from port input string:
     (tokenize
      ;; Reader used by tokenize procedure requires input from a port:
      (open-input-string str))))

  ;; Used for debuging syntax object returned from parser:
  (define (logicalc-debugger str)
   (parse-to-datum (tokenize (open-input-string str))))

  ;;;;;;;;;;;;;;;;;;;;;
  ;; Local procedues ;;
  ;;;;;;;;;;;;;;;;;;;;;

  ;; Transfrom input read from 'in' input port into a list of tokens:
  (define (tokenize in [result empty])
    ;; 'let' defines a local variable for each character being read from the input port 'in':
    (let ([char (read-char in)])
      ;; EOF is returned by reader when port being read is empty:
      (if (eof-object? char)
          ;; Result is revered since cons appends list backwards (FILO):
          (reverse result)
          ;; Create brag tokens that can be transformed into a syntax object by the brag 'parse' procedure:
          (cond
            [(char-numeric? char) (tokenize in (cons (token (string char) (string char)) result))]
            [(char-alphabetic? char) (tokenize in (cons (token (string char) (string char)) result))]
            [(eqv? char #\~) (tokenize in (cons (token "~" "~") result))]
            [(eqv? char #\*) (tokenize in (cons (token "*" "*") result))]
            [(eqv? char #\^) (tokenize in (cons (token "^" "^") result))]
            [(eqv? char #\>) (tokenize in (cons (token ">" ">") result))]
            [(eqv? char #\=) (tokenize in (cons (token "=" "=") result))]
            [(eqv? char #\)) (tokenize in (cons (token ")" ")") result))]
            [(eqv? char #\() (tokenize in (cons (token "(" "(") result))]
            ;; Continue reading from input port:
            [else (tokenize in result)])))) 
)
  






