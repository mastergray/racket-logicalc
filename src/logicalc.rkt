#lang racket
;; Dependencies:
(require "logicalc-reader.rkt")    ;; Transforms a logical expression into a parsed syntax object
(require "logicalc-expander.rkt")  ;; Evalutates arithmetic exprssion as a parsed syntax object
(require "logicalc-value-set.rkt") ;; Create the values used to evaluate a logical expression with
(require readline/readline)        ;; Handles command line I/0

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppport Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Returns list of variable names from a given string:
  (define (create-var-table str)
    (remove-duplicates (regexp-match* #rx"[A-Za-z]+" str)))

  ;; Retuns a hash table that binds a list of variable names to a list of values:
  (define (create-env var-table values)
    (make-hash (map cons var-table values)))

  ;; Prints out the values used with, and the result of , a logical expression:
  (define (print-output val-list exp-result)
    (define truth-values
      (foldl
       (lambda (val result)
         (if (= 0 val)
           (string-append result "F | ")
           (string-append result "T | ")))
       (string)
       val-list))
    ;; Append truth values to result of expression:
    (string-append truth-values exp-result))

 ;; Prints the result for every possible set of values for given logical expression:
 (define (solve-input exp)
   (define stx (logicalc-reader exp))                       ;; Syntax object from string expression
   (define var-table (create-var-table exp))                ;; List of every variable in expression 
   (define val-set (logicalc-value-set (length var-table))) ;; List of every combination of truth-values
   ;; Loops through each list of truth values:
   (for ([truth-values val-set])
     ;; Creates an "enviroment" used to resolve the given expression:
     (define env (create-env var-table truth-values))
     ;; Print result of resolving expression:
     (displayln (print-output truth-values (logicalc-expander stx env))))
   ;; Add an extra line between the truth table and next expression:
   (displayln "\n"))

 ;; Prints out everything this thing can do:
 (define (show-help)
   (displayln "\n")
   (displayln "|--------------------|")
   (displayln "| Operators          |")
   (displayln "|--------------------|")
   (displayln "| * | AND            |")
   (displayln "| ^ | OR             |")
   (displayln "| > | IF/THEN        |")
   (displayln "| = | IF/ONLY IF     |")
   (displayln "| ~ | NOT            |")
   (displayln "|--------------------|")
   (displayln "| Truth Values       |")
   (displayln "|--------------------|")
   (displayln "| 0       | FALSE    |")
   (displayln "| 1       | TRUE     |")
   (displayln "|[A-Za-z] | VARIABLE |")
   (displayln "|--------------------|")
   (displayln "| q | Quit           |")
   (displayln "| h | Help           |")
   (displayln "|--------------------|")
   (displayln "\n")
   (main (readline "?: ")))

;;;;;;;;;;;;;;;;;;
;; Main Program ;;
;;;;;;;;;;;;;;;;;;

 ;; Defines program for entering an expression from command line:
 (define (main input)
   (cond
    ;; Enter "q" stops the progam:
    [(equal? input "q") (displayln "Done.")]
    ;; Enter "h" shows "help" info:
    [(equal? input "h") (show-help)]
    (else
      ;; Prints output of solved expression to command line input:
      (solve-input input)
      ;; Gets ready for next expression to solve from command line: 
      (main (readline "?: ")))))

;; Starts main program:
(displayln "Solve Something... (enter \"q\" to quit, \"h\" for help)\n")
(main (readline "? "))