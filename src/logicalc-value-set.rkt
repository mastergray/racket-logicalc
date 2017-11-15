;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module that generates truth values to be evaluated by a logical expression
;;
;; The idea here is that if we know the number of variables in an expression,
;; then we should be able to generate every combination of truth values for that
;; expression by counting every binary number from 0 to some limit - where that
;; limit is the maximum number of "true" truth value for that expression.
;;
;; So, given a * b - we have two variabes "a" and b", that means we can have at most
;; two true values (1 1). If we convert (1 1) to a decimal number, we get 3 - assuming the
;; most significant bit is 2 (that is if I understand MSB correctly anyway..) - which also happens
;; to be the number of variables in "a * b".  We would then count from 0 to 3, converting
;; each decimal into binary using an MSB equal 2, and end up with following list of combinations:
;;
;; (0 0) - 0
;; (0 1) - 1
;; (1 0) - 2
;; (1 1) - 3
;;
;; The same is true for any number of variables in a given expression.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module logicalc-value-set racket

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Procedures exported from module ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (provide logicalc-value-set)
 
;; Returns a list of every combination of truth-values for a given number of variables:
(define (logicalc-value-set size)
  (value-set-msb size
   (value-set-range
    (value-set-limit size))))
  
;; Returns a list of 1's for a given integer -
;; This is suppose to model the "limit" of truth values
;; given some number of variables
;; (e.g., an expression with three variables would at most have three "true" truth values, or (1 1 1):
(define (value-set-limit size)
   (if (> size 0)
       (cons 1 (value-set-limit (- size 1)))
        '()))
  
;; Converts every decimcal number to a binary number for a given limit:
(define (value-set-range limit [range '((0))])
  (let ([limitAsDec (toDec limit)])
    (if (> limitAsDec 0)
      (value-set-range (toBin (- limitAsDec 1)) (cons limit range))
      range)))

;; Ensures all values in range have the same number of digits:  
(define (value-set-msb msb range [result '()])
  (if (null? range)
      (reverse result)
      (value-set-msb msb (cdr range) (cons (setMSB msb (car range)) result))))

;; Converts a decimal number into list of 0's and 1's:
(define (toBin num [result '()])
  (if (> num 0)
      (toBin
       (quotient num 2)
       (append result (list (modulo num 2))))
      (reverse result)))

;; Converts a list of 0's and 1's into a decimal number:  
(define (toDec binList [result 0])
  (if (null? binList)
      result
      (let ([radix (- (length binList) 1)])
        (if (eq? (car binList) 1)
            (toDec (cdr binList) (+ result (expt 2 radix)))
            (toDec (cdr binList) result)))))

  ;; Appends 0's to a list of 0's and 1's so that it has as many digits as given by msb:
(define (setMSB msb binList)
  (if ( > msb (length binList))
      (setMSB msb (cons 0 binList))
      binList))

)