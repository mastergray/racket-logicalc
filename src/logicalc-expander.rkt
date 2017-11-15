;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module that evalutates a logical expression as a parsed syntax object ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module logicalc-expander racket

  ;; Creates a reference to the current namespace:
  ;; (https://stackoverflow.com/questions/20778926/mysterious-racket-error-define-unbound-identifier-also-no-app-syntax-trans)
  (define-namespace-anchor anc)
  (define ns (namespace-anchor->namespace anc))

  ;; Stores variable env:
  (define env '())
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Procedures exported from module ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (provide logicalc-expander)

  ;; Evalutes logical expression for a given syntax object and hash table
  (define (logicalc-expander stx hash)
    ;; Sets env
    (set! env hash)
    ;; "Expands" syntax object returned from brag parse
    (eval stx
     ;; Eval requires a namespace to have access to procedures used for resolving an expression:
     ns))

  ;;;;;;;;;;;;;;;;;;;;;;
  ;; Local Procedures ;;
  ;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Returns a boolean for a given string:
  (define (truth-value arg)
    ;; Only returns false if string is equal to 0:
    (cond
      [(string? arg) (if (string=? arg "0") #f #t)]
      [(number? arg) (if (= arg 0) #f #t)]
      [#t #t]))
  
  ;; Returns a boolean from a set variable
  (define (VAR . args)
    ;; Build identifiers from given strings:
    (define id (foldr
                (lambda (result arg) (string-append result arg))
                ""
                args))
    ;; Returns truth-value from set enviroment:
    (truth-value (hash-ref env id)))
    
  ;; Returns the result of an evaluated "well-formed formula" as either "T" or "F":
  (define (WFF result)
    (cond
      [(eq? result #t) "T"]
      [(eq? result #f) "F"]))

  ;; Returns string value as boolean:
  (define (VAL str)
    (truth-value str))
   
  ;; Defines form for returning first argument if no argument is given, otherwise returns result of operation:
  (define (op-form op p [q null])
    (if (null? q) p (op p q)))
  
  ;; Defines logical conjunction operation using op-from
  (define (op-and p [q null])
    (op-form (lambda (p q) (and p q)) p q))

  ;; Defines logical disjunction operation using op-from
  (define (op-or p [q null])
    (op-form (lambda (p q) (or p q)) p q))
  
  ;; Define logical equivlance operation as (p > q) * (q > p) using op-form:
  (define (op-eqv p [q null])
    (op-form (lambda (p q) (and (op-imp p q) (op-imp q p))) p q))
  
  ;; Defines logical implication operation as ~(p * ~q) using op-form:
  (define (op-imp p q)
    (op-form (lambda (p q) (not (and p (not q)))) p q))
     
  ;; Procedure for applying logical conjunction to any number of arguments:
  (define (AND . args)
    (foldr
     (lambda (result arg) (op-and arg result))
     (car args)
     (cdr args)))

  ;; Procedure for applying logical disjunction to any number of arguments:   
  (define (OR . args)
    (foldr
     (lambda (result arg) (op-or arg result))
     (car args)
     (cdr args)))

  ;; Procedure for applying logical implication to any number of arguments:   
  (define (IMP . args)
    (foldr
     (lambda (result arg) (op-imp arg result))
     (car args)
     (cdr args)))

  ;; Procedure for applying logical equivalnce to any number of arguments:   
  (define (EQV . args)
    (foldr
     (lambda (result arg) (op-eqv arg result))
     (car args)
     (cdr args)))

  ;; Procedure that applies logical negation to an argument:
  (define (NOT arg)
    (not arg))
)