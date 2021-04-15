;;; run with scheme < main.scm
;;; reference: https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref.html

;;; Exercise 2.1

; restricts arity of a procedure
(define (restrict-arity proc nargs) 
  (hash-table-set! arity-table proc nargs) 
  proc) 

; retrieves arity of a procedure
(define (get-arity proc) 
  (or (hash-table-ref/default arity-table proc #f) 
    (let ((a (procedure-arity proc))) ;arity not in table
      (procedure-arity-min a)))) 

(define arity-table (make-key-weak-eqv-hash-table)) 


; compose procedure checking arity
(define (compose f g)
  (assert (= (get-arity f) 1))
  (define (the-composition . args)
    (assert (= (get-arity g) (length args)))
    (f (apply g args)))
  (restrict-arity the-composition (get-arity g))
  the-composition)

; tests
(define foo (lambda (x) (* x x)))
(assert (= (get-arity foo) 1))

(define bar (lambda (x y) (+ x y)))
(assert (= (get-arity bar) 2))

(define foobar (compose foo bar))
(assert (= (get-arity foobar) 2))
(assert (= (foobar 2 3) 25))

(define foofoo (compose foo foo))
((assert (= (get-arity foofoo) 1))
(assert (= (foofoo 3) 27))