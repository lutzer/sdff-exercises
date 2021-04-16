;;; run with scheme < exercise2.scm
;;; reference: https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref.html

;;; Exercise 2.2

(define arity-table (make-key-weak-eqv-hash-table)) 
; restricts arity of a procedure
(define (restrict-arity proc arity) 
  (hash-table-set! arity-table proc arity)) 

; retrieves arity of a procedure
(define (get-arity proc) 
  (or (hash-table-ref/default arity-table proc #f) 
    (procedure-arity proc)))

(define (check-arity-for-arguments proc nargs)
  (let ((a (get-arity proc)))
    (and (>= nargs (car a)) 
      (or (eqv? (cdr a) #f) (<= nargs (cdr a) )))
    ))

; compose procedure checking min and max arity
(define (compose f g)
  (assert (check-arity-for-arguments f 1))
  (define (the-composition . args)
    (assert (check-arity-for-arguments g (length args)))
    (f (apply g args)))
  (restrict-arity the-composition (get-arity g))
  the-composition)

;;; Tests
(define foo (lambda (x) (* x x)))
(define bar (lambda (x y) (+ x y)))

(define foobar (compose foo bar))
(let ((a (get-arity foobar) ))
  (assert (eqv? (car a) 2))
  (assert (eqv? (cdr a) 2)))
(assert (= (foobar 2 3) 25))

(define foofoo (compose foo foo))
(let ((a (get-arity foofoo) ))
  (assert (eqv? (car a) 1))
  (assert (eqv? (cdr a) 1)))
(assert (= (foofoo 3) 27))
(foofoo 81)

(define func1 (compose + -))
(let ((a (get-arity func1) ))
  (assert (eqv? (car a) 1))
  (assert (eqv? (cdr a) #f)))
(assert (= (func1 3 2) 1))

(define func1 (compose - +))
(let ((a (get-arity func1) ))
  (assert (eqv? (car a) 0))
  (assert (eqv? (cdr a) #f)))
(assert (= (func1 3 3) (- 6)))