;;; run with scheme < exercise2.scm
;;; reference: https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref.html
;;; run with scheme < exercise23.scm

;;; Exercise 2.3

(load "_arity.scm")

; compose accepting multiple return values
(define (compose2 f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args) f)))
  the-composition)

; parallel combine procedure that accepts multiple return values from f and g feeding them into h
(define (parallel-combine h f g)
  (let ((a (get-arity-combined f g)))
    (assert (not (and (number? (cdr a)) (< (cdr a) (car a)))))
    (define (the-composition . args)
      (assert (check-arity-for-arguments g (length args)))
      (assert (check-arity-for-arguments f (length args)))
      (let ((res1 (apply f args) ) (res2 (apply g args)))
        ;need to find the concatenate values operator for this to work
      )
    (restrict-arity the-composition a)
    the-composition)))

;;; Tests
(define foo (lambda (x) (* x x)))
(define bar (lambda (x y) (+ x y)))
(define foo-mult (lambda (x) (values x x)))

(define barfoofoo (parallel-combine bar foo foo))
(let ((a (get-arity barfoofoo) ))
  (assert (eqv? (car a) 1))
  (assert (eqv? (cdr a) 1)))
(assert (barfoofoo 2) 8)

(define barplusminus (parallel-combine bar + -))
(let ((a (get-arity barplusminus) ))
  (assert (eqv? (car a) 1))
  (assert (eqv? (cdr a) #f)))
(assert (= (barplusminus 2 3) 4))

; should fail:
; (define foofoobar (parallel-combine foo foo bar))

; (define foofoomult (compose2 foo foo-mult))
((compose2 bar foo-mult) 2)


