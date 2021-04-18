(define arity-table (make-key-weak-eqv-hash-table)) 
; restricts arity of a procedure
(define (restrict-arity proc arity) 
  (hash-table-set! arity-table proc arity)) 

; retrieves arity of a procedure
(define (get-arity proc) 
  (or (hash-table-ref/default arity-table proc #f) 
    (procedure-arity proc)))

; get the arity of two combined functions
(define (get-arity-combined proc1 proc2)
  (define (min-or-false x y)
    (if (and (number? x) (number? y))
      (min x y)
      (if (number? x)
        x
        (if (number? y)
          y
          #f))))
  (let* ( (a1 (get-arity proc1)) (a2 (get-arity proc2))
    (lower (max (car a1) (car a2))) 
    (upper (min-or-false (cdr a1) (cdr a2)))) 
      (cons lower upper)))

; (define (get-arity-min proc)
;   ((compose car get-arity) proc))

; (define (get-arity-max proc)
;   ((compose cdr get-arity) proc))

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
