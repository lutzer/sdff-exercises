(define (compose f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args))
      f))
  the-composition)

(define (make-unit-conversion to from)
  (let ((to-op (make-apply-hook to #f))
        (from-op (make-apply-hook from #f)))
    (set-apply-hook-extra! to-op
                           (make-unit-conversion-record from-op))
    (set-apply-hook-extra! from-op
                           (make-unit-conversion-record to-op))
    to-op))

(define-record-type <unit-conversion-record>
    (make-unit-conversion-record inverse)
    unit-conversion-record?
  (inverse unit-conversion-record-inverse))

(define (unit-conversion? object)
  (and (apply-hook? object)
       (unit-conversion-record? (apply-hook-extra object))))
(register-predicate! unit-conversion? 'unit-conversion)

(define (unit:invert unit-conversion)
  (guarantee unit-conversion? unit-conversion)
  (unit-conversion-record-inverse (apply-hook-extra unit-conversion)))

(define (unit:* u1 u2)
  (make-unit-conversion (compose u2 u1)
                        (compose (unit:invert u1)
                                 (unit:invert u2))))

