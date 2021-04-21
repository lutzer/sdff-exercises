(load "exercise211-predefined.scm")

;; Exercise 2.11: Implementing with conversions (p. 52)

(define unit-conversion-table (make-equal-hash-table))

(define (register-unit-conversion unit-from unit-to convert-proc)
  (hash-table-set! unit-conversion-table (cons unit-from unit-to) convert-proc))

(define (make-converter unit-from unit-to)
  (hash-table-ref/default unit-conversion-table (cons unit-from unit-to) #f))

; Test it
(register-unit-conversion
 'fahrenheit
 'kelvin
 (make-unit-conversion (lambda (x) (+ (/ (* (- x 32) 5) 9) 273.15))
                       (lambda (x) (+ (/ (* (- x 273.15) 9) 5) 32))))

((make-converter 'fahrenheit 'kelvin) 100)
(make-converter 'kelvin 'fahrenheit)

; (unit:* (fahrenheit -> kelvin) (kelvin -> celsius)) -> (fahrenheit -> celsius)
; m/s => km/h
; (m -> km) (s -> h), (unit:/ m->km s->h)

(define time-converter (make-unit-conversion (lambda (x) (* x 60))
                                             (lambda (x) (/ x 60))))
(register-unit-conversion 'hour 'minute time-converter)
(register-unit-conversion 'minute 'second time-converter)

((make-converter 'hour 'minute) 10)
((unit:invert (make-converter 'hour 'minute)) 10)

;; Test
(unit:* (make-converter 'hour 'minute) (make-converter 'minute 'second))
((unit:* (make-converter 'fahrenheit 'kelvin) (unit:invert (make-converter 'minute 'second))) 100)

;; register km -> m
(register-unit-conversion 'kilometer
                          'meter
                          (make-unit-conversion (lambda (x) (* x 1000))
                                                (lambda (x) (/ x 1000))))

(define (unit:/ unit-converter-1 unit-converter-2)
  (make-unit-conversion (unit:* unit-converter-1 (unit:invert unit-converter-2))
                        (unit:* (unit:invert unit-converter-1) unit-converter-2)))

; go from km/m -> m/s
; 100 m/s
; divide by 1000
; multiply by 60
((unit:/ (make-converter 'kilometer 'meter) (make-converter 'minute 'second)) 100)
((unit:invert (unit:/ (make-converter 'kilometer 'meter) (make-converter 'minute 'second))) 100)

;; implement exponential unit combination
(define (unit:exp unit-converter n)
  (if (= n 1)
      unit-converter
      (make-unit-conversion (unit:* unit-converter (unit:exp unit-converter (- n 1)))
                            (unit:invert (unit:* unit-converter
                                                 (unit:exp unit-converter (- n 1)))))))

;; km2 -> m2
((unit:exp (make-converter 'kilometer 'meter) 2) 1)
((unit:invert (unit:exp (make-converter 'kilometer 'meter) 2)) 1)

;; d - color conversion

; hsb
(define-record-type rgb-color
  (make-rgb-color r g b)
  rgb-color?
  (r rgb-color-r)
  (g rgb-color-g)
  (b rgb-color-b))

(define-record-type hsl-color
  (make-hsl-color h s l)
  hsl-color?
  (h hsl-color-h)
  (s hsl-color-s)
  (l hsl-color-l))

(define (interpolate hsl-color-1 hsl-color-2)
  (make-hsl-color (/ (+ (hsl-color-h hsl-color-1) (hsl-color-h hsl-color-2)) 2)
                  (/ (+ (hsl-color-s hsl-color-1) (hsl-color-s hsl-color-2)) 2)
                  (/ (+ (hsl-color-l hsl-color-1) (hsl-color-l hsl-color-2)) 2)))

(define (hsl->string hsl-color) (string-append (number->string (inexact->exact (* (hsl-color-h hsl-color) 360)))
                                               ", "
                                               (number->string (inexact->exact (* (hsl-color-s hsl-color) 100)))
                                               "%, "
                                               (number->string (inexact->exact (* (hsl-color-l hsl-color) 100)))
                                               "%"))

(hsl->string (interpolate (make-hsl-color 0.0 0.0 0.0) (make-hsl-color 1.0 1.0 1.0)))
(hsl->string (interpolate (make-hsl-color 0.16 1.0 0.5) (make-hsl-color 0.74 0.24 0.44)))

(define (hsl->rgb hsl)
  (assert (hsl-color? hsl))
  (let* ((h (hsl-color-h hsl))
         (s (hsl-color-s hsl))
         (l (hsl-color-l hsl))
         (c (* (- 1 (abs (- (* 2 l) 1))) s))
         (x (* c (- 1 (abs (- (modulo (/ h (/ 60 360)) 2) 1)))))
         (m (- l (/ c 2))))
    (cond ((<= h (/ 60 360)) (make-rgb-color c x 0.0))
          ((<= h (/ 120 360)) (make-rgb-color x c 0.0))
          ((<= h (/ 180 360)) (make-rgb-color 0.0 c x))
          ((<= h (/ 240 360)) (make-rgb-color 0.0 x c))
          ((<= h (/ 300 360)) (make-rgb-color x 0.0 c))
          (else (make-rgb-color c 0.0 x)))))


(define (rgb->hsl rgb)
    (assert (rgb-color? rgb))
    (let* ((r (rgb-color-r rgb)) 
        (g (rgb-color-g rgb)) 
        (b (rgb-color-b rgb)) 
        (cmax (max r g b))
        (cmin (min r g b))
        (delta (- cmax cmin)))
            (let* ((h (* (/ 60 360) (cond 
                ((= delta 0) 0)
                ((= cmax r) (* (/ (- g b) delta) 1))
                ((= cmax g) (+ (/ (- b r) delta) 2))
                ((= cmax b) (+ (/ (- r g) delta) 4)))))
                (l (/ (+ cmax cmin) 2))
                (s (if (= delta 0)
                  0
                  (/ delta (- 1 (abs (- (* 2 l) 1)))))))
              (make-hsl-color h l s))))

(hsl->string (rgb->hsl (make-rgb-color 0 0 0)))
(hsl->string (rgb->hsl (make-rgb-color 0 0 1)))
(hsl->string (rgb->hsl (make-rgb-color 0 1 0)))
(hsl->string (rgb->hsl (make-rgb-color 1 0 0)))

(register-unit-conversion
 'hsl
 'rgb
 (make-unit-conversion hsl->rgb rgb->hsl))

((make-converter 'hsl 'rgb) (make-hsl-color 0.5 0.5 0.5))
(rgb-color-r (hsl->rgb (make-hsl-color 0.5 0.5 0.5)))
