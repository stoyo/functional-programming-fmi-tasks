#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))

(define (succ n)
  (lambda (f v)
    (f (n f v))))

(define (from-numeral n)
  (n (lambda (val) (+ 1 val)) 0))

(define (to-numeral n)
  (define (repeated n)
    (lambda (f v)
      (if (= n 0)
          v
          (f ((repeated (- n 1)) f v)))))
  (repeated n))

(define (plus n m)
  (lambda (f v)
    (n f (m f v))))

(define (mult n m)
    (n (lambda (numeral) (plus m numeral)) zero))

(define (pred n) void)
