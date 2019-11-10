#lang racket

(provide from-k-ary
         to-k-ary)

(define (from-k-ary n k)
  (define (digits-count n)
    (if (< n 10)
        1
        (+ 1 (digits-count (quotient n 10)))))
  (define (help num pow)
    (if (= (digits-count n) pow)
        (* (remainder num 10)
           (expt k pow))
        (+ (* (remainder num 10)
              (expt k pow))
           (help (quotient num 10) (+ 1 pow)))))
  (help n 0))

(define (to-k-ary n k)
  (define (help num pow)
    (if (= (quotient num k) 0)
        (* (remainder num k)
           (expt 10 pow))
        (+ (* (remainder num k)
              (expt 10 pow))
           (help (quotient num k) (+ 1 pow)))))
  (help n 0))
