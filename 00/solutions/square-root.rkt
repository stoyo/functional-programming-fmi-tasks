#lang racket

(provide my-sqrt)

(define (my-sqrt x)
  (define (square-root guess x)
    (if (< (abs (- (* guess guess) x)) 0.001)
        guess
        (square-root (/ (+ guess (/ x guess)) 2) x)))
  (square-root 1.0 x))
