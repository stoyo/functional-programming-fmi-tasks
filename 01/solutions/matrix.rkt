#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         enumerate-interval
         diag
         secondary-diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

(define (all? p? xs)
  (or (null? xs)
      (and (p? (car xs))
           (all? p? (cdr xs)))))

(define (any? p? xs)
  (not (all? (lambda (x) (not (p? x))) xs)))

(define (concat xss)
  (apply append xss))

(define (rows xss)
  xss)

(define (nth-column matrix n)
  (map (lambda (row)
         (list-ref row n))
       matrix))

(define (cols matrix)
  (define number-of-columns (length (car matrix)))

  (map (lambda (column-index)
         (nth-column matrix column-index))
       (enumerate-interval 0 (- number-of-columns 1))))

(define (matrix-ref xss i j)
  (list-ref (list-ref (rows xss) i) j))

(define (set xs i x)
  (cond ((null? xs) '())
        ((= i 0) (cons x (cdr xs)))
        (else (cons (car xs) (set (cdr xs) (- i 1) x)))))

(define (place xss i j x)
  (cond ((= i 0)
         (cons (set (car xss) j x) (cdr xss)))
        (else
         (cons (car xss) (place (cdr xss) (- i 1) j x)))))


(define (enumerate-interval a b)
    (if (> a b)
        '()
        (cons a (enumerate-interval (+ a 1) b))))

(define (diag xss)
  (let ((indexes (enumerate-interval 0 (- (length (car xss)) 1))))
    (map list-ref xss indexes)))

(define (secondary-diag xss)
  (let ((reversed-indexes (reverse (enumerate-interval 0 (- (length (car xss)) 1)))))
    (map list-ref xss reversed-indexes)))

(define (diags xss)
  (list (diag xss) (secondary-diag xss)))

(define (map-matrix f xss)
  (map
   (lambda (row)
     (map f row))
   xss))

(define (filter-matrix p? xss)
  (map
   (lambda (row) (filter p? row))
   xss))

(define (zip-with f xs ys)
  (if (or (null? xs)
          (null? ys))
      '()
      (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys)))))

(define (zip-matrix xss yss)
  (map (lambda (xs ys) (zip-with cons xs ys)) xss yss))