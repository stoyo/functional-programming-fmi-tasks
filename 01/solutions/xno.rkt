#lang racket

(provide winner
         play)

(require "matrix.rkt")

(define (all-signs-equal? xs)
  (all? (lambda (x)
          (equal? x (car xs)))
        xs))

(define (get-winner-sign b)
  (car (apply append (filter (lambda (row) (and (all-signs-equal? row) (not (member #f row)))) b))))

(define (winner b)
  (cond ((any? (lambda (row) (and (all-signs-equal? row) (not (member #f row)))) (rows b)) (get-winner-sign (rows b)))
        ((any? (lambda (col) (and (all-signs-equal? col) (not (member #f col)))) (cols b)) (get-winner-sign (cols b)))
        ((and (not (equal? (cadr (diag b)) #f)) (or (all-signs-equal? (diag b)) (all-signs-equal? (secondary-diag b)))) (cadr (diag b)))
        ((all? (lambda (row) (not (member #f row))) (rows b)) "D")
        (else #f)))

(define (empty-spots xss)
  (map (lambda (spot) (cons (quotient spot 3) (remainder spot 3)))
       (filter integer?
               (map (lambda (sign i)
                      (if sign sign i))
                    (apply append xss)
                    (enumerate-interval 0 8)))))

(define (play curr-board curr-sign)
  (let* ((curr-player (if curr-sign "X" "O"))
         (results (map (lambda (move)
                         (list (minimax curr-board move curr-player #t) ; curr-sign always denotes maximiser
                               move))
                       (empty-spots curr-board))))
    (cadr (foldl (lambda (mv1 mv2) (if (> (car mv1) (car mv2)) mv1 mv2))
                 (car results)
                 (cdr results)))))

(define (minimax curr-board move curr-player is-max)
  (let* ((moved-board (place curr-board (car move) (cdr move) curr-player))
         (winner-sign (winner moved-board)))
    (if winner-sign
        (if (equal? winner-sign "D")
            0
            (if is-max
                1
                -1))
        (let* ((moves (empty-spots moved-board))
               (scores (map (lambda (move) (minimax moved-board move (if (equal? curr-player "X") "O" "X") (not is-max))) moves)))
          (if is-max
              (foldl min (car scores) (cdr scores))
              (foldl max (car scores) (cdr scores)))))))