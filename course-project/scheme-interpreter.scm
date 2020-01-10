(define (get-variable-value variable environments)
  (if (null? environments)
      (display (string-append "\n" (symbol->string variable) ": undefined;"))
      (let ((environment-lookup-result (assoc variable (car environments))))
        (if environment-lookup-result
            (cadr environment-lookup-result)
            (get-variable-value variable (cdr environments))))))

(define (handle-define expression environments)
  (add-in-environment! (get-define-symbol expression)
                       (i-eval (get-define-expression expression) environments)
                       environments)
  'processed)

(define (add-in-environment! symbol expression environments)
  (let ((first-environment (car environments))) ; ((env E3) -> (env E2) -> (env E1) -> (env E1)) - env E3 in this case since it is the most nested
    (define (help environment-pairs)
      (cond ((null? environment-pairs) ; add upfront
             (set-cdr! first-environment (cons (car first-environment) (cdr first-environment))) ; (car first-environment) assumes at least one binding exists in environment - that is the case in default-environment
             (set-car! first-environment (cons symbol (list expression))))
            ((equal? symbol (caar environment-pairs)) ; update
             (set-cdr! (car environment-pairs) (list expression)))
            (else ; keep looking
             (help (cdr environment-pairs)))))
    (help first-environment)))

(define (add-environment environment environments)
  (cons environment environments))

(define (get-define-symbol expression)
  (let ((define-head (cadr expression)))
    (if (pair? define-head) ; check whether expression is (define (head args) body ...+)
        (car define-head)
        define-head))) ; else assume (define head <expression>)

(define (get-define-expression expression)
  (let ((define-head (cadr expression)))
    (if (pair? define-head)
        (let ((define-head-args (cdadr expression))
              (define-head-body (cddr expression)))
          (cons 'lambda (cons define-head-args define-head-body)))
        (caddr expression))))

(define (handle-lambda expression environments)
  (let ((lambda-params (cadr expression))
        (lambda-body (cddr expression)))
    (list 'function lambda-params lambda-body environments)))

(define (handle-if expression environments)
  (let ((test-expression (cadr expression))
        (then-expression (caddr expression))
        (else-expression-container (cdddr expression)))
    (if (i-eval test-expression environments) ; takes advantage of the fact that host language is the same as the language being implemented
                                              ; IMO it would make sense to abstract the "is true check" (everything that is not explicitly false is true)
        (i-eval then-expression environments)
        (if (not (null? else-expression-container)) ; check whether expression is not only (if <test-expression> <then-expr>)
            (i-eval (car else-expression-container) environments)))))

(define (filter p? xs)
  (cond ((null? xs) '())
        ((p? (car xs)) (cons (car xs) (filter p? (cdr xs))))
        (else (filter p? (cdr xs)))))

; all valid
; (cond)
; (cond (else ...+))
; (cond (<expression> ...+) [...])
; (cond (<expression> ...+) [...] (else ...+))
(define (handle-cond expression environments)
  (define (evaluate expressions)
    (if (not (null? expressions))
        (let ((current-expression (car expressions)))
          (if (or (equal? (car current-expression) 'else) (i-eval (car current-expression) environments)) ; handle-if comments apply here, also
              (evaluate-expressions (cdr current-expression) environments)
              (evaluate (cdr expressions))))))
  
  (define (valid-else-clause? expression)
    (let ((else-clauses (filter (lambda (sub-expression) (equal? (car sub-expression) 'else)) (cdr expression))))
      (or (= (length else-clauses) 0)
          (and (= (length else-clauses) 1)
               (equal? (car (list-ref (cdr expression) (- (length (cdr expression)) 1))) 'else)))))
  
  (if (not (valid-else-clause? expression))
      (display "\ncond: bad syntax for `else` clause")
      (evaluate (cdr expression))))

(define (evaluate-expressions expressions environments)
  (cond ((null? expressions))
        ((= (length expressions) 1) (i-eval (car expressions) environments)) ; makes sure last expression evaluation is returned
        (else (i-eval (car expressions) environments)
              (evaluate-expressions (cdr expressions) environments))))

(define (handle-function expression environments)
  (let ((function (car expression)) ; can be lambda or function name
        (function-arguments (cdr expression)))
    (i-apply (i-eval function environments)
             (map (lambda (function-argument) (i-eval function-argument environments)) function-arguments))))

(define (i-eval expression environments)
  (cond ((or (boolean? expression) (number? expression) (char? expression) (string? expression)) expression) ; evaluates to self
        ((symbol? expression) (get-variable-value expression environments)) ; is "variable"
        ((and (pair? expression) (equal? (car expression) 'define)) (handle-define expression environments)) ; is define expression
        ((and (pair? expression) (equal? (car expression) 'lambda)) (handle-lambda expression environments)) ; is lambda expression
        ((and (pair? expression) (equal? (car expression) 'if)) (handle-if expression environments)) ; is if expression
        ((and (pair? expression) (equal? (car expression) 'cond)) (handle-cond expression environments)) ; is cond expression
        ((and (pair? expression) (equal? (car expression) 'quote)) (cadr expression)) ; is quote
                                                                                      ; interesting thing: 'datanum arrives as (quote datanum) to the evaluator, also
                                                                                      ; I believe this is because I use same host language as the one I am implementing
        ((pair? expression) (handle-function expression environments)) ; is function invocation
        (else (display "\ni-eval: expression type not recognized in: ")
              (display expression))))

; valid function can be either
;   - (list 'function params body environments)
;   - (list 'provided the-function-name)
(define (i-apply function arguments)
  (cond ((and (pair? function) (equal? (car function) 'provided))
         (apply (cadr function) arguments)) ; use host language apply function to be able to invoke functions which receive a variable number of arguments + cons, car, cdr, null?
        ((and (pair? function) (equal? (car function) 'function))
         (let ((function-params (cadr function))
               (function-body (caddr function))
               (function-environments (cadddr function)))
           (if (not (= (length arguments) (length function-params)))
               (display (string-append "\ni-apply: expected " (number->string (length function-params)) " argument[s], got " (number->string (length arguments))))
               (evaluate-expressions function-body
                                     (add-environment (map list function-params arguments) function-environments)))))
        (else (display "\ni-apply: '")
              (display function)
              (display "' wasn't recognized"))))

(define provided-functions
  (list (list '+ (list 'provided +))
        (list '- (list 'provided -))
        (list '* (list 'provided *))
        (list '/ (list 'provided /))
        (list '> (list 'provided >))
        (list '>= (list 'provided >=))
        (list '< (list 'provided <))
        (list '<= (list 'provided <=))
        (list '= (list 'provided =))
        (list 'list (list 'provided list))
        (list 'cons (list 'provided cons))
        (list 'car (list 'provided car))
        (list 'cdr (list 'provided cdr))
        (list 'null? (list 'provided null?))
        (list 'char-downcase (list 'provided char-downcase)))) ; added only for test17

(define default-environment
  (add-environment provided-functions '()))

; the 2 functions below are intended to be "consumer" facing, the ones above may depend on assumed invariant of some sort
(define (interpret expressions)
  (evaluate-expressions expressions default-environment))

(define (read-eval-print)
  (newline)
  (let* ((user-input (read))
         (evaluated (i-eval user-input default-environment)))
    (display evaluated)
    (read-eval-print)))

; uncomment below to start read-eval-print mode
; (read-eval-print)

; <!! TESTS !!>

(define (print-success test)
  (display (string-append test " - OK"))
  (newline))

(define (print-failure test expected result)
  (display (string-append test " - ERROR: " (string-append "expected: '" expected "', got: '" result "'")))
  (newline))

(let ((test "test1")
      (expected 3)
      (result (interpret '((define (length xs)
                             (if (null? xs)
                                 0
                                 (+ 1 (length (cdr xs)))))
                           (length (list 1 2 3))))))
  (if (= expected result)
      (print-success test)
      (print-failure test (number->string expected) (number->string result))))

(let ((test "test2")
      (expected 7)
      (result (interpret '((define (f x) (+ x 2))
                           (define x 5)
                           ((if (null? '()) f g) x)))))
  (if (= expected result)
      (print-success test)
      (print-failure test (number->string expected) (number->string result))))

(let ((test "test3")
      (expected 26)
      (result (interpret '((define (add-n n) (lambda (m) (+ n m)))
                           (define add-5 (add-n 5))
                           (define (f x y) (+ x y ((add-n x) 10)))
                           (f 5 6)))))
  (if (= expected result)
      (print-success test)
      (print-failure test (number->string expected) (number->string result))))

(let ((test "test4")
      (expected 36)
      (result (interpret '((define (f a)
                             (define (g b) (* a b))
                             (g (g (+ a 1))))
                           (f 3)))))
  (if (= expected result)
      (print-success test)
      (print-failure test (number->string expected) (number->string result))))

(let ((test "test5")
      (expected (list #\c #\b #\a))
      (result (interpret '((define (append xs ys)
                             (if (null? xs)
                                 ys
                                 (cons (car xs) (append (cdr xs) ys))))
                           (define (reverse xs)
                             (if (null? xs)
                                 (list)
                                 (append (reverse (cdr xs)) (list (car xs)))))
                           (reverse (list #\a #\b #\c))))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test (string-append "[" (list->string expected) "]") (string-append "[" (list->string result) "]"))))

(let ((test "test6")
      (expected #\a)
      (result (interpret '(#\a))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test (make-string 1 expected) (make-string 1 result))))

(let ((test "test7")
      (expected 5)
      (result (interpret '((define a 5) a))))
  (if (= expected result)
      (print-success test)
      (print-failure test (number->string expected) (number->string result))))

(let ((test "test8")
      (expected 'processed)
      (result (interpret '((define (grade-student)
                             (lambda (student-grade)
                               (cond ((>= student-grade 5.5) "Excellent")
                                     ((>= student-grade 4.5) "Very good")
                                     ((>= student-grade 3.5) "Good")
                                     ((>= student-grade 3) "Fine")
                                     (else "Failed"))))))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test (symbol->string expected) (symbol->string result))))

(let ((test "test9")
      (expected "Excellent")
      (result (interpret '(((grade-student) 6)))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test expected result)))

(let ((test "test10")
      (expected "Very good")
      (result (interpret '(((grade-student) 5)))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test expected result)))

(let ((test "test11")
      (expected "Good")
      (result (interpret '(((grade-student) 4)))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test expected result)))

(let ((test "test12")
      (expected "Fine")
      (result (interpret '(((grade-student) 3)))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test expected result)))

(let ((test "test13")
      (expected "Failed")
      (result (interpret '(((grade-student) 2)))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test expected result)))

(let ((test "test14")
      (expected (list #\a #\b #\c))
      (result (interpret '((quote (#\a #\b #\c))))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test (string-append "[" (list->string expected) "]") (string-append "[" (list->string result) "]"))))

(let ((test "test15")
      (expected 'foobar)
      (result (interpret '((quote foobar)))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test (symbol->string expected) (symbol->string result))))

(let ((test "test16")
      (expected '(define (1+ a) (+ a 1)))
      (result (interpret '((quote (define (1+ a) (+ a 1)))))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test (symbol->string expected) (symbol->string result))))

(let ((test "test17")
      (expected (list #\a #\b #\c #\d #\e))
      (result (interpret '((define (map f xs)
               (if (null? xs)
                   '()
                   (cons (f (car xs))
                         (map f (cdr xs)))))
             (map char-downcase (list #\A #\b #\C #\d #\E))))))
  (if (equal? expected result)
      (print-success test)
      (print-failure test (string-append "[" (list->string expected) "]") (string-append "[" (list->string result) "]"))))
