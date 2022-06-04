#lang racket
; ---------------------------------------------------------------------------------------------------
(require rackunit)
(require rackunit/text-ui)
(require nd-matrix)
; ---------------------------------------------------------------------------------------------------
;; TESTING
(define tests
  (test-suite
    "matrix tests"
    (test-case 
      "cases for matrix function"
      (test-begin
        (check-equal? (matrix '(3 3)) (vector (vector 0 0 0) (vector 0 0 0) (vector 0 0 0)))
        (check-equal? (matrix "x" ) (vector 0 0 0))))


    (test-case 
      "cases for matrix-ref function"
      (test-begin
        (define m (matrix '(3 3)))
        (check-equal? (matrix-ref m (list "A" 0))  (vector 0 0 0))
        (check-equal? (matrix-ref m (list (cons 0 2) 0)) (vector 0 0))
        (check-equal? (matrix-ref m (list "A" "A"))  (vector (vector 0 0 0) (vector 0 0 0) 
                                                             (vector 0 0 0)))))

    (test-case 
      "cases for matrix-update function"
      (test-begin
        (define m (list->matrix  (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 3 3)))
        (matrix-update m (list 1 2) "a")
        (check-equal?  m (vector (vector 1 2 3) (vector 4 5  "a")
                                 (vector 7 8 9)))
        (matrix-update m (list 1) (vector "x" "x" "x"))
        (check-equal?  m (vector (vector 1 2 3)
                                 (vector "x" "x" "x")
                                 (vector 7 8 9)))))
    ))

(run-tests tests)
