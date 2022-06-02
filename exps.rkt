#lang racket
; ---------------------------------------------------------------------------------------------------
#|(provide )|#

; ---------------------------------------------------------------------------------------------------
(require rackunit)
(require (for-syntax racket/list))
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))
(require racket/stxparam)

; ---------------------------------------------------------------------------------------------------
; BODY

(define-syntax (unlist stx)
  (syntax-parse stx
    [(_ lst) #`(+ #,@#'lst)]))
;(+ 5 a)
(define a 10)


(unlist (1 2 3))
(define (x lst)
  (apply + lst))
(build-vector 5 (lambda (n) (x '(2 3 4))))































; ---------------------------------------------------------------------------------------------------
;; TESTING
#|(define tests
  (test-suite
    ""
    (test-begin
      ""
      (check-equal? ))))|#
