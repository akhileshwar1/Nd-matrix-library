#lang racket
; ---------------------------------------------------------------------------------------------------
(provide (contract-out 
           ;; returns a struct matrix with a name and data.
           [matrix (-> list? vector?)]
           ;; acceses the element/matrix at (pos1 pos2 ...) as dimensions.
           [matrix-ref (->* (vector? list?) vector?)]
           ;; creating a matrix from a list.
           [list->matrix (->* (list? list?) vector?)]
           ;; updating the same matrix at a certain position. 
           [matrix-update (->* (vector? list? any/c) void?)]
           ;; return the dot product of two 1d matrices.
           [matrix-* (->* (vector? vector?) vector?)]))

; ---------------------------------------------------------------------------------------------------
(require rackunit)
(require rackunit/text-ui)
; ---------------------------------------------------------------------------------------------------
; BODY

;(get-dims (vector (vector 0 0 0) (vector 0 0 0)))

;; get the dims of an input matrix.
(define (get-dims m)
  (cond [(not (vector? m)) '()]
        [else (cons (vector-length m) (get-dims (vector-ref m 0)))]))



;(struct Matrix (name dims data) #:transparent)

;(define m (matrix  (list 3 3 3)))

(define (matrix x)
  (cond [(equal? 1 (car x)) (matrix (cdr x))] 
        [(equal? 1 (length x))  (build-vector (car x) (lambda (x) 0))]
        [else
          (build-vector (car x) (lambda (n) (matrix (cdr x))))])) 



(define (matrix-ref m xpos)
  (define dims (get-dims m))
  (let* ([c-dim (if (empty? dims) 0 (car dims))] [r-dim (if (empty? dims) 0 (cdr dims))] 
                                                 [pos (if (empty? xpos) xpos (car xpos))])
    (cond [(or (empty? dims) (empty? pos)) m] 
          [(pair? pos) (let* ([start (car pos)] [end (cdr pos)])
                         (if (and (< start c-dim) (< end c-dim))
                           (get-vector-in-range m dims (cons start end) xpos)
                           (matrix-ref m '())))]

          [(equal? "A" pos) (get-vector-in-range m dims (cons 0 c-dim) xpos)] 

          [(> pos (sub1 (car dims))) (matrix-ref m '())]

          [else (matrix-ref (vector-ref m pos)  (cdr xpos))]))) 


;; splicing matrix for special cases like all or a range.
(define (get-vector-in-range m dims range xpos)
  (define next-slice (cdr xpos))
  (define start (car range))
  (define end (cdr range))
  (define r-dim (cdr dims))
  (list->vector (for/list ([i (in-range start end)])
                  (matrix-ref  (vector-ref m i) (cdr xpos)))))


;(list->matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 3 3)) 
;(list->matrix (list (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list (list 1 2 3) (list 4 5 6) (list 7 8 9))) (list 2 3 3)) 

(define (list->matrix lt dims)
  (let* ([c-dim (if (empty? dims) 0 (car dims))] [r-dim (if (empty? dims) 0 (cdr dims))])
    (cond [(equal? (length dims) 1) (list->vector lt)]
          [else (list->vector (for/list ([i (in-range c-dim)])
                                (list->matrix (list-ref lt i) r-dim)))])))

;(matrix-update n (list 1 2) 3)

(define (matrix-update m posn element)
  (let* ([c-pos (if (empty? posn) posn (car posn))] [r-pos (if (empty? posn) posn (cdr posn))])
    (cond [(empty? r-pos)  
           (vector-set! m c-pos element)]
          [else 
            (matrix-update (vector-ref m c-pos) r-pos element)])))




;(dot-product (matrix (list 5)) (matrix (list 4)))

(define (dot-product m1 m2)
  (define m1-dims (get-dims m1))
  (define m2-dims (get-dims m2))
  (define ln-m1 (car m1-dims))
  (define ln-m2 (car m2-dims))
  (for/fold ([result 0])
    ([i (vector->list m1)]
     [j (vector->list m2)])
    (+ result (* i j))))



;(define ex (list->matrix (list (list 1 2 3) (list 1 2 3) (list 1 2 3)) (list 3 3)))
;(define ex1 (list->matrix (list (list 1 2 3) (list 1 2 3)) (list 2 3)))
;(2d-transpose ex1)

;; returning transpose of a 2d matrix.
(define (2d-transpose m)
  (define dims (get-dims m))
  (build-vector (car dims) (lambda (n)
                             (matrix-ref m (list "A" n)))))


;(define ex2 (list->matrix (list (list 1 1 1) (list 1 2 3) (list 1 2 1)) (list 3 3)))
;(matrix-* ex  ex2)

;; multiplying two 2d matrices.
(define (matrix-* m1 m2)
  (define m2-new (2d-transpose m2))
  (define m1-ln (vector-length m1))
  (define m2-ln (vector-length m2-new))
  (build-vector m1-ln (lambda (n1)
                        (define ls1 (matrix-ref m1 (list n1)))
                        (build-vector m2-ln 
                                      (lambda (n2)
                                        (define ls2 (matrix-ref m2-new (list n2)))
                                        (dot-product ls1 ls2))))))



; ---------------------------------------------------------------------------------------------------
;; TESTING

(module+ test
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

(run-tests tests))

