#lang racket
; ---------------------------------------------------------------------------------------------------
(provide matrix
         matrix-ref
         list->matrix
         matrix-update
         matrix-*)

; ---------------------------------------------------------------------------------------------------
(require rackunit)
; ---------------------------------------------------------------------------------------------------
; BODY

;; get the dims of an input matrix.
;; matrix -> (list of dim)
(define (get-dims m)
  (cond [(not (vector? m)) '()]
        [else (cons (vector-length m) (get-dims (vector-ref m 0)))]))

;(get-dims (vector (vector 0 0 0) (vector 0 0 0)))


;(struct Matrix (name dims data) #:transparent)

;; returns a struct matrix with a name and data.
;; string, number ... -> matrix.
(define (matrix x)
  (cond [(equal? 1 (car x)) (matrix (cdr x))] 
        [(equal? 1 (length x))  (build-vector (car x) (lambda (x) 0))]
        [else
          (build-vector (car x) (lambda (n) (matrix (cdr x))))])) 


;(define m (matrix  (list 3 3 3)))
;(displayln m)

;; acceses the element/matrix at (pos1 pos2 ...) as dimensions.
;; matrix, number, number ... -> matrix.
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
;; matrix, dims, pair, list => matrix. 
(define (get-vector-in-range m dims range xpos)
  (define next-slice (cdr xpos))
  (define start (car range))
  (define end (cdr range))
  (define r-dim (cdr dims))
  (list->vector (for/list ([i (in-range start end)])
                  (matrix-ref  (vector-ref m i) (cdr xpos)))))


;(define n (matrix  (list 3 3)))
;(Matrix-data n)
;(define ref (matrix-ref n (list "A" 0)))
;(displayln ref)
;(define ref1 (matrix-ref n (list (cons 1 2) 0)))
;(displayln ref1)


;; creating a matrix from a list.
;; list, name -> matrix
(define (list->matrix lt dims)
  (let* ([c-dim (if (empty? dims) 0 (car dims))] [r-dim (if (empty? dims) 0 (cdr dims))])
    (cond [(equal? (length dims) 1) (list->vector lt)]
          [else (list->vector (for/list ([i (in-range c-dim)])
                                (list->matrix (list-ref lt i) r-dim)))])))

;(list->matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 3 3)) 
;(list->matrix (list (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list (list 1 2 3) (list 4 5 6) (list 7 8 9))) (list 2 3 3)) 


;; updating the same matrix at a certain position. 
;; matrix, pos -> matrix.
(define (matrix-update m posn element)
  (let* ([c-pos (if (empty? posn) posn (car posn))] [r-pos (if (empty? posn) posn (cdr posn))])
    (cond [(empty? r-pos)  
           (vector-set! m c-pos element)]
          [else 
            (matrix-update (vector-ref m c-pos) r-pos element)])))



;(matrix-update n (list 1 2) 3)
;n


;; return the dot product of two 1d matrices.
;; matrix, matrix -> vector/Od 
(define (dot-product m1 m2)
  (define m1-dims (get-dims m1))
  (define m2-dims (get-dims m2))
  (define ln-m1 (car m1-dims))
  (define ln-m2 (car m2-dims))
  (for/fold ([result 0])
    ([i (vector->list m1)]
     [j (vector->list m2)])
    (+ result (* i j))))

;; START
#|(cond [(equal? ln-m1 ln-m2) (dot m1-data m2-data)]
        [else (displayln "they have to be of same length")])|#;)

;(dot-product (matrix (list 5)) (matrix (list 4)))


;; returning transpose of a 2d matrix.
;; matrix -> transposed matrix. 
(define (2d-transpose m)
  (define dims (get-dims m))
  (build-vector (car dims) (lambda (n)
                             (matrix-ref m (list "A" n)))))
#|(cond [(equal? (length dims) 2) (Matrix "res" (list (cdr dims) (car dims)) (transpose data))]
        [else "it has to be a 2d matrix"])|#

;(define ex (list->matrix (list (list 1 2 3) (list 1 2 3) (list 1 2 3)) (list 3 3)))
;(define ex1 (list->matrix (list (list 1 2 3) (list 1 2 3)) (list 2 3)))
;(2d-transpose ex1)


;; multiplying two 2d matrices.
;; matrix, matrix -> vector.
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

;(define ex2 (list->matrix (list (list 1 1 1) (list 1 2 3) (list 1 2 1)) (list 3 3)))
;(matrix-* ex  ex2)


; ---------------------------------------------------------------------------------------------------
;; TESTING
#|(define tests
    (test-suite
      ""
      (test-begin
        "
        (check-equal? ))))|#
