(import (chezscheme))  ; requires version >=10.0.0

; simple vector-backed, column-major matrix type
(define-record-type matrix
  (fields
    (immutable data)
    (immutable rows)
    (immutable columns))
  (protocol
    (lambda (new)
      (lambda (rows columns fill)
        (new (make-flvector (fx* rows columns) fill) rows columns)))))

(define (matrix-length a)
  (flvector-length (matrix-data a)))

(define (linear-index a i j)
  (fx+ i (fx* j (matrix-rows a))))

(define (subscripts a k)
  (let-values ([(j i) (fxdiv-and-mod k (matrix-rows a))])
    (values i j)))

(define (matrix-ref a i j)
  (flvector-ref (matrix-data a)
                (linear-index a i j)))

(define (matrix-set! a i j v)
  (flvector-set! (matrix-data a)
                 (linear-index a i j)
                 v))

(define (matrix-generate n)
  (let* ([m (fixnum->flonum n)]
         [a (make-matrix n n (fl/ 1.0 m m))]
         [p (matrix-length a)])
    (do ([k 0 (fx+ k 1)])
      ((fx>= k p) a)
      (let-values ([(i j) (subscripts a k)])
        (matrix-set! a i j (fl* (matrix-ref a i j)
                                (fixnum->flonum (fx- i j))
                                (fixnum->flonum (fx+ i j))))))))

(define (matrix-multiply a b)
  (let* ([n (matrix-rows a)]
         [p (matrix-columns a)]
         [m (matrix-columns b)]
         [c (make-matrix n m 0.0)])
    (do ([j 0 (fx+ j 1)])
      ((fx>= j m) c)
      (do ([k 0 (fx+ k 1)])
        ((fx>= k p))
        (do ([i 0 (fx+ i 1)])
          ((fx>= i n))
          (matrix-set! c i j (fl+ (matrix-ref c i j)
                                  (fl* (matrix-ref a i k)
                                       (matrix-ref b k j)))))))))

(let* ([n 1500]
       [i (fxsrl n 1)])
  (display (matrix-ref (matrix-multiply (matrix-generate n)
                                        (matrix-generate n))
                       i i)))
