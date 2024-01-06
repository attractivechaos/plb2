(import (chezscheme))

(define (nonzero? x) (not (fxzero? x)))

(define (first-choice init n possible-choices)
  (let loop ([i init])
    (if (and (fx< i n)
             (nonzero? (fxlogand possible-choices
                                 (fxsll 1 i))))
      (loop (fx1+ i))
      i)))

(define (solve-queens n)
  (let ([a (make-fxvector n -1)]
        [l (make-fxvector n 0)]
        [c (make-fxvector n 0)]
        [r (make-fxvector n 0)]
        [y0 (fx1- (fxsll 1 n))])
    (let loop ([k 0]
               [m 0])
      (if (fx>= k 0)
        (let ([ak+1 (fx1+ (fxvector-ref a k))]
              [y (fxlogand y0 (fxlogior (fxvector-ref r k)
                                        (fxvector-ref l k)
                                        (fxvector-ref c k)))])
          (if (nonzero? (fxsrl (fxlogxor y y0) ak+1))
            (let* ([i (first-choice ak+1 n y)]
                   [z (fxsll 1 i)]
                   [k+1 (fx1+ k)])
              (if (fx< k (fx1- n))
                (begin
                  (fxvector-set! a k i)
                  (fxvector-set! l k+1
                                 (fxsll (fxlogior (fxvector-ref l k) z) 1))
                  (fxvector-set! c k+1
                                 (fxlogior (fxvector-ref c k) z))
                  (fxvector-set! r k+1
                                 (fxsrl (fxlogior (fxvector-ref r k) z) 1))
                  (loop k+1 m))
                (loop (fx1- k) (fx1+ m))))
            (begin
              (fxvector-set! a k -1)
              (loop (fx1- k) m))))
        m))))

(display (solve-queens 15))
