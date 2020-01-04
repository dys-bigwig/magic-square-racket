#lang racket
(require lens)

(define (show-nested-vector v)
  (for ([e (in-vector v)])
    (displayln e)))

(define (vector-ref* v y x)
  (lens-view (vector-ref-nested-lens y x) v))

(define (vector-ref-posn v p)
  (vector-ref* v (posn-y p)
                 (posn-x p)))

(define (vector-set* v y x a)
  (lens-set (vector-ref-nested-lens y x) v a))

(define (vector* y x)
  (vector->immutable-vector (make-vector y (vector->immutable-vector (make-vector x 0)))))

(struct posn (y x) #:transparent)

(define (magic-square o)
  (define (next-pos p v)
    (define up+right (posn (modulo (sub1 (posn-y p)) o)
                           (modulo (add1 (posn-x p)) o)))
    (define down (posn (modulo (add1 (posn-y p)) o)
                       (modulo (posn-x p) o)))
    (cond
      [(positive? (vector-ref-posn v up+right)) down] ;vector initialised to 0 - positive means posn has been taken/visited
      [else up+right]))
  (let loop ([v (vector* o o)]
             [p (posn 0 1)]
             [n 1])
    (cond
      [(> n (expt o 2)) v]
      [else (define new-v (vector-set* v (posn-y p) (posn-x p) n))
            (loop new-v
                  (next-pos p new-v)
                  (add1 n))])))

(show-nested-vector (magic-square 3))
