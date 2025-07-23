(use-modules (cpnet core)
             (cpnet runtime)
             (cpnet category)
             (cpnet lambda))

(define n         (make-cell 'n       #f))
(define acc       (make-cell 'acc     1))
(define n-next    (make-cell 'n-next  #f))
(define acc-next  (make-cell 'acc-next #f))
(define is-zero   (make-cell 'is-zero #f))
(define result    (make-cell 'result  #f))

(define p-decr    (p-subtract-1 'decr-n n n-next))
(define p-mult    (p-multiply   'mult-acc n acc acc-next))

(define p-test    (p-is-zero 'is-zero n is-zero))

(define FACT-Net
  (make-fixpoint-net
   (list acc n)
   (list acc-next n-next)
   (cons p-decr p-mult)
   p-test
   is-zero
   (list result)))

(cell-set-value! acc 1)
(cell-set-value! n 5)
(runtime-settle! FACT-Net)
(display (format #f "5! = ~a\n" (cell-value result)))
