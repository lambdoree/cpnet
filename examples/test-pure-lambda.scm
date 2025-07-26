(define-module (test-pure-lambda)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet core)
  #:use-module (cpnet runtime)
  #:use-module (cpnet system)
  #:use-module (cpnet functor)
  #:use-module (cpnet pure-lambda))

(define (test-combinator-K)
  (display "\n--- [TEST] K Combinator: (K 5) 10 -> 5 ---\n")
  (let* ((system (make-system))
         (k-cells (make-pure-combinator-K system "k"))
         (c-trigger (make-cell 'c-trigger #t))
         (c-5 (make-cell 'c-5 #f))
         (c-10 (make-cell 'c-10 #f))
         (c-result (make-cell 'c-result #f))
         (c-k-of-5 (make-cell 'c-k-of-5 #f)))

    (system-add-objects system (list c-trigger c-5 c-10 c-result c-k-of-5))

    (let ((connection-propagators
           (list
            (make-connector-propagator 'p-conn-k-in c-5 (hash-ref k-cells 'x-in))
            (make-connector-propagator 'p-conn-k-out (hash-ref k-cells 'fn-out) c-k-of-5)
            (p-pure-apply 'p-apply-k5-10 c-k-of-5 c-10 c-result)
            (p-const 'p-inject-5 c-trigger c-5 5)
            (p-const 'p-inject-10 c-trigger c-10 10))))
      (system-add-morphisms system connection-propagators))

    (runtime-settle! system)
    (runtime-show-state system "Test K Done")))

(define (test-combinator-I)
  (display "\n--- [TEST] I Combinator: I = S K K; I 42 -> 42 ---\n")
  (let* ((system (make-system))
         (k1-cells (make-pure-combinator-K system "k1"))
         (k2-cells (make-pure-combinator-K system "k2"))
         (s1-cells (make-pure-combinator-S system "s1"))
         (c-trigger (make-cell 'c-trigger #t))
         (c-input (make-cell 'c-input #f))
         (c-final-result (make-cell 'c-final-result #f))
         (c-k1-rep (make-cell 'c-k1-rep #f))
         (c-k2-rep (make-cell 'c-k2-rep #f))
         (c-i-rep (make-cell 'c-i-rep #f)))

    (system-add-objects system (list c-trigger c-input c-final-result c-k1-rep c-k2-rep c-i-rep))

    (let ((connection-propagators
           (list
            (p-const 'p-inject-k1-rep c-trigger c-k1-rep
                     (make-pfn (hash-ref k1-cells 'x-in)
                               (hash-ref k1-cells 'fn-out)))
            (p-const 'p-inject-k2-rep c-trigger c-k2-rep
                     (make-pfn (hash-ref k2-cells 'x-in)
                               (hash-ref k2-cells 'fn-out)))
            (p-const 'p-inject-input c-trigger c-input 42)

            (make-connector-propagator 'p-conn-s-x-in c-k1-rep (hash-ref s1-cells 'x-in))
            (make-connector-propagator 'p-conn-s-y-in c-k2-rep (hash-ref s1-cells 'y-in))
            (make-connector-propagator 'p-conn-s-fn-out (hash-ref s1-cells 'fn-out) c-i-rep)

            (p-pure-apply 'p-apply-i-42
                          c-i-rep
                          c-input
                          c-final-result)
            )))
      (system-add-morphisms system connection-propagators))

    (runtime-settle! system)
    (runtime-show-state system "Test I Done")))

(test-combinator-K)
(test-combinator-I)
