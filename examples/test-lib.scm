(define-module (test-lib)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet core)
  #:use-module (cpnet runtime)
  #:use-module (cpnet system)
  #:use-module (cpnet lib))

(define (test-apply-gate)
  (display "\n--- [TEST] Apply Gate with Currying: ((+ 5) 10) -> 15 ---\n")
  (let* ((system (make-system))
         (apply1-cells (make-apply-gate system "apply1"))
         (apply2-cells (make-apply-gate system "apply2"))

         (c-trigger (make-cell 'c-trigger #t))
         (c-plus (make-cell 'c-plus #f))
         (c-5 (make-cell 'c-5 #f))
         (c-10 (make-cell 'c-10 #f))
         (c-plus-5 (make-cell 'c-plus-5 #f))
         (c-final-result (make-cell 'c-final-result #f)))

    (system-add-objects system
              (list c-trigger c-plus c-5 c-10 c-plus-5 c-final-result))

    (let ((propagators
           (list
            (p-const 'p-inject-plus c-trigger c-plus (lambda (x) (lambda (y) (+ x y))))
            (p-const 'p-inject-5 c-trigger c-5 5)
            (p-const 'p-inject-10 c-trigger c-10 10)

            (make-connector-propagator 'p-conn-plus c-plus (hash-ref apply1-cells 'fn-in))
            (make-connector-propagator 'p-conn-5 c-5 (hash-ref apply1-cells 'arg-in))
            (make-connector-propagator 'p-conn-plus5-out (hash-ref apply1-cells 'result-out) c-plus-5)

            (make-connector-propagator 'p-conn-plus5-in c-plus-5 (hash-ref apply2-cells 'fn-in))
            (make-connector-propagator 'p-conn-10 c-10 (hash-ref apply2-cells 'arg-in))
            (make-connector-propagator 'p-conn-final-result (hash-ref apply2-cells 'result-out) c-final-result)
            )))
      (system-add-morphisms system propagators))

    (runtime-settle! system)
    (runtime-show-state system "Test Apply Gate Done")))

(test-apply-gate)
