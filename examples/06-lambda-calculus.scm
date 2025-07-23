(use-modules (cpnet core)
             (cpnet runtime)
             (cpnet category)
             (cpnet lambda))

(display "--- CP-Net Lambda Calculus Demo: Church Booleans ---\n")

(define trigger (make-cell 'trigger #f))
(define control (make-cell 'control #f))
(define then-val (make-cell 'then-val 100))
(define else-val (make-cell 'else-val 200))
(define result (make-cell 'result #f))

(define p-true (p-const 'TRUE trigger control 'true))
(define p-false (p-const 'FALSE trigger control 'false))
(define p-if (p-gate 'IF control then-val else-val result))

(define C (make-cpnet-category
           (list trigger control then-val else-val result)
           p-if))


(define (run-test name condition-propagator expected-val)
  (format #t "\n--- Testing: ~a ---\n" name)
  (category-add-morphism C condition-propagator)
  
  (cell-set-value! trigger #t)
  (runtime-settle! C)
  
  (let ((final-result (cell-value result)))
    (format #t "Final Result: ~a (Expected: ~a)\n" final-result expected-val)
    (if (equal? final-result expected-val)
        (display "Success\n")
        (display "Failure\n")))

  (cell-set-value! trigger #f)
  (cell-set-value! control #f)
  (cell-set-value! result #f)
  (category-remove-morphism C condition-propagator))

(run-test "IF TRUE" p-true 100)
(run-test "IF FALSE" p-false 200)
