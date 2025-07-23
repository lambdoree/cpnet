(use-modules (cpnet core)
             (cpnet runtime)
             (cpnet category)
             (cpnet lambda))

(display "--- CP-Net Lambda Calculus Demo: Equality Axiom ---\n")

(define (run-eq-test val1 val2 expected)
  (let* ((a (make-cell 'a val1))
         (b (make-cell 'b val2))
         (c (make-cell 'c #f))
         (p-eq (p-equal? 'eq-check a b c))
         (C (make-cpnet-category (list a b c) p-eq)))
    (runtime-settle! C)
    (let ((result-val (cell-value c)))
      (format #t "equal? ~a ~a -> ~a (Expected: ~a) -> ~a\n"
              val1 val2 result-val expected
              (if (eq? result-val expected) "Success" "Failure")))))

(run-eq-test 5 5 'true)
(run-eq-test 5 6 'false)
(run-eq-test "hello" "hello" 'true)
(run-eq-test "hello" "world" 'false)
(run-eq-test '(a b) '(a b) 'true)
(run-eq-test '(a b) '(a c) 'false)
