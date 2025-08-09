(define-module (examples test-determinism)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet apply)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime))

(display "\n--- [Testing Determinism] ---\n")

;; Use the same system definition as test-apply-arity for consistency
(define-category const-42
  (objects
   (instance out Data #f 'Replace))
  (morphisms
   ((morphism make-const () -> out)
    (lambda (vals _) (cons 42 '())))))

(register-builder (make-category-builder
                   'const-42
                   const-42
                   '((inputs ()) (outputs (out)))))

(define-category DeterminismTestInterface
  (objects
   (instance result Data #f 'Replace)))

(define-cpnet-system DeterminismTestSystem
  (DeterminismTestInterface)
  (add-subsystem! (current-system) (apply-gate 'source-impl)))

;; Function to run the scenario and return the final value of the result cell
(define (run-scenario)
  (let ((sys (DeterminismTestSystem)))
    (parameterize ((current-system sys)
                   (*deterministic-execution* #f))
      (trigger (get-cell 'source-impl 'apply-interface 'code) (get-builder 'const-42))
      (trigger (get-cell 'source-impl 'apply-interface 'args) '())
      (trigger (get-cell 'source-impl 'apply-interface 'results) (list (get-cell 'DeterminismTestInterface 'result)))
      (run)
      (get-cell-value 'DeterminismTestInterface 'result))))

;; Run the scenario multiple times and collect results
(define (run-determinism-test num-runs)
  (format #t "Running determinism test ~a times...\n" num-runs)
  (let loop ((n num-runs) (results '()))
    (if (zero? n)
        results
        (loop (- n 1) (cons (run-scenario) results)))))

(let* ((num-runs 10)
       (results (run-determinism-test num-runs))
       (first-result (car results))
       (all-same? (every (lambda (res) (equal? res first-result)) (cdr results))))
  (format #t "All ~a runs finished.\n" num-runs)
  (format #t "Results: ~s\n" results)
  (if all-same?
      (format #t "OK: All results are identical (~s).\n" first-result)
      (format (current-error-port) "FAIL: Results are not deterministic!\n")))
