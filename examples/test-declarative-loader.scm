(define-module (examples test-declarative-loader)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet apply)
  #:use-module (cpnet sexp-loader)
  #:use-module (cpnet runtime))

(display "\n--- [Testing Declarative S-exp Loader] ---\n")

;; The builders used by the .sexp file must be registered first.
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

(define-category SourceTestInterface
  (objects
   (instance result Data #f 'Replace)))

(register-builder (make-category-builder
                   'SourceTestInterface
                   SourceTestInterface
                   '()))


;; Now load the system from the S-expression file
(let ((sys (load-system-from-file "examples/declarative-source.sexp")))
  (parameterize ((current-system sys))
    (show-state "--- Declarative Loader: Before ---")
    (run)
    (show-state "--- Declarative Loader: After (should be 42) ---")
    (let ((final-val (get-cell-value 'SourceTestInterface 'result)))
      (if (equal? final-val 42)
          (format #t "OK: Final value is 42.\n")
          (format (current-error-port) "FAIL: Final value is ~s, expected 42.\n" final-val)))))
