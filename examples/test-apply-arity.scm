(define-module (examples test-apply-arity)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet apply)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime))

;; --- Test 1: 0 -> 1 (Source) ---
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

(define-cpnet-system SourceTestSystem
  (SourceTestInterface)
  (add-subsystem! (current-system) (apply-gate 'source-impl))
  (propagator p-setup
    (get-builder 'const-42)
    -> (list (get-cell 'source-impl 'apply-interface 'code)
             (get-cell 'source-impl 'apply-interface 'args)
             (get-cell 'source-impl 'apply-interface 'results))
    (lambda (vals srcs)
      (cons (list (car vals) '() (list (get-cell 'SourceTestInterface 'result))) '()))))

(parameterize ((current-system (SourceTestSystem)))
  (show-state "--- Arity Test (0->1): Before ---")
  (run)
  (show-state "--- Arity Test (0->1): After (should be 42) ---"))


;; --- Test 2: 1 -> 0 (Sink) ---
(define-category logger
  (objects
   (instance in Data #f 'Replace))
  (morphisms
   ((morphism log-it (in) -> ())
    (effect-scope 'logger
      (lambda (vals _)
        (cons *nothing*
              (list (make-effect 'display (format #f "Logger Sink: ~a\n" (car vals))))))))))

(register-builder (make-category-builder
                   'logger
                   logger
                   '((inputs (in)) (outputs ()))))

(define-category SinkTestInterface
  (objects
   (instance input Data #f 'Replace)))

(define-cpnet-system SinkTestSystem
  (SinkTestInterface)
  (add-subsystem! (current-system) (apply-gate 'sink-impl))
  (propagator p-setup
    (list (get-builder 'logger) (get-cell 'SinkTestInterface 'input))
    -> (list (get-cell 'sink-impl 'apply-interface 'code)
             (get-cell 'sink-impl 'apply-interface 'args)
             (get-cell 'sink-impl 'apply-interface 'results))
    (lambda (vals srcs)
      (cons (list (car vals) (list (cadr srcs)) '()) '()))))

(parameterize ((current-system (SinkTestSystem)))
  (show-state "--- Arity Test (1->0): Before ---")
  (trigger (get-cell 'SinkTestInterface 'input) "Hello Sink!")
  (run)
  (show-state "--- Arity Test (1->0): After (no changes, check console output) ---"))
