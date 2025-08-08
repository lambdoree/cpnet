(define-module (examples test-oscillation)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime))

(define-category flip-flop-interface
  (objects
   (instance A Bool #f)
   (instance B Bool #f)))

(define-cpnet-system TestOscillation
  (flip-flop-interface)
  (propagator p-A->B
              (get-cell 'flip-flop-interface 'A)
              -> (get-cell 'flip-flop-interface 'B)
              (lambda (vals _) (cons (not (car vals)) '())))
  (propagator p-B->A
              (get-cell 'flip-flop-interface 'B)
              -> (get-cell 'flip-flop-interface 'A)
              (lambda (vals _) (cons (not (car vals)) '()))))

(parameterize ((current-system (TestOscillation)))
  (show-state "--- Test Oscillation: Before ---")
  (run)
  (show-state "--- Test Oscillation: After ---"))
