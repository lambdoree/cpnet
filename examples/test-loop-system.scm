(define-module (examples test-loop-system)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet pure)
  #:use-module (cpnet system))

(define-object Data)

(define-category factorial-body
  (objects
   (instance X Data #f)
   (instance Y Data #f)
   (instance Z Data #f))
  (morphisms
   ((morphism step (X Y) -> Z)
    (lambda (vals _)
      (let* ((state (cadr vals))
             (n (if (list? state) (car state) -1))
             (acc (if (list? state) (cadr state) 0)))
        (if (and (integer? n) (> n 0))
            (cons (list (- n 1) (* n acc)) '())
            (cons state '())))))))

(define-category get-second-from-pair
  (objects
   (instance pair Data #f)
   (instance second Data #f))
  (morphisms
   ((morphism get-second (pair) -> second)
    (lambda (vals _)
      (let ((p (car vals)))
        (if (list? p)
            (cons (cadr p) '())
            (cons *nothing* '())))))))

(define-category TestLoopInterface
  (objects
   (instance final-val Data #f)))

(define-cpnet-system TestLoopSystem
  (TestLoopInterface)
  (add-subsystem! (current-system) (loop-system 'my-loop))
  (add-subsystem! (current-system) (get-second-from-pair 'extractor))

  (wire (get-cell 'my-loop 'loop-interface 'output) (get-cell 'extractor 'get-second-from-pair 'pair))

  (wire (get-cell 'extractor 'get-second-from-pair 'second) (get-cell 'TestLoopInterface 'final-val)))

(parameterize ((current-system (TestLoopSystem)))
  (show-state "--- Test LOOP: Before ---")
  (trigger (get-cell 'my-loop 'loop-interface 'body) (get-builder 'factorial-body))
  (trigger (get-cell 'my-loop 'loop-interface 'output) '(5 1))
  (run)
  (show-state "Result should be 120")
  (visualize "test-loop.dot"))
