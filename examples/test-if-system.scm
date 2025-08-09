(define-module (examples test-if-system)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet pure)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime))

(define-category TestIfInterface
  (objects
   (instance cond     Bool #f)
   (instance then_val Data 10)
   (instance else_val Data 20)
   (instance result   Data #f)))

(define-cpnet-system TestIfSystem
  (TestIfInterface)
  (add-subsystem! (current-system) (if-system 'my-if))
  (wire (get-cell 'TestIfInterface 'cond) (get-cell 'my-if 'if-interface 'condition))
  (wire (get-cell 'TestIfInterface 'then_val) (get-cell 'my-if 'if-interface 'then-val))
  (wire (get-cell 'TestIfInterface 'else_val) (get-cell 'my-if 'if-interface 'else-val))
  (wire (get-cell 'my-if 'if-interface 'result) (get-cell 'TestIfInterface 'result)))

;; Test case 1: condition is true
(parameterize ((current-system (TestIfSystem)))
  (show-state "--- Test IF (cond=#t): Before ---")
  (trigger (get-cell 'TestIfInterface 'cond) #t)
  (run)
  (show-state "Result should be 10")
  (visualize "test-if-true.dot"))

;; Test case 2: condition is false
(parameterize ((current-system (TestIfSystem)))
  (show-state "--- Test IF (cond=#f): Before ---")
  (trigger (get-cell 'TestIfInterface 'cond) #f)
  (run)
  (show-state "Result should be 20")
  (visualize "test-if-false.dot"))
