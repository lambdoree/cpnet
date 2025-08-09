(define-module (examples test-pure)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet pure)
  #:use-module (cpnet apply)
  #:use-module (cpnet system))

(define true-gate-builder  (get-builder 'true-gate))
(define false-gate-builder (get-builder 'false-gate))

(define-category ConditionalApplyInterface
  (objects
   (instance cond      Bool #t)
   (instance then_code category-builder true-gate-builder)
   (instance else_code category-builder false-gate-builder)
   (instance arg_a     Data 100)
   (instance arg_b     Data 200)
   (instance result    Data #f)))

(define-cpnet-system ConditionalApplySystem
  (ConditionalApplyInterface)
  (add-subsystem! (current-system) (if-system 'if-system))
  (add-subsystem! (current-system) (apply-gate 'apply-gate))

  (wire (get-cell 'ConditionalApplyInterface 'cond)
        (get-cell 'if-system 'if-interface 'condition))
  (wire (get-cell 'ConditionalApplyInterface 'then_code)
        (get-cell 'if-system 'if-interface 'then-val))
  (wire (get-cell 'ConditionalApplyInterface 'else_code)
        (get-cell 'if-system 'if-interface 'else-val))

  (wire (get-cell 'if-system 'if-interface 'result)
        (get-cell 'apply-gate 'apply-interface 'code))

  (wire (get-cell 'apply-gate 'apply-interface 'result)
        (get-cell 'ConditionalApplyInterface 'result)))

(parameterize ((current-system (ConditionalApplySystem)))
  (show-state "--- Conditional Apply: Before (cond=#t) ---")
  
  (trigger (get-cell 'ConditionalApplyInterface 'cond) #t)
  (trigger (get-cell 'apply-gate 'apply-interface 'args)
           (list (get-cell 'ConditionalApplyInterface 'arg_a)
                 (get-cell 'ConditionalApplyInterface 'arg_b)))
  
  (display "Should apply 'true-gate' to (100, 200)\n")
  (run)
  (show-state "Result should be 100")
  (visualize "cond-apply-true.dot"))

(parameterize ((current-system (ConditionalApplySystem)))
  (show-state "--- Conditional Apply: Before (cond=#f) ---")

  (trigger (get-cell 'ConditionalApplyInterface 'cond) #f)
  (trigger (get-cell 'apply-gate 'apply-interface 'args)
           (list (get-cell 'ConditionalApplyInterface 'arg_a)
                 (get-cell 'ConditionalApplyInterface 'arg_b)))

  (display "Should apply 'false-gate' to (100, 200)\n")
  (run)
  (show-state "Result should be 200")
  (visualize "cond-apply-false.dot"))
