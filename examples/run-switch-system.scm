(define-module (examples run-switch-system)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet runtime)
  #:use-module (cpnet pure)
  #:use-module (cpnet sexp-loader))

;; The interface category must be defined for the loader to find it.
(define-category TestSwitchInterface
  (objects
   (instance input-key Data 'bar)
   (instance case-data Data '() 'Replace)
   (instance default-data Data 'default)
   (instance output Data #f 'Replace)))

(register-builder (make-category-builder 'TestSwitchInterface TestSwitchInterface '()))

(let ((system (load-system-from-file "examples/test-switch-system.cpnet")))
  (parameterize ((current-system system))
    (show-state "--- Test SEXP SWITCH: Before (key=bar) ---")
    (let ((case-list '((foo . 1) (bar . 2) (baz . 3))))
      (trigger (get-cell 'TestSwitchInterface 'case-data) case-list)
      (trigger (get-cell 'TestSwitchInterface 'input-key) 'bar))
    (run)
    (show-state "Result should be 2")
    
    (clear-effect-log!)
    (trigger (get-cell 'TestSwitchInterface 'input-key) 'other)
    (run)
    (show-state "Result should be 'default'")))
