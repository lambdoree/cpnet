(define-module (examples test-try-catch)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet pure)
  #:use-module (cpnet apply)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime))

;; A body that can fail
(define-category divide-body
  (objects (instance X Data #f) (instance Y Data #f))
  (morphisms
   ((morphism divide (X) -> Y)
    (lambda (vals _)
      (let ((n (car vals)))
        (if (and (number? n) (not (zero? n)))
            (cons (/ 100 n) '())
            (cons '(error . "division by zero") '())))))))
(register-builder (make-category-builder 'divide-body divide-body '((inputs (X)) (outputs (Y)))))

;; A body that succeeds
(define-category double-body
  (objects (instance X Data #f) (instance Y Data #f))
  (morphisms
   ((morphism double (X) -> Y)
    (lambda (vals _) (cons (* (car vals) 2) '())))))
(register-builder (make-category-builder 'double-body double-body '((inputs (X)) (outputs (Y)))))

;; A handler for errors
(define-category error-handler-body
  (objects (instance E Data #f) (instance R Data #f))
  (morphisms
   ((morphism handle (E) -> R)
    (effect-scope 'error-handler-body
      (lambda (vals _)
        (display (format #f "Caught error: ~a, returning -1\n" (car vals)))
        (cons -1 '()))))))
(register-builder (make-category-builder 'error-handler-body error-handler-body '((inputs (E)) (outputs (R)))))

(define-category TryCatchTestInterface
  (objects
   (instance input Data #f 'Replace)
   (instance result Data #f 'Replace)))

(define-cpnet-system TryCatchTestSystem
  (TryCatchTestInterface)
  (add-subsystem! (current-system) (try-catch-system 'try-catch-impl))
  (wire (get-cell 'TryCatchTestInterface 'input) (get-cell 'try-catch-impl 'try-catch-interface 'body-in))
  (wire (get-cell 'try-catch-impl 'try-catch-interface 'result) (get-cell 'TryCatchTestInterface 'result))
  (trigger (get-cell 'try-catch-impl 'try-catch-interface 'catch-body) (get-builder 'error-handler-body)))

;; --- Scenario 1: Success ---
(parameterize ((current-system (TryCatchTestSystem)))
  (show-state "--- Test try-catch (Success): Before ---")
  (trigger (get-cell 'try-catch-impl 'try-catch-interface 'try-body) (get-builder 'double-body))
  (trigger (get-cell 'TryCatchTestInterface 'input) 10)
  (run)
  (show-state "--- Test try-catch (Success): After (should be 20) ---"))

;; --- Scenario 2: Failure ---
(parameterize ((current-system (TryCatchTestSystem)))
  (show-state "--- Test try-catch (Failure): Before ---")
  (trigger (get-cell 'try-catch-impl 'try-catch-interface 'try-body) (get-builder 'divide-body))
  (trigger (get-cell 'TryCatchTestInterface 'input) 0)
  (run)
  (show-state "--- Test try-catch (Failure): After (should be -1) ---"))
