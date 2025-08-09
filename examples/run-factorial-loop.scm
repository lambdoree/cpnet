(define-module (examples run-factorial-loop)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet pure)
  #:use-module (cpnet system)
  #:use-module (cpnet sexp-loader)
  #:use-module (cpnet runtime))

;; The interface for the system defined in the .cpet file
(define-category TestLoopInterface
  (objects
   (instance final-val Data #f)))

(register-builder (make-category-builder 'TestLoopInterface TestLoopInterface '()))

;; The body for the loop, calculates one step of a factorial
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

;; Manually register the builder with a signature for the apply-gate.
(register-builder (make-category-builder
                   'factorial-body
                   factorial-body
                   '((inputs (X Y)) (outputs (Z)))))

;; A helper system to extract the second element from a pair
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

(register-builder (make-category-builder
                   'get-second-from-pair
                   get-second-from-pair
                   '((inputs (pair)) (outputs (second)))))

;; --- Scenario ---
(let ((system (load-system-from-file "examples/factorial-loop.cpnet")))
  (parameterize ((current-system system))
    (show-state "--- Factorial Loop (from .cpet): Before ---")
    (run)
    (show-state "Result should be 120")))
