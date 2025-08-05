(define-module (examples computational-core)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet conditional)
  #:use-module (cpnet iteration))

(define-object Bool)
(define-object Data)

;;;
;;; 1. Application-specific data
;;;
(define-category user-app-category
  (objects
   ;; Conditional part
   (instance branch-cond Bool #f)
   (instance then-val Data 10)
   (instance else-val Data 20)
   (instance branch-result Data #f)
   ;; Power of Two part
   (instance power-of-two-input Data 0)
   (instance power-of-two-start-val Data 1)
   (instance power-of-two-result Data #f)
   ;; Factorial part
   (instance factorial-input Data 0)
   (instance factorial-result Data #f)
   ;; GCD part
   (instance gcd-a-input Data 0)
   (instance gcd-b-input Data 0)
   (instance gcd-result Data #f)))
(define-cpnet-system user-app (user-app-category))

;;;
;;; 2. Define specific calculation systems by composing generic engines
;;;
(define-cpnet-system power-of-two-engine
  (iteration-category)
  ;; State Update Logic: (counter, accumulator) -> (counter-1, accumulator*2)
  (propagator p-next-a (list (get-cell 'iteration-category 'keep_running) (get-cell 'iteration-category 'current_A)) -> (get-cell 'iteration-category 'next_A)
              (lambda (vals _) (let ((running? (car vals)) (a (cadr vals)))
                                (if (and running? (number? a)) (cons (- a 1) '()) (cons *nothing* '())))))
  (propagator p-next-b (list (get-cell 'iteration-category 'keep_running) (get-cell 'iteration-category 'current_B)) -> (get-cell 'iteration-category 'next_B)
              (lambda (vals _) (let ((running? (car vals)) (b (cadr vals)))
                                (if (and running? (number? b)) (cons (* b 2) '()) (cons *nothing* '())))))
  ;; Termination Condition: counter > 0
  (propagator p-cond (get-cell 'iteration-category 'current_A) -> (get-cell 'iteration-category 'keep_running)
              (lambda (val _) (let ((a val)) (cons (and (number? a) (> a 0)) '())))))

(define-cpnet-system factorial-engine
  (iteration-category)
  ;; State Update Logic: (n, fact) -> (n-1, fact*n)
  (propagator p-next-a (list (get-cell 'iteration-category 'keep_running) (get-cell 'iteration-category 'current_A)) -> (get-cell 'iteration-category 'next_A)
              (lambda (vals _) (let ((running? (car vals)) (a (cadr vals)))
                                (if (and running? (number? a)) (cons (- a 1) '()) (cons *nothing* '())))))
  (propagator p-next-b (list (get-cell 'iteration-category 'keep_running) (get-cell 'iteration-category 'current_A) (get-cell 'iteration-category 'current_B)) -> (get-cell 'iteration-category 'next_B)
              (lambda (vals _) (let ((running? (car vals)) (a (cadr vals)) (b (caddr vals)))
                                (if (and running? (number? a) (number? b)) (cons (* a b) '()) (cons *nothing* '())))))
  ;; Termination Condition: n > 0
  (propagator p-cond (get-cell 'iteration-category 'current_A) -> (get-cell 'iteration-category 'keep_running)
              (lambda (val _) (let ((a val)) (cons (and (number? a) (> a 0)) '())))))

(define-cpnet-system gcd-engine
  (iteration-category)
  ;; State Update Logic: (a, b) -> (b, a mod b)
  (propagator p-next-a (list (get-cell 'iteration-category 'keep_running) (get-cell 'iteration-category 'current_B)) -> (get-cell 'iteration-category 'next_A)
              (lambda (vals _) (let ((running? (car vals)) (b (cadr vals)))
                                (if (and running? (number? b)) (cons b '()) (cons *nothing* '())))))
  (propagator p-next-b (list (get-cell 'iteration-category 'keep_running) (get-cell 'iteration-category 'current_A) (get-cell 'iteration-category 'current_B)) -> (get-cell 'iteration-category 'next_B)
              (lambda (vals _) (let ((is-running (car vals)) (a (cadr vals)) (b (caddr vals)))
                                (if (and is-running (number? a) (number? b) (not (zero? b))) (cons (modulo a b) '()) (cons *nothing* '())))))
  ;; Termination Condition: b != 0
  (propagator p-cond (get-cell 'iteration-category 'current_B) -> (get-cell 'iteration-category 'keep_running)
              (lambda (val _) (let ((b val)) (cons (and (number? b) (not (zero? b))) '())))))


;;;
;;; 3. Compose the final system from all components
;;;
(define ComputationSystem
  (compose-systems
   (systems
    conditional-engine
    user-app
    power-of-two-engine
    factorial-engine
    gcd-engine)
   (connections)
   (execution
    ;; Wire components using Functors for a more declarative style
    (apply-functor-as-connections
     (make-system-functor
      (name app->cond)
      (from user-app.user-app-category) (to conditional-engine.conditional-category)
      (mappings (branch-cond -> p) (then-val -> a) (else-val -> b)))
     (mappings (branch-cond -> p (lambda (v _) (cons v '())))
               (then-val -> a (lambda (v _) (cons v '())))
               (else-val -> b (lambda (v _) (cons v '())))))
    (apply-functor-as-connections
     (make-system-functor
      (name cond->app)
      (from conditional-engine.conditional-category) (to user-app.user-app-category)
      (mappings (result -> branch-result)))
     (mappings (result -> branch-result (lambda (v _) (cons v '())))))

    (apply-functor-as-connections
     (make-system-functor
      (name app->p2)
      (from user-app.user-app-category) (to power-of-two-engine.iteration-category)
      (mappings (power-of-two-input -> start_A) (power-of-two-start-val -> start_B)))
     (mappings (power-of-two-input -> start_A (lambda (v _) (cons v '())))
               (power-of-two-start-val -> start_B (lambda (v _) (cons v '())))))
    (apply-functor-as-connections
     (make-system-functor
      (name p2->app)
      (from power-of-two-engine.iteration-category) (to user-app.user-app-category)
      (mappings (final_B -> power-of-two-result)))
     (mappings (final_B -> power-of-two-result (lambda (v _) (cons v '())))))

    (apply-functor-as-connections
     (make-system-functor
      (name app->fact)
      (from user-app.user-app-category) (to factorial-engine.iteration-category)
      (mappings (factorial-input -> start_A)))
     (mappings (factorial-input -> start_A (lambda (v _) (cons v '())))))
    ;; This propagator sets a constant start value, which is not a simple mapping.
    (propagator w-fact-start-b (get-cell 'factorial-engine.iteration-category 'start_A) -> (get-cell 'factorial-engine.iteration-category 'start_B) (lambda (v _) (cons 1 '())))
    (apply-functor-as-connections
     (make-system-functor
      (name fact->app)
      (from factorial-engine.iteration-category) (to user-app.user-app-category)
      (mappings (final_B -> factorial-result)))
     (mappings (final_B -> factorial-result (lambda (v _) (cons v '())))))

    (apply-functor-as-connections
     (make-system-functor
      (name app->gcd)
      (from user-app.user-app-category) (to gcd-engine.iteration-category)
      (mappings (gcd-a-input -> start_A) (gcd-b-input -> start_B)))
     (mappings (gcd-a-input -> start_A (lambda (v _) (cons v '())))
               (gcd-b-input -> start_B (lambda (v _) (cons v '())))))
    (apply-functor-as-connections
     (make-system-functor
      (name gcd->app)
      (from gcd-engine.iteration-category) (to user-app.user-app-category)
      (mappings (final_A -> gcd-result)))
     (mappings (final_A -> gcd-result (lambda (v _) (cons v '())))))
    
    (define-scenario run-conditional-scenario
      (show-state "--- [Conditional] Initial State ---")
      (trigger (get-cell 'user-app.user-app-category 'branch-cond) #t)
      (trigger (get-cell 'user-app.user-app-category 'then-val) 10)
      (trigger (get-cell 'user-app.user-app-category 'else-val) 20) (run)
      (show-state "[Conditional] branch-cond is #t, result should be 10")
      (trigger (get-cell 'user-app.user-app-category 'branch-cond) #f) (run)
      (show-state "[Conditional] branch-cond is #f, result should be 20")
      (trigger (get-cell 'user-app.user-app-category 'then-val) 100) (run)
      (show-state "[Conditional] then-val changed, result should still be 20")
      (trigger (get-cell 'user-app.user-app-category 'branch-cond) #t) (run)
      (show-state "[Conditional] branch-cond is #t, result should now be new value of a (100)"))

    (define-scenario run-iteration-scenario
      (show-state "--- [Iteration] Initial State ---")
      (trigger (get-cell 'user-app.user-app-category 'power-of-two-input) 3)
      (trigger (get-cell 'user-app.user-app-category 'power-of-two-start-val) 10) (run)
      (show-state "[Iteration] 10 * 2^3 should be 80")
      (trigger (get-cell 'user-app.user-app-category 'power-of-two-input) 5)
      (trigger (get-cell 'user-app.user-app-category 'power-of-two-start-val) 1) (run)
      (show-state "[Iteration] 1 * 2^5 should be 32")
      (trigger (get-cell 'user-app.user-app-category 'power-of-two-input) 0)
      (trigger (get-cell 'user-app.user-app-category 'power-of-two-start-val) 1) (run)
      (show-state "[Iteration] 1 * 2^0 should be 1"))

    (define-scenario run-factorial-scenario
      (show-state "--- [Factorial] Initial State ---")
      (trigger (get-cell 'user-app.user-app-category 'factorial-input) 5) (run)
      (show-state "[Factorial] 5! should be 120")
      (trigger (get-cell 'user-app.user-app-category 'factorial-input) 1) (run)
      (show-state "[Factorial] 1! should be 1")
      (trigger (get-cell 'user-app.user-app-category 'factorial-input) 0) (run)
      (show-state "[Factorial] 0! should be 1"))

    (define-scenario run-gcd-scenario
      (show-state "--- [GCD] Initial State ---")
      (trigger (get-cell 'user-app.user-app-category 'gcd-a-input) 48)
      (trigger (get-cell 'user-app.user-app-category 'gcd-b-input) 18) (run)
      (show-state "[GCD] gcd(48, 18) should be 6")
      (trigger (get-cell 'user-app.user-app-category 'gcd-a-input) 101)
      (trigger (get-cell 'user-app.user-app-category 'gcd-b-input) 10) (run)
      (show-state "[GCD] gcd(101, 10) should be 1"))

    (visualize "composed-system.dot")
    (run-conditional-scenario)
    (run-iteration-scenario)
    (run-factorial-scenario)
    (run-gcd-scenario))))
