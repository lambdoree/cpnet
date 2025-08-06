(define-module (cpnet pure)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet apply)
  #:export (left-gate
	    right-gate
            if-system
            loop-system))

(define-object Data)
(define-object Bool)
(define-object category-builder)

(define-category left-gate
  (objects
   (instance X Data #f replace-merge-fn)
   (instance Y Data #f replace-merge-fn)
   (instance Z Data #f replace-merge-fn))
  (morphisms
   ((morphism select-left (X Y) -> Z)
    (lambda (vals _)
      (cons (car vals) '())))))

(define-category right-gate
  (objects
   (instance X Data #f replace-merge-fn)
   (instance Y Data #f replace-merge-fn)
   (instance Z Data #f replace-merge-fn))
  (morphisms
   ((morphism select-right (X Y) -> Z)
    (lambda (vals _)
      (cons (cadr vals) '())))))

(define-category if-selector
  (objects
   (instance condition Bool #f replace-merge-fn)
   (instance true-val category-builder (get-builder 'left-gate) replace-merge-fn)
   (instance false-val category-builder (get-builder 'right-gate) replace-merge-fn)
   (instance result category-builder #f replace-merge-fn)))

(define-category if-interface
  (objects
   (instance condition Bool #f replace-merge-fn)
   (instance then-val Data #f replace-merge-fn)
   (instance else-val Data #f replace-merge-fn)
   (instance result Data #f replace-merge-fn)))

(define-cpnet-system if-system
  (if-interface)
  (if-selector)
  (add-subsystem! (current-system) (apply-gate 'if-apply-gate))

  (system-add-branch-propagator (current-system) 'if-selector 'condition 'true-val 'false-val 'result)

  (wire (get-cell 'if-interface 'condition) (get-cell 'if-selector 'condition))
  (wire (get-cell 'if-selector 'result) (get-cell 'if-apply-gate 'apply-interface 'code))

  (propagator setup-args-for-if
              (list (get-cell 'if-interface 'then-val)
                    (get-cell 'if-interface 'else-val))
              -> (get-cell 'if-apply-gate 'apply-interface 'args)
              (lambda (vals srcs)
                (cons srcs '())))

  (wire (get-cell 'if-apply-gate 'apply-interface 'result)
        (get-cell 'if-interface 'result)))

(define-cpnet-system not-gate
  (define-category not-gate-interface
    (objects
     (instance in Bool #f replace-merge-fn)
     (instance out Bool #f replace-merge-fn)))
  (not-gate-interface)
  (propagator p-invert (get-cell 'not-gate-interface 'in) -> (get-cell 'not-gate-interface 'out)
	      (lambda (vals _) (cons (not (car vals)) '()))))

(define-category gated-channel-interface
  (objects
   (instance control Bool #f)
   (instance input Data #f)
   (instance output Data #f)))

(define-cpnet-system gated-channel-system
  (gated-channel-interface)
  (add-subsystem! (current-system) (if-system 'if-impl))
  (wire (get-cell 'gated-channel-interface 'control) (get-cell 'if-impl 'if-interface 'condition))
  (wire (get-cell 'gated-channel-interface 'input) (get-cell 'if-impl 'if-interface 'then-val))
  (wire (get-cell 'if-impl 'if-interface 'result) (get-cell 'gated-channel-interface 'output))
  (wire (get-cell 'gated-channel-interface 'output) (get-cell 'if-impl 'if-interface 'else-val)))

(define-category const-provider
  (objects
   (instance true Bool #t)
   (instance false Bool #f)))

(define-cpnet-system is-not-false-gate
  (define-category is-not-false-gate-interface
    (objects
     (instance in Data #f replace-merge-fn)
     (instance out Bool #f replace-merge-fn)))
  (is-not-false-gate-interface)
  (propagator p-check (get-cell 'is-not-false-gate-interface 'in) -> (get-cell 'is-not-false-gate-interface 'out)
	      (lambda (vals _) (cons (not (not (car vals))) '()))))

(define-cpnet-system and-gate
  (define-category and-gate-interface
    (objects
     (instance a Bool #f replace-merge-fn)
     (instance b Bool #f replace-merge-fn)
     (instance out Bool #f replace-merge-fn)))
  (and-gate-interface)
  (const-provider)
  (add-subsystem! (current-system) (if-system 'if-impl))
  (wire (get-cell 'and-gate-interface 'a) (get-cell 'if-impl 'if-interface 'condition))
  (wire (get-cell 'and-gate-interface 'b) (get-cell 'if-impl 'if-interface 'then-val))
  (wire (get-cell 'const-provider 'false) (get-cell 'if-impl 'if-interface 'else-val))
  (wire (get-cell 'if-impl 'if-interface 'result) (get-cell 'and-gate-interface 'out)))

(define-cpnet-system or-gate
  (define-category or-gate-interface
    (objects
     (instance a Bool #f replace-merge-fn)
     (instance b Bool #f replace-merge-fn)
     (instance out Bool #f replace-merge-fn)))
  (or-gate-interface)
  (const-provider)
  (add-subsystem! (current-system) (if-system 'if-impl))
  (wire (get-cell 'or-gate-interface 'a) (get-cell 'if-impl 'if-interface 'condition))
  (wire (get-cell 'const-provider 'true) (get-cell 'if-impl 'if-interface 'then-val))
  (wire (get-cell 'or-gate-interface 'b) (get-cell 'if-impl 'if-interface 'else-val))
  (wire (get-cell 'if-impl 'if-interface 'result) (get-cell 'or-gate-interface 'out)))

(define-category sr-latch-combinational-interface
  (objects
   (instance s Bool #f replace-merge-fn)
   (instance r-check Bool #f replace-merge-fn)
   (instance q-old Bool #f replace-merge-fn)
   (instance q-new Bool #f replace-merge-fn)))

(define-cpnet-system sr-latch-combinational-logic
  (sr-latch-combinational-interface)
  (add-subsystem! (current-system) (and-gate 'r-and))
  (add-subsystem! (current-system) (not-gate 'r-not))
  (add-subsystem! (current-system) (and-gate 'q-and-not-r))
  (add-subsystem! (current-system) (or-gate 'final-or))
  ;; R signal logic: r = r-check and q-old
  (wire (get-cell 'sr-latch-combinational-interface 'r-check) (get-cell 'r-and 'and-gate-interface 'a))
  (wire (get-cell 'sr-latch-combinational-interface 'q-old) (get-cell 'r-and 'and-gate-interface 'b))
  ;; Latch equation: q-new = s or (!r and q-old)
  (wire (get-cell 'r-and 'and-gate-interface 'out) (get-cell 'r-not 'not-gate-interface 'in))
  (wire (get-cell 'r-not 'not-gate-interface 'out) (get-cell 'q-and-not-r 'and-gate-interface 'a))
  (wire (get-cell 'sr-latch-combinational-interface 'q-old) (get-cell 'q-and-not-r 'and-gate-interface 'b))
  (wire (get-cell 'sr-latch-combinational-interface 's) (get-cell 'final-or 'or-gate-interface 'a))
  (wire (get-cell 'q-and-not-r 'and-gate-interface 'out) (get-cell 'final-or 'or-gate-interface 'b))
  (wire (get-cell 'final-or 'or-gate-interface 'out) (get-cell 'sr-latch-combinational-interface 'q-new)))

(define-category greater-than-gate
  (objects
   (instance a Data #f)
   (instance b Data #f)
   (instance result Bool #f))
  (morphisms
   ((morphism p> (a b) -> result)
    (lambda (vals _) (cons (> (car vals) (cadr vals)) '())))))

(define-category list-get
  (objects
   (instance list-in Data '())
   (instance index Data 0)
   (instance value Data #f))
  (morphisms
   ((morphism p-get (list-in index) -> value)
    (lambda (vals _)
      (let ((l (car vals)) (i (cadr vals)))
        (if (and (list? l) (>= i 0) (< i (length l)))
            (cons (list-ref l i) '())
            (cons *nothing* '())))))))

(define-category list-set
  (objects
   (instance list-in Data '())
   (instance index Data 0)
   (instance value Data #f)
   (instance list-out Data '()))
  (morphisms
   ((morphism p-set (list-in index value) -> list-out)
    (lambda (vals _)
      (let* ((l (car vals)) (i (cadr vals)) (v (caddr vals)))
        (cons (list-set l i v) '()))))))

(define-category list-length
  (objects
   (instance list-in Data '())
   (instance length Data 0))
  (morphisms
   ((morphism p-len list-in -> length)
    (lambda (vals _) (cons (length (car vals)) '())))))

(define-category loop-internal-state
  (objects
   (instance is-first-run Bool #t replace-merge-fn)))

(define-category loop-interface
  (objects
   (instance start-val Data #f)
   (instance next-val Data #f)
   (instance keep-running Bool #f)
   (instance current-val Data #f) ; output for loop body
   (instance result Data #f)))

(define-cpnet-system loop-system
  (loop-interface)
  (loop-internal-state)
  (const-provider)

  ;; Instantiate subsystems
  (add-subsystem! (current-system) (if-system 'main-mux))
  (add-subsystem! (current-system) (gated-channel-system 'feedback-gate))
  (add-subsystem! (current-system) (gated-channel-system 'output-gate))
  (add-subsystem! (current-system) (not-gate 'not-kr))
  (add-subsystem! (current-system) (is-not-false-gate 's-gate))
  (add-subsystem! (current-system) (is-not-false-gate 'r-check))
  (add-subsystem! (current-system) (sr-latch-combinational-logic 'latch))

  ;; Main MUX to choose between start-val and next-val
  (wire (get-cell 'loop-internal-state 'is-first-run) (get-cell 'main-mux 'if-interface 'condition))
  (wire (get-cell 'loop-interface 'start-val) (get-cell 'main-mux 'if-interface 'then-val))
  (wire (get-cell 'main-mux 'if-interface 'result) (get-cell 'loop-interface 'current-val))

  ;; Feedback gate for the loop's next value
  (wire (get-cell 'loop-interface 'keep-running) (get-cell 'feedback-gate 'gated-channel-interface 'control))
  (wire (get-cell 'loop-interface 'next-val) (get-cell 'feedback-gate 'gated-channel-interface 'input))
  (wire (get-cell 'feedback-gate 'gated-channel-interface 'output) (get-cell 'main-mux 'if-interface 'else-val))

  ;; Output gate for the final result
  (wire (get-cell 'loop-interface 'keep-running) (get-cell 'not-kr 'not-gate-interface 'in))
  (wire (get-cell 'not-kr 'not-gate-interface 'out) (get-cell 'output-gate 'gated-channel-interface 'control))
  (wire (get-cell 'loop-interface 'current-val) (get-cell 'output-gate 'gated-channel-interface 'input))
  (wire (get-cell 'output-gate 'gated-channel-interface 'output) (get-cell 'loop-interface 'result))

  ;; SR Latch logic wiring to control the 'is-first-run' state
  (wire (get-cell 'loop-interface 'start-val) (get-cell 's-gate 'is-not-false-gate-interface 'in))
  (wire (get-cell 'loop-interface 'current-val) (get-cell 'r-check 'is-not-false-gate-interface 'in))
  (wire (get-cell 's-gate 'is-not-false-gate-interface 'out) (get-cell 'latch 'sr-latch-combinational-interface 's))
  (wire (get-cell 'r-check 'is-not-false-gate-interface 'out) (get-cell 'latch 'sr-latch-combinational-interface 'r-check))
  (wire (get-cell 'loop-internal-state 'is-first-run) (get-cell 'latch 'sr-latch-combinational-interface 'q-old))
  (wire (get-cell 'latch 'sr-latch-combinational-interface 'q-new) (get-cell 'loop-internal-state 'is-first-run)))
