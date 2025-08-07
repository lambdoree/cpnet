(define-module (cpnet pure)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet apply)
  #:export (true-gate
	    false-gate
            if-system
            loop-system))

(define-object Data)
(define-object Bool)
(define-object category-builder)

(define-category true-gate
  (objects
   (instance X Data #f replace-merge-fn)
   (instance Y Data #f replace-merge-fn)
   (instance Z Data #f replace-merge-fn))
  (morphisms
   ((morphism select-left (X Y) -> Z)
    (lambda (vals _)
      (cons (car vals) '())))))

(define-category false-gate
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
   (instance true-val category-builder (get-builder 'true-gate) replace-merge-fn)
   (instance false-val category-builder (get-builder 'false-gate) replace-merge-fn)
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

(define-category is-not-f-interface
  (objects
   (instance in Data #f)
   (instance out Bool #f)))

(define-cpnet-system is-not-f-system
  (is-not-f-interface)
  (propagator is-not-f-prop
              (get-cell 'is-not-f-interface 'in)
              -> (get-cell 'is-not-f-interface 'out)
              (lambda (vals _)
                (let ((v (car vals)))
                  (if (eq? v #f)
                      (cons #f '())
                      (cons #t '()))))))

(define-category loop-interface
  (objects
   (instance body category-builder #f)
   (instance input Data #f)
   (instance output Data #f)))

(define-cpnet-system loop-system
  (loop-interface)
  (add-subsystem! (current-system) (apply-gate 'apply-body))
  (add-subsystem! (current-system) (gated-channel-system 'feedback-gate))
  (add-subsystem! (current-system) (is-not-f-system 'gate-control))

  (wire (get-cell 'loop-interface 'body) (get-cell 'apply-body 'apply-interface 'code))

  (wire (get-cell 'apply-body 'apply-interface 'result) (get-cell 'gate-control 'is-not-f-interface 'in))
  (wire (get-cell 'gate-control 'is-not-f-interface 'out) (get-cell 'feedback-gate 'gated-channel-interface 'control))

  (wire (get-cell 'apply-body 'apply-interface 'result) (get-cell 'feedback-gate 'gated-channel-interface 'input))
  (propagator feedback-output-prop
              (get-cell 'feedback-gate 'gated-channel-interface 'output)
              -> (get-cell 'loop-interface 'output)
              (lambda (vals _)
                (let ((v (car vals)))
                  (if (eq? v #f)
                      (cons *nothing* '())
                      (cons v '())))))

  (propagator loop-system-arg-setup
              (list (get-cell 'loop-interface 'input) (get-cell 'loop-interface 'output))
              -> (get-cell 'apply-body 'apply-interface 'args)
              (lambda (vals srcs)
		(cons srcs '()))))

