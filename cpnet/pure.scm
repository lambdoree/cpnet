(define-module (cpnet pure)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet apply)
  #:export (true-gate
	    false-gate
            if-system
            loop-system))

(define (select-non-f-merge-fn cell new-vals)
  (let ((filtered-vals (filter (lambda (v) (not (eq? v #f))) new-vals)))
    (if (null? filtered-vals)
        (cons #f '())
        (default-merge-fn cell filtered-vals))))

(define (ignore-f-merge-fn cell new-vals)
  (let ((filtered-vals (filter (lambda (v) (not (eq? v #f))) new-vals)))
    (if (null? filtered-vals)
        (cons (cell-value cell) '())
        (default-merge-fn cell filtered-vals))))

(define-category const-bool-provider
  (objects
   (instance true Bool #t)
   (instance false Bool #f)))

(define-category const-f-provider
  (objects
   (instance f Data #f)))

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

(define-category if-interface
  (objects
   (instance condition Bool #f replace-merge-fn)
   (instance then-val Data #f replace-merge-fn)
   (instance else-val Data #f replace-merge-fn)
   (instance result Data #f select-non-f-merge-fn)))

(define-cpnet-system if-system
  (if-interface)
  (add-subsystem! (current-system) (gated-channel-system 'true-path))
  (add-subsystem! (current-system) (gated-channel-system 'false-path))

  (propagator true-path-control
              (get-cell 'if-interface 'condition)
              -> (get-cell 'true-path 'gated-channel-interface 'control)
              (lambda (vals _) (cons (eq? (car vals) #t) '())))

  (propagator false-path-control
              (get-cell 'if-interface 'condition)
              -> (get-cell 'false-path 'gated-channel-interface 'control)
              (lambda (vals _) (cons (eq? (car vals) #f) '())))

  (wire (get-cell 'if-interface 'then-val) (get-cell 'true-path 'gated-channel-interface 'input))
  (wire (get-cell 'if-interface 'else-val) (get-cell 'false-path 'gated-channel-interface 'input))
  
  (wire (get-cell 'true-path 'gated-channel-interface 'output) (get-cell 'if-interface 'result))
  (wire (get-cell 'false-path 'gated-channel-interface 'output) (get-cell 'if-interface 'result)))

(define-category gated-channel-interface
  (objects
   (instance control Bool #f)
   (instance input Data #f)
   (instance output Data #f)))

(define-cpnet-system gated-channel-system
  (gated-channel-interface)
  (propagator p-gate
              (list (get-cell 'gated-channel-interface 'control)
                    (get-cell 'gated-channel-interface 'input))
              -> (get-cell 'gated-channel-interface 'output)
              (lambda (vals _)
                ;; control is #t -> output is input
                ;; control is #f -> output is #f
                (cons (and (car vals) (cadr vals)) '()))))

(define-category is-not-f-interface
  (objects
   (instance in Data #f)
   (instance out Bool #f)))

(define-cpnet-system is-not-f-system
  (is-not-f-interface)
  (add-subsystem! (current-system) (if-system 'if-impl))
  (add-subsystem! (current-system) (const-bool-provider 'consts))

  (propagator is-f-cond
              (get-cell 'is-not-f-interface 'in)
              -> (get-cell 'if-impl 'if-interface 'condition)
              (lambda (vals _)
                (let ((v (car vals)))
                  (cons (eq? v #f) '()))))

  (wire (get-cell 'consts 'const-bool-provider 'false) (get-cell 'if-impl 'if-interface 'then-val))
  (wire (get-cell 'consts 'const-bool-provider 'true) (get-cell 'if-impl 'if-interface 'else-val))

  (wire (get-cell 'if-impl 'if-interface 'result) (get-cell 'is-not-f-interface 'out)))

(define-category loop-interface
  (objects
   (instance body category-builder #f)
   (instance input Data #f)
   (instance output Data #f ignore-f-merge-fn)))

(define-cpnet-system loop-system
  (loop-interface)
  (add-subsystem! (current-system) (apply-gate 'apply-body))
  (add-subsystem! (current-system) (gated-channel-system 'feedback-gate))
  (add-subsystem! (current-system) (is-not-f-system 'gate-control))

  (wire (get-cell 'loop-interface 'body) (get-cell 'apply-body 'apply-interface 'code))

  (wire (get-cell 'apply-body 'apply-interface 'result) (get-cell 'gate-control 'is-not-f-interface 'in))
  (wire (get-cell 'gate-control 'is-not-f-interface 'out) (get-cell 'feedback-gate 'gated-channel-interface 'control))

  (wire (get-cell 'apply-body 'apply-interface 'result) (get-cell 'feedback-gate 'gated-channel-interface 'input))
  (wire (get-cell 'feedback-gate 'gated-channel-interface 'output) (get-cell 'loop-interface 'output))

  (propagator loop-system-arg-setup
              (list (get-cell 'loop-interface 'input) (get-cell 'loop-interface 'output))
              -> (get-cell 'apply-body 'apply-interface 'args)
              (lambda (vals srcs)
		(cons srcs '()))))

