(define-module (cpnet apply)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:export (apply-gate
            apply-interface))

(define-category apply-interface
  (objects
   (instance code category-builder #f 'Replace)
   (instance args Data '() 'Replace) ; list of input cells
   (instance results Data '() 'Replace) ; list of output cells
   (instance is-built Bool #f 'Replace)
   (instance built-instance-name Data #f 'Replace)))

(define-cpnet-system apply-gate
  (apply-interface)
  (propagator p-reset-on-code-change
              (list (get-cell 'apply-interface 'code))
              -> (get-cell 'apply-interface 'is-built)
              (lambda (vals _) (cons #f '()))
              10)
  (propagator p-cleanup
              (list (get-cell 'apply-interface 'is-built) (get-cell 'apply-interface 'built-instance-name))
              -> (get-cell 'apply-interface 'built-instance-name)
              (effect-scope 'apply-gate
                            (lambda (vals srcs)
                              (let ((is-built (car vals))
                                    (instance-name (cadr vals)))
                                (if (and (not is-built) instance-name)
                                    (cons *nothing* (list (make-effect 'remove-subsystem instance-name)
                                                          (make-effect 'set-cell (list (cadr srcs) #f))))
                                    (cons *nothing* '())))))
              9)
  (propagator p-eval-apply
              (list (get-cell 'apply-interface 'code) (get-cell 'apply-interface 'args) (get-cell 'apply-interface 'results) (get-cell 'apply-interface 'is-built))
              -> (get-cell 'apply-interface 'built-instance-name)
              (effect-scope 'apply-gate
			    (lambda (vals srcs)
			      (let ((cb           (car vals))
				    (arg-cells    (cadr vals))
				    (result-cells (caddr vals))
				    (is-built     (cadddr vals)))
				(if (and (not is-built) cb (category-builder? cb) (list? arg-cells) (list? result-cells))
				    (let* ((builder-name (builder-name cb))
					   (builder-proc (builder-function cb))
					   (signature (builder-signature cb))
					   (in-ports (cadr (assoc 'inputs signature)))
					   (out-ports (cadr (assoc 'outputs signature)))
					   (instance-name (gensym (format #f "~a-instance-" builder-name)))
					   (cell-path-parts (string-split (symbol->string (cell-id (car srcs))) #\.))
					   (interface-prefix (string->symbol (string-join (reverse (cdr (reverse cell-path-parts))) ".")))
					   (built-instance-name-cell (system-find-cell (cell-system (car srcs)) interface-prefix 'built-instance-name))
					   (is-built-cell (cadddr srcs)))
				      (if (and (= (length arg-cells) (length in-ports))
					       (= (length result-cells) (length out-ports)))
					  (cons *nothing*
						(list
						 (make-effect 'add-subsystem (list instance-name builder-proc))
						 (make-effect 'add-morphisms
							      (append
							       (map (lambda (src-cell port-name)
								      (let ((tgt-cell-ref (list (string->symbol (format #f "~a.~a" instance-name builder-name)) port-name)))
									(list (list src-cell) (list tgt-cell-ref) (lambda (v _) (cons (list (car v)) '())))))
								    arg-cells in-ports)
							       (map (lambda (port-name tgt-cell)
								      (let ((src-cell-ref (list (string->symbol (format #f "~a.~a" instance-name builder-name)) port-name)))
									(list (list src-cell-ref) (list tgt-cell) (lambda (v _) (cons (list (car v)) '())))))
								    out-ports result-cells)))
						 (make-effect 'set-cell (list is-built-cell #t))
						 (make-effect 'set-cell (list built-instance-name-cell instance-name))))
					  (begin
					    (format (current-error-port) "apply-gate: Arity mismatch for ~a. Expected ~a inputs, ~a outputs. Got ~a inputs, ~a outputs.\n"
						    builder-name (length in-ports) (length out-ports) (length arg-cells) (length result-cells))
					    (cons *nothing* '()))))
				    (cons *nothing* '())))))))
