(define-module (cpnet apply)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:export (apply-gate
            apply-interface))

(define-category apply-interface
  (objects
   (instance code category-builder #f replace-merge-fn)
   (instance args Data '() replace-merge-fn)
   (instance result Data #f replace-merge-fn)
   (instance is-built Bool #f replace-merge-fn)
   (instance built-instance-name Data #f replace-merge-fn)))

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
              (list (get-cell 'apply-interface 'code) (get-cell 'apply-interface 'args) (get-cell 'apply-interface 'is-built))
              -> (get-cell 'apply-interface 'result)
              (effect-scope 'apply-gate
			    (lambda (vals srcs)
			      (let ((cb (car vals))
				    (arg-cells (cadr vals))
				    (is-built (caddr vals)))
				(if (and (not is-built) cb (category-builder? cb) (list? arg-cells) (not (null? arg-cells)))
				    (let* ((builder-name (builder-name cb))
					   (builder-proc (builder-function cb))
					   (instance-name (gensym (format #f "~a-instance-" builder-name)))
					   (cell-path-parts (string-split (symbol->string (cell-id (car srcs))) #\.))
					   (interface-prefix (string->symbol (string-join (reverse (cdr (reverse cell-path-parts))) ".")))
					   (system-prefix (if (> (length cell-path-parts) 2) (string->symbol (string-join (reverse (cddr (reverse cell-path-parts))) ".")) '()))
					   (mangled-cat-name (string->symbol (format #f "~a.~a" instance-name builder-name)))
					   (result-cell (system-find-cell (cell-system (car srcs)) interface-prefix 'result))
					   (built-instance-name-cell (system-find-cell (cell-system (car srcs)) interface-prefix 'built-instance-name))
					   (arg1-cell (car arg-cells))
					   (arg2-cell (cadr arg-cells)))
				      (cons *nothing*
					    (list
					     (make-effect 'add-subsystem (list instance-name builder-proc))
					     (make-effect 'add-morphisms
							  (list
							   ;; arg1 -> instance.X
							   (list (list arg1-cell) (list (list mangled-cat-name 'X)) (lambda (vals _) (cons vals '())))
							   ;; arg2 -> instance.Y
							   (list (list arg2-cell) (list (list mangled-cat-name 'Y)) (lambda (vals _) (cons vals '())))
							   ;; instance.Z -> result
							   (list (list (list mangled-cat-name 'Z)) (list result-cell) (lambda (vals _) (cons vals '())))))
					     (make-effect 'set-cell (list (caddr srcs) #t))
					     (make-effect 'set-cell (list built-instance-name-cell instance-name)))))
				    (cons *nothing* '())))))))
