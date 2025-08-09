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
   (instance args Data '() 'Replace)
   (instance results Data '() 'Replace)
   (instance built-instance-name Data #f 'Replace)
   (instance built-from-code category-builder #f 'Replace)))

(define-cpnet-system apply-gate
  (apply-interface)
  (let ((manage-fn
         (effect-scope 'apply-gate
           (lambda (vals srcs)
             (let ((code (car vals))
                   (instance-name (cadr vals))
                   (built-code (caddr vals))
                   (arg-cells (cadddr vals))
                   (result-cells (list-ref vals 4)))
               (if (eq? code built-code)
                   ;; System is in sync, do nothing.
                   (cons (list *nothing* *nothing*) '())
                   ;; System is out of sync, action needed.
                   (cond
                    ;; Teardown needed: an instance exists from old code.
                    (instance-name
                     (cons (list #f #f) (list (make-effect 'remove-subsystem instance-name))))
                    ;; Build needed: no instance exists, and new code is provided.
                    (code
                     (if (not (category-builder? code))
                         (begin
                           (format (current-error-port) "cpnet/apply: ERROR: code cell does not contain a valid category-builder. Value is: ~s\n" code)
                           (cons (list *nothing* *nothing*) '()))
                         (let* ((cb code)
                                (builder-name (builder-name cb))
                                (builder-proc (builder-function cb))
                                (signature (builder-signature cb))
                                (input-assoc (assoc 'inputs signature))
                                (output-assoc (assoc 'outputs signature)))
                           (if (or (not input-assoc) (not output-assoc))
                               (begin
                                 (format (current-error-port) "cpnet/apply: ERROR: Malformed signature for builder ~s. Must contain 'inputs' and 'outputs'. Signature was: ~s\n" builder-name signature)
                                 (cons (list code #f) '()))
                               (let* ((in-ports (cadr input-assoc))
                                      (out-ports (cadr output-assoc))
                                      (new-instance-name (gensym (format #f "~a-instance-" builder-name)))
                                      (arity-ok? (and (list? arg-cells) (list? result-cells)
                                                      (= (length arg-cells) (length in-ports))
                                                      (= (length result-cells) (length out-ports)))))
                                 (cond
                                   (arity-ok?
                                    (cons (list new-instance-name code) ; Set instance and lock in built-from-code
                                          (list
                                           (make-effect 'add-subsystem (list new-instance-name builder-proc))
                                           (make-effect 'add-morphisms
                                                        (append
                                                         (map (lambda (src-cell port-name)
                                                                (let ((tgt-cell-ref (list (string->symbol (format #f "~a.~a" new-instance-name builder-name)) port-name)))
                                                                  (list (list src-cell) (list tgt-cell-ref) (lambda (v _) (cons (list (car v)) '())))))
                                                              arg-cells in-ports)
                                                         (map (lambda (port-name tgt-cell)
                                                                (let ((src-cell-ref (list (string->symbol (format #f "~a.~a" new-instance-name builder-name)) port-name)))
                                                                  (list (list src-cell-ref) (list tgt-cell) (lambda (v _) (cons (list (car v)) '())))))
                                                              out-ports result-cells))))))
                                   (else
                                    (begin
                                      (format (current-error-port) "cpnet/apply: Arity mismatch for builder ~s. Expected ~a/~a, Got ~a/~a. arity-ok?=~s\n"
                                              builder-name (length in-ports) (length out-ports) (length arg-cells) (length result-cells) arity-ok?)
                                      (cons (list #f code) '()))))))))) ; Arity mismatch, lock code and clear instance.
                    ;; Code is nil and no instance exists. Do nothing.
                    (else (cons (list *nothing* *nothing*) '())))))))))
    (propagator p-manager
                (list (get-cell 'apply-interface 'code)
                      (get-cell 'apply-interface 'built-instance-name)
                      (get-cell 'apply-interface 'built-from-code)
                      (get-cell 'apply-interface 'args)
                      (get-cell 'apply-interface 'results))
                -> (list (get-cell 'apply-interface 'built-instance-name)
                         (get-cell 'apply-interface 'built-from-code))
                manage-fn
                9)))
