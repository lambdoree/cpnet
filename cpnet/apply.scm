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
   (instance result Data #f replace-merge-fn)))

(define-cpnet-system apply-gate
  (apply-interface)
  (propagator p-eval-apply
              (list (get-cell 'apply-interface 'code) (get-cell 'apply-interface 'args))
              -> (get-cell 'apply-interface 'result)
              (effect-scope 'apply-gate
                (lambda (vals srcs)
                  (let ((cb (car vals))
                        (arg-cells (cadr vals)))
                    (if (and cb (category-builder? cb) (list? arg-cells) (not (null? arg-cells)))
                        (let* ((builder-name (builder-name cb))
                               (builder-proc (builder-function cb))
                               (instance-name (gensym (format #f "~a-instance-" builder-name)))
                               (mangled-cat-name (string->symbol (format #f "~a.~a" instance-name builder-name)))
                               (my-cat-prefix (string->symbol (string-join (reverse (cdr (reverse (string-split (symbol->string (cell-id (car srcs))) #\.)))) ".")))
                               (result-cell (system-find-cell (cell-system (car srcs)) my-cat-prefix 'result))
                               (arg1-cell (car arg-cells))
                               (arg2-cell (cadr arg-cells)))
                          (cons *nothing*
                                (list
                                 (make-effect 'add-subsystem (list instance-name builder-proc))
                                 (make-effect 'add-morphisms
                                              (list
                                               ;; arg1 -> instance.X
                                               (list (list arg1-cell) (list (list mangled-cat-name 'X)) (lambda (v _) (cons v '())))
                                               ;; arg2 -> instance.Y
                                               (list (list arg2-cell) (list (list mangled-cat-name 'Y)) (lambda (v _) (cons v '())))
                                               ;; instance.Z -> result
                                               (list (list (list mangled-cat-name 'Z)) (list result-cell) (lambda (v _) (cons v '()))))))))
                        (cons *nothing* '())))))))
