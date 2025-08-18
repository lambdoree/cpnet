(define-module (cpnet apply)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:export (apply-gate
            apply-interface))

;; propagator 함수: 입력 값 리스트의 첫 번째 원소를 리스트로 감싸 반환합니다.
(define (forward-first-val-as-list v _) (cons (list (car v)) '()))

(define-category apply-interface
  (objects
   (instance code category-builder #f 'Replace)
   (instance args Data '() 'Replace)
   (instance results Data '() 'Replace)
   (instance built-instance-name Data #f 'Replace)
   (instance built-from-code category-builder #f 'Replace)))

;; `apply-gate`의 핵심 로직 핸들러.
;; `code` cell의 내용(category-builder)이 변경되면, 이전 하위 시스템을 제거하고
;; 새로운 하위 시스템을 동적으로 생성 및 연결하는 이펙트(effect)를 발생시킵니다.
(define (apply-manage-handler vals srcs)
  (let ((code (car vals))
        (instance-name (cadr vals))
        (built-code (caddr vals))
        (arg-cells (cadddr vals))
        (result-cells (list-ref vals 4)))
    (if (or (eq? code built-code)
            (and (category-builder? code)
                 (category-builder? built-code)
                 (eq? (builder-name code) (builder-name built-code))))
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
                (format (current-output-port) "cpnet/apply: ERROR: code cell does not contain a valid category-builder. Value is: ~s\n" code)
                (cons (list *nothing* *nothing*) '()))
              (let* ((cb code)
                     (builder-name (builder-name cb))
                     (builder-proc (builder-function cb))
                     (signature (builder-signature cb))
                     (input-assoc (assoc 'inputs signature))
                     (output-assoc (assoc 'outputs signature)))
                (if (or (not input-assoc) (not output-assoc))
                    (begin
                      (format (current-output-port) "cpnet/apply: ERROR: Malformed signature for builder ~s. Must contain 'inputs' and 'outputs'. Signature was: ~s\n" builder-name signature)
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
                                                       (list (list src-cell) (list tgt-cell-ref) forward-first-val-as-list)))
                                                   arg-cells in-ports)
                                              (map (lambda (port-name tgt-cell)
                                                     (let ((src-cell-ref (list (string->symbol (format #f "~a.~a" new-instance-name builder-name)) port-name)))
                                                       (list (list src-cell-ref) (list tgt-cell) forward-first-val-as-list)))
                                                   out-ports result-cells))))))
                        (else
                         (begin
                           (format (current-output-port) "cpnet/apply: Arity mismatch for builder ~s. Expected ~a/~a, Got ~a/~a. arity-ok?=~s\n"
                                   builder-name (length in-ports) (length out-ports) (length arg-cells) (length result-cells) arity-ok?)
                           (cons (list #f code) '())))))))))
         ;; Code is nil and no instance exists. Do nothing.
         (else (cons (list *nothing* *nothing*) '()))))))

(define-cpnet-system apply-gate
  (apply-interface)
  (let ((manage-fn (effect-scope 'apply-gate apply-manage-handler)))
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
