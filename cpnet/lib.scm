(define-module (cpnet lib)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet detail)
  #:export (make-apply-gate))

(define (define-apply-gate-propagators cells prop-id)
  (let ((c-fn-in (hash-ref cells 'fn-in))
        (c-arg-in (hash-ref cells 'arg-in))
        (c-result-out (hash-ref cells 'result-out))
        (c-fn-val (hash-ref cells 'internal-fn-val))
        (c-arg-val (hash-ref cells 'internal-arg-val))
        (c-fn-ready (hash-ref cells 'internal-fn-ready))
        (c-arg-ready (hash-ref cells 'internal-arg-ready))
        (c-apply-trigger (hash-ref cells 'internal-apply-trigger)))
    (list
     (make-propagator (prop-id "p-latch-fn") c-fn-in c-fn-val
                      (lambda (v src-cell) (cons v (list (make-effect 'set-value (cons c-fn-ready #t))
                                                            (make-effect 'set-value (cons src-cell #f))))))
     (make-propagator (prop-id "p-latch-arg") c-arg-in c-arg-val
                      (lambda (v src-cell) (cons v (list (make-effect 'set-value (cons c-arg-ready #t))
                                                            (make-effect 'set-value (cons src-cell #f))))))
     (make-propagator (prop-id "p-check-join-from-fn") c-fn-ready c-apply-trigger
                      (lambda (fn-ready? _) (if (and fn-ready? (cell-value c-arg-ready)) (cons #t '()) (cons #f '()))))
     (make-propagator (prop-id "p-check-join-from-arg") c-arg-ready c-apply-trigger
                      (lambda (arg-ready? _) (if (and arg-ready? (cell-value c-fn-ready)) (cons #t '()) (cons #f '()))))
     (make-propagator (prop-id "p-apply") c-apply-trigger c-result-out
                      (lambda (trigger? src-cell)
                        (if trigger?
                            (let ((fn (cell-value c-fn-val))
                                  (arg (cell-value c-arg-val)))
                              (cons (fn arg)
                                    (list (make-effect 'set-value (cons src-cell #f))
                                          (make-effect 'set-value (cons c-fn-ready #f))
                                          (make-effect 'set-value (cons c-arg-ready #f))
                                          (make-effect 'set-value (cons c-fn-val #f))
                                          (make-effect 'set-value (cons c-arg-val #f)))))
                            (cons #f '())))))))

(define make-apply-gate
  (make-component-factory
   '((fn-in #f) (arg-in #f) (result-out #f))
   '((internal-fn-val #f)
     (internal-arg-val #f)
     (internal-fn-ready #f)
     (internal-arg-ready #f)
     (internal-apply-trigger #f))
   define-apply-gate-propagators))
