(define-module (cpnet pure-lambda)
  #:use-module (cpnet system)
  #:use-module (cpnet core)
  #:use-module (cpnet detail)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (pfn?
            make-pfn
            pfn-c-in
            pfn-c-out
            p-pure-apply
            make-pure-combinator-K
            make-pure-combinator-S))

(define-record-type <pfn>
  (make-pfn c-in c-out)
  pfn?
  (c-in pfn-c-in)
  (c-out pfn-c-out))

(define (p-pure-apply id c-func c-arg c-result)
  (make-propagator id c-func c-func
		   (lambda (fn-rep src-cell)
		     (if (pfn? fn-rep)
			 (let ((c-in (pfn-c-in fn-rep))
			       (c-out (pfn-c-out fn-rep)))
			   (cons #f
				 (list (make-effect 'set-value (cons src-cell #f))
				       (make-effect 'connect-once (cons c-arg c-in))
				       (make-effect 'connect-once (cons c-out c-result)))))
			 (cons #f (list (make-effect 'set-value (cons src-cell #f))))))))

(define (define-K-propagators cells prop-id)
  (let ((c-x-in (hash-ref cells 'x-in))
        (c-fn-out (hash-ref cells 'fn-out))
        (c-y-in (hash-ref cells 'k-internal-y-in))
        (c-x-out (hash-ref cells 'k-internal-x-out)))
    (list
     (make-propagator (prop-id "p-k-activate") c-x-in c-fn-out
                      (lambda (val src-cell)
                        (if val
                            (let ((effects (list (make-effect 'set-value (cons c-x-out val))
                                                 (make-effect 'set-value (cons src-cell #f)))))
                              (cons (make-pfn c-y-in c-x-out) effects))
                            (cons #f '()))))
     (make-propagator (prop-id "p-k-consume-y") c-y-in c-y-in
                      (lambda (val src-cell)
                        (if val
                            (let ((effects (list (make-effect 'set-value (cons src-cell #f)))))
                              (if (pfn? val)
                                  (cons #f (append effects (list (make-effect 'set-value (cons (pfn-c-out val) #f)))))
                                  (cons #f effects)))
                            (cons #f '())))))))

(define make-pure-combinator-K
  (make-component-factory
   '((x-in #f) (fn-out #f))
   '((k-internal-y-in #f)
     (k-internal-x-out #f))
   define-K-propagators))

(define (define-S-propagators cells prop-id)
  (let ((c-x-in (hash-ref cells 'x-in)) (c-y-in (hash-ref cells 'y-in)) (c-fn-out (hash-ref cells 'fn-out))
        (c-x-val (hash-ref cells 's-internal-x-val)) (c-y-val (hash-ref cells 's-internal-y-val))
        (c-x-ready (hash-ref cells 's-internal-x-ready)) (c-y-ready (hash-ref cells 's-internal-y-ready))
        (c-package-trigger (hash-ref cells 's-internal-package-trigger))
        (c-z-in (hash-ref cells 's-internal-z-in)) (c-result-out (hash-ref cells 's-internal-result-out))
        (c-x-z-func (hash-ref cells 's-internal-x-z-func)) (c-x-z-arg (hash-ref cells 's-internal-x-z-arg))
        (c-x-z-result (hash-ref cells 's-internal-x-z-result))
        (c-y-z-func (hash-ref cells 's-internal-y-z-func)) (c-y-z-arg (hash-ref cells 's-internal-y-z-arg))
        (c-y-z-result (hash-ref cells 's-internal-y-z-result))
        (c-apply-func (hash-ref cells 's-internal-apply-func)) (c-apply-arg (hash-ref cells 's-internal-apply-arg)))
    (list
     (make-propagator (prop-id "p-s-latch-x") c-x-in c-x-val
                      (lambda (v src-cell)
                        (cons v (list (make-effect 'set-value (cons c-x-ready #t))
                                      (make-effect 'set-value (cons src-cell #f))))))
     (make-propagator (prop-id "p-s-latch-y") c-y-in c-y-val
                      (lambda (v src-cell)
                        (cons v (list (make-effect 'set-value (cons c-y-ready #t))
                                      (make-effect 'set-value (cons src-cell #f))))))
     (make-propagator (prop-id "p-s-check-join-from-x") c-x-ready c-package-trigger
                      (lambda (x-ready? _)
                        (if (and x-ready? (cell-value c-y-ready))
                            (cons #t '())
                            (cons #f '()))))
     (make-propagator (prop-id "p-s-check-join-from-y") c-y-ready c-package-trigger
                      (lambda (y-ready? _)
                        (if (and y-ready? (cell-value c-x-ready))
                            (cons #t '())
                            (cons #f '()))))
     (make-propagator (prop-id "p-s-dist-x") c-x-val c-x-z-func
                      (lambda (v src-cell)
                        (cons v (list (make-effect 'set-value (cons src-cell #f))))))
     (make-propagator (prop-id "p-s-dist-y") c-y-val c-y-z-func
                      (lambda (v src-cell)
                        (cons v (list (make-effect 'set-value (cons src-cell #f))))))
     (make-fan-out-propagator (prop-id "p-s-fan-z") c-z-in (list c-x-z-arg c-y-z-arg))
     (p-pure-apply (prop-id "p-s-apply-xz") c-x-z-func c-x-z-arg c-x-z-result)
     (p-pure-apply (prop-id "p-s-apply-yz") c-y-z-func c-y-z-arg c-y-z-result)
     (make-propagator (prop-id "p-s-setup-apply-func") c-x-z-result c-apply-func
                      (lambda (v src-cell)
                        (cons v (list (make-effect 'set-value (cons src-cell #f))))))
     (make-propagator (prop-id "p-s-setup-apply-arg") c-y-z-result c-apply-arg
                      (lambda (v src-cell)
                        (cons v (list (make-effect 'set-value (cons src-cell #f))))))
     (p-pure-apply (prop-id "p-s-apply-final") c-apply-func c-apply-arg c-result-out)
     (make-propagator (prop-id "p-pack-s-fn") c-package-trigger c-fn-out
                      (lambda (val src-cell)
                        (if val
                            (cons (make-pfn c-z-in c-result-out)
                                  (list (make-effect 'set-value (cons src-cell #f))
                                        (make-effect 'set-value (cons c-x-ready #f))
                                        (make-effect 'set-value (cons c-y-ready #f))))
                            (cons #f '())))))))

(define make-pure-combinator-S
  (make-component-factory
   '((x-in #f) (y-in #f) (fn-out #f))
   '((s-internal-x-val #f)
     (s-internal-y-val #f)
     (s-internal-x-ready #f)
     (s-internal-y-ready #f)
     (s-internal-package-trigger #f)
     (s-internal-z-in #f)
     (s-internal-result-out #f)
     (s-internal-x-z-func #f)
     (s-internal-x-z-arg #f)
     (s-internal-x-z-result #f)
     (s-internal-y-z-func #f)
     (s-internal-y-z-arg #f)
     (s-internal-y-z-result #f)
     (s-internal-apply-func #f)
     (s-internal-apply-arg #f))
   define-S-propagators))
