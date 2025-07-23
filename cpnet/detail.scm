(define-module (cpnet detail)
  #:use-module (cpnet core)
  #:use-module (cpnet category)
  #:export (make-event-propagator
            make-dispatcher-propagator
            implement-component))

;;; --- event processing propagator ---

(define (make-event-propagator id src tgt user-fn)
  (make-propagator id src tgt
		   (lambda (val src-cell)
		     (if val
			 (let ((result (user-fn val)))
			   (cons (car result)
				 (cons (make-effect 'set-value (cons src-cell #f))
				       (cdr result))))
			 (cons #f '())))))


;;; --- dispatcher propagator ---

(define (make-dispatcher-propagator id input-cell dispatcher-fn)
  (make-event-propagator id input-cell input-cell
			 (lambda (val)
			   (cons #f (dispatcher-fn val)))))


;;; --- implement component ---

(define (implement-component interface-net private-cells propagators)
  (for-each (lambda (c) (category-add-object interface-net c)) private-cells)
  (for-each (lambda (p) (category-add-morphism interface-net p)) propagators)
  interface-net)
