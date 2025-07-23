(define-module (examples restaurant detail sales)
  #:use-module (cpnet core)
  #:use-module (cpnet detail)
  #:use-module (examples restaurant static-architecture)
  #:export (ImplementedSalesNet))

;;; --- Propagators ---
(define p-process-sale
  (make-event-propagator 'process-sale sale-event public-dish-sold
			 (lambda (event)
			   (if (and (list? event) (assoc 'item event) (assoc 'quantity event))
			       (let* ((item (cdr (assoc 'item event)))
				      (quantity (cdr (assoc 'quantity event)))
				      (dishes (make-list quantity item)))
				 (cons dishes '()))
			       (cons #f (list (make-effect 'display "WARN: Invalid sale event format\n")))))))

;;; --- Component Implementation ---
(define ImplementedSalesNet
  (implement-component SalesNet
    '() ;; private-cells
    (list p-process-sale)))
