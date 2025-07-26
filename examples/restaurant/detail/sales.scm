(define-module (examples restaurant detail sales)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet detail)
  #:export (make-sales-component))

(define (define-sales-propagators cells prop-id)
  (let ((sale-event (hash-ref cells 'sale-event))
        (public-dish-sold (hash-ref cells 'public-dish-sold)))
    (list
     (make-propagator (prop-id "process-sale") sale-event public-dish-sold
                      (lambda (events src-cell)
                        (if (and events (list? events))
                            (let* ((results
                                    (map (lambda (event)
                                           (if (and (list? event) (assoc 'item event) (assoc 'quantity event))
                                               (let* ((item (cdr (assoc 'item event)))
                                                      (quantity (cdr (assoc 'quantity event))))
                                                 (cons (make-list quantity item) '()))
                                               (cons '() (list (make-effect 'display "WARN: Invalid sale event format in list\n")))))
                                         events))
                                   (all-dishes (apply append (map car results)))
                                   (all-effects (apply append (map cdr results))))
                              (cons all-dishes (append all-effects (list (make-effect 'set-value (cons src-cell #f))))))
                            (cons #f '())))))))

(define make-sales-component
  (make-component-factory
   '((sale-event #f) (public-dish-sold #f))
   '()
   define-sales-propagators))
