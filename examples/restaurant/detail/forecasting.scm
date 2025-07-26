(define-module (examples restaurant detail forecasting)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet detail)
  #:export (make-forecasting-component))

(define (define-forecasting-propagators cells prop-id)
  (let ((in-forecast-dish (hash-ref cells 'in-forecast-dish))
        (out-ingredient-forecast (hash-ref cells 'out-ingredient-forecast))
        (sales-count-spaghetti (hash-ref cells 'sales-count-spaghetti))
        (sales-count-salad (hash-ref cells 'sales-count-salad))
        (sales-count-pizza (hash-ref cells 'sales-count-pizza))
        (most-sold-item (hash-ref cells 'most-sold-item))
        (history-dirty-flag (hash-ref cells 'history-dirty-flag))
        (forecast-recipes (hash-ref cells 'forecast-recipes)))
    (list
     (make-propagator (prop-id "dispatch-sales") in-forecast-dish history-dirty-flag
                      (lambda (dishes src-cell)
                        (if (and dishes (not (null? dishes)))
                            (let* ((counts (make-hash-table))
                                   (effects '()))
                              (for-each
                               (lambda (dish)
                                 (hash-set! counts dish (+ 1 (hash-ref counts dish 0))))
                               dishes)
                              (hash-for-each
                               (lambda (dish num)
                                 (let ((count-cell
                                        (case dish
                                          ((spaghetti) sales-count-spaghetti)
                                          ((salad) sales-count-salad)
                                          ((pizza) sales-count-pizza)
                                          (else #f))))
                                   (when count-cell
                                     (let ((new-count (+ num (cell-value count-cell))))
                                       (set! effects (cons (make-effect 'set-value (cons count-cell new-count))
                                                           effects))))))
                               counts)
                              (cons #t (append effects (list (make-effect 'set-value (cons src-cell #f))))))
                            (cons #f '()))))
     (make-propagator (prop-id "find-max-sale") history-dirty-flag most-sold-item
                      (lambda (is-dirty src-cell)
                        (if is-dirty
                            (let ((counts `((spaghetti . ,(cell-value sales-count-spaghetti))
                                            (salad . ,(cell-value sales-count-salad))
                                            (pizza . ,(cell-value sales-count-pizza))))
                                  (max-count -1)
                                  (top-item #f))
                              (for-each
                               (lambda (item-count)
                                 (let ((item (car item-count))
                                       (count (cdr item-count)))
                                   (when (> count max-count)
                                     (set! max-count count)
                                     (set! top-item item))))
                               counts)
                              (cons top-item (list (make-effect 'set-value (cons src-cell #f))))))
                            (cons #f '()))))
     (make-propagator (prop-id "lookup-forecast") most-sold-item out-ingredient-forecast
                      (lambda (item-name src-cell)
                        (if item-name
                            (let* ((recipes (cell-value forecast-recipes))
                                   (recipe (assoc item-name recipes))
                                   (forecast (if recipe (cdr recipe) '())))
                              (cons forecast (list (make-effect 'set-value (cons src-cell #f)))))
                            (cons #f '()))))))

(define make-forecasting-component
  (make-component-factory
   '((in-forecast-dish #f) (out-ingredient-forecast #f))
   (list
    '(sales-count-spaghetti 0)
    '(sales-count-salad 0)
    '(sales-count-pizza 0)
    '(most-sold-item #f)
    '(history-dirty-flag #f)
    (list 'forecast-recipes
          '((spaghetti ((pasta 1) (sauce 1)))
            (salad ((lettuce 1) (tomato 1)))
            (pizza ((dough 1) (cheese 1) (sauce 1))))))
   define-forecasting-propagators))
