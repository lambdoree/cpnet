(define-module (examples restaurant detail forecasting)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet core)
  #:use-module (cpnet detail)
  #:use-module (examples restaurant static-architecture)
  #:export (ImplementedForecastingNet))

;;; --- Private State Cells ---
(define cell-sales-history
  (make-cell 'sales-history (make-hash-table)))

(define cell-forecast-recipes
  (make-cell 'forecast-recipes
             '((spaghetti ((pasta 1) (sauce 1)))
               (salad ((lettuce 1) (tomato 1)))
               (pizza ((dough 1) (cheese 1) (sauce 1))))))

;;; --- Propagators ---
(define p-update-sales-history
  (make-event-propagator 'update-sales-history in-forecast-dish cell-sales-history
    (lambda (dishes)
      (if (list? dishes)
          (let ((new-history (make-hash-table)))
            ;; To avoid mutation, copy old hash table and update it
            (hash-for-each
             (lambda (k v) (hash-set! new-history k v))
             (cell-value cell-sales-history))
            (for-each
             (lambda (dish)
               (hash-set! new-history dish (+ 1 (hash-ref new-history dish 0))))
             dishes)
            (cons new-history '()))
        (cons #f '())))))

(define p-generate-forecast
  (make-propagator 'generate-forecast cell-sales-history out-ingredient-forecast
    (lambda (history _)
      (if (hash-table? history)
          (let* ((most-sold-item
                  (let ((max-count -1) (top-item #f))
                    (hash-for-each
                     (lambda (item count)
                       (when (> count max-count)
                         (set! max-count count)
                         (set! top-item item)))
                     history)
                    top-item))
                 (recipe (assoc most-sold-item (cell-value cell-forecast-recipes)))
                 (forecast (if recipe (cdr recipe) '())))
            (cons forecast '()))
        (cons #f '())))))

;;; --- Component Implementation ---
(define ImplementedForecastingNet
  (implement-component ForecastingNet
    (list cell-sales-history cell-forecast-recipes)
    (list p-update-sales-history p-generate-forecast)))
