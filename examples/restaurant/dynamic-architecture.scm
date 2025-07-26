(define-module (examples restaurant dynamic-architecture)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (examples restaurant detail sales)
  #:use-module (examples restaurant detail inventory)
  #:use-module (examples restaurant detail recipe-book)
  #:use-module (examples restaurant detail forecasting)
  #:export (TheRestaurantSystem
            sale-event
            new-recipe-event
            inventory-cells))

(define TheRestaurantSystem (make-system))

(define sales-cells (make-sales-component TheRestaurantSystem "sales"))
(define inventory-cells (make-inventory-component TheRestaurantSystem "inventory"))
(define recipe-book-cells (make-recipe-book-component TheRestaurantSystem "recipe-book"))
(define forecasting-cells (make-forecasting-component TheRestaurantSystem "forecasting"))

(define sale-event (hash-ref sales-cells 'sale-event))
(define new-recipe-event (hash-ref recipe-book-cells 'in-new-recipe))

(let ((connection-propagators
       (list
        (make-fan-out-propagator 'p-fan-out-sales
                                 (hash-ref sales-cells 'public-dish-sold)
                                 (list (hash-ref recipe-book-cells 'in-dish-name)
                                       (hash-ref forecasting-cells 'in-forecast-dish)))
        (make-connector-propagator 'p-recipes->inventory
                                   (hash-ref recipe-book-cells 'out-ingredients)
                                   (hash-ref inventory-cells 'public-ingredients-decrement))
        (make-propagator 'p-inventory->alert
                         (hash-ref inventory-cells 'out-low-stock-trigger)
                         (hash-ref inventory-cells 'out-low-stock-trigger)
                         (lambda (alerts src-cell)
                           (if alerts
                               (cons #f (append
                                         (map (lambda (alert-data)
                                                (let ((ingredient (car alert-data))
                                                      (stock (cadr alert-data)))
                                                  (make-effect 'display (format #f "\n*** ALERT: Low stock for ~a (~a remaining) ***\n" ingredient stock))))
                                              alerts)
                                         (list (make-effect 'set-value (cons src-cell #f)))))
                               (cons #f '()))))
        (make-propagator 'p-display-forecast
                         (hash-ref forecasting-cells 'out-ingredient-forecast)
                         (hash-ref forecasting-cells 'out-ingredient-forecast)
                         (lambda (forecast src-cell)
                           (if forecast
                               (cons #f (list (make-effect 'display (format #f ">>> FORECAST based on sales history: Need ~a\n" forecast))
                                              (make-effect 'set-value (cons src-cell #f))))
                               (cons #f '())))))))
  (system-add-morphisms TheRestaurantSystem connection-propagators))

(display "Assembled TheRestaurantSystem from component factories and connection logic.\n")
