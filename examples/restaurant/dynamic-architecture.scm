(define-module (examples restaurant dynamic-architecture)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:use-module (cpnet category)
  #:use-module (cpnet architecture)
  #:use-module (cpnet detail)
  #:use-module (examples restaurant static-architecture)
  #:use-module (examples restaurant detail sales)
  #:use-module (examples restaurant detail inventory)
  #:use-module (examples restaurant detail recipe-book)
  #:use-module (examples restaurant detail forecasting)
  #:export (SystemNet))

(display "\n--- System Integration ---\n")

(define p-fan-out-sales
  (make-fan-out-propagator 'fan-out-sales
			   public-dish-sold (list in-dish-name in-forecast-dish)))

(define p-recipes->inventory
  (make-connector-propagator 'recipes->inventory
			     out-ingredients public-ingredients-decrement))

(define p-inventory->alert
  (make-dispatcher-propagator 'inventory->alert out-low-stock-trigger
			      (lambda (alerts)
				(if (not (null? alerts))
				    (append-map
				     (lambda (alert-data)
				       (let ((ingredient (car alert-data))
					     (stock (cadr alert-data)))
					 (list (make-effect 'display (format #f "\n*** ALERT: Low stock for ~a (~a remaining) ***\n" ingredient stock)))))
				     alerts)
				    '()))))

(define p-new-recipe->recipe-book
  (make-connector-propagator 'new-recipe->recipe-book
			     new-recipe-event in-new-recipe))

(define implemented-components
  (list ImplementedSalesNet
        ImplementedInventoryNet
        ImplementedRecipeBookNet
        ImplementedForecastingNet))

(define connection-propagators
  (list p-fan-out-sales
        p-recipes->inventory
        p-inventory->alert
        p-new-recipe->recipe-book))

(define SystemNet
  (assemble-system implemented-components connection-propagators))
(display "Assembled SystemNet from detail components and connection logic.\n")
