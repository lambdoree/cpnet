(define-module (examples restaurant detail inventory)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet core)
  #:use-module (cpnet detail)
  #:use-module (examples restaurant static-architecture)
  #:export (ImplementedInventoryNet
            stock-pasta
            stock-sauce
            stock-lettuce
            stock-tomato
            stock-dough
            stock-cheese))

;;; --- Private State & Config Cells ---
(define stock-pasta (make-cell 'stock-pasta 100))
(define stock-sauce (make-cell 'stock-sauce 100))
(define stock-lettuce (make-cell 'stock-lettuce 50))
(define stock-tomato (make-cell 'stock-tomato 50))
(define stock-dough (make-cell 'stock-dough 50))
(define stock-cheese (make-cell 'stock-cheese 50))
(define event-pasta (make-cell 'event-pasta #f))
(define event-sauce (make-cell 'event-sauce #f))
(define event-lettuce (make-cell 'event-lettuce #f))
(define event-tomato (make-cell 'event-tomato #f))
(define event-dough (make-cell 'event-dough #f))
(define event-cheese (make-cell 'event-cheese #f))
(define inventory-dirty-flag (make-cell 'inventory-dirty-flag #f))
(define alert-sent-pasta (make-cell 'alert-sent-pasta #f))
(define alert-sent-sauce (make-cell 'alert-sent-sauce #f))
(define alert-sent-lettuce (make-cell 'alert-sent-lettuce #f))
(define alert-sent-tomato (make-cell 'alert-sent-tomato #f))
(define alert-sent-dough (make-cell 'alert-sent-dough #f))
(define alert-sent-cheese (make-cell 'alert-sent-cheese #f))

;; Configuration data now also managed as cells
(define cell-event-cell-map
  (make-cell 'event-cell-map
             `((pasta . ,event-pasta) (sauce . ,event-sauce)
               (lettuce . ,event-lettuce) (tomato . ,event-tomato)
               (dough . ,event-dough) (cheese . ,event-cheese))))

(define cell-all-stock-info
  (make-cell 'all-stock-info
             (list (list 'pasta   stock-pasta   10 alert-sent-pasta)
                   (list 'sauce   stock-sauce   10 alert-sent-sauce)
                   (list 'lettuce stock-lettuce 10 alert-sent-lettuce)
                   (list 'tomato  stock-tomato  10 alert-sent-tomato)
                   (list 'dough   stock-dough   10 alert-sent-dough)
                   (list 'cheese  stock-cheese  10 alert-sent-cheese))))

(define private-cells
  (list stock-pasta stock-sauce stock-lettuce stock-tomato stock-dough stock-cheese
        event-pasta event-sauce event-lettuce event-tomato event-dough event-cheese
        inventory-dirty-flag
        alert-sent-pasta alert-sent-sauce alert-sent-lettuce alert-sent-tomato alert-sent-dough alert-sent-cheese
        cell-event-cell-map cell-all-stock-info))

;;; --- Propagators ---
(define p-dispatch-ingredients
  (make-dispatcher-propagator 'dispatch-ingredients public-ingredients-decrement
			      (lambda (ingredients-list)
				(if (not (null? ingredients-list))
				    (let ((aggregated (make-hash-table)))
				      (for-each
				       (lambda (item)
					 (let ((name (car item)) (amount (cadr item)))
					   (hash-set! aggregated name (+ amount (hash-ref aggregated name 0)))))
				       ingredients-list)
				      (let ((effects '())
					    (current-event-map (cell-value cell-event-cell-map)))
					(hash-for-each
					 (lambda (name amount)
					   (let ((event-cell (cdr (assoc name current-event-map))))
					     (when event-cell
					       (set! effects (cons (make-effect 'set-value (cons event-cell amount))
								   effects)))))
					 aggregated)
					effects))
				    '()))))

(define (make-stock-updater ingredient-name event-cell stock-cell)
  (make-event-propagator
   (string->symbol (format #f "update-~a-stock" ingredient-name))
   event-cell stock-cell
   (lambda (amount)
     (if (and amount (number? amount))
         (let ((new-stock (- (cell-value stock-cell) amount)))
           (cons new-stock (list (make-effect 'set-value (cons inventory-dirty-flag #t)))))
         (cons #f '())))))

(define p-check-all-stock-levels
  (make-event-propagator 'check-all-stock-levels
			 inventory-dirty-flag out-low-stock-trigger
			 (lambda (is-dirty)
			   (if is-dirty
			       (let* ((current-stock-info (cell-value cell-all-stock-info))
				      (items-and-effects
				       (filter-map
					(lambda (info)
					  (let ((name       (list-ref info 0))
						(stock-cell (list-ref info 1))
						(threshold  (list-ref info 2))
						(alert-cell (list-ref info 3)))
					    (if (and (<= (cell-value stock-cell) threshold)
						     (not (cell-value alert-cell)))
						(list
						 (list name (cell-value stock-cell))
						 (make-effect 'set-value (cons alert-cell #t)))
						#f)))
					current-stock-info))
				      (low-stock-items   (map car items-and-effects))
				      (set-alert-effects (map cadr items-and-effects)))
				 (cons (if (null? low-stock-items) #f low-stock-items)
				       set-alert-effects))
			       (cons #f '())))))

;;; --- Component Implementation ---
(define ImplementedInventoryNet
  (let ((inventory-propagators
         (list p-dispatch-ingredients
               p-check-all-stock-levels
               (make-stock-updater 'pasta event-pasta stock-pasta)
               (make-stock-updater 'sauce event-sauce stock-sauce)
               (make-stock-updater 'lettuce event-lettuce stock-lettuce)
               (make-stock-updater 'tomato event-tomato stock-tomato)
               (make-stock-updater 'dough event-dough stock-dough)
               (make-stock-updater 'cheese event-cheese stock-cheese))))
    (implement-component InventoryNet private-cells inventory-propagators)))
