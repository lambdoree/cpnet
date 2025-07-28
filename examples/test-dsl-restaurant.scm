;;; examples/test-dsl-restaurant.scm
(use-modules (cpnet core)
             (cpnet system)
             (cpnet runtime)
             (cpnet dsl))

;;; --- Category Definitions ---
(define-category sales
  (cells (Cell sale-event #f)
         (Cell public-dish-sold #f))
  (propagators
   ((prop p-extract-dish sale-event -> public-dish-sold)
    (lambda (sale src)
      (cons (if sale (car sale) #f)
            (list (make-effect 'set-value (cons src #f))))))))

(define-category recipe-book
  (cells (Cell in-dish-name    #f)
         (Cell in-new-recipe   #f)
         (Cell out-ingredients #f)
         (Cell internal-recipes
           '(("Pasta" . (("Flour" . 2) ("Tomato" . 3)))
             ("Omlette" . (("Egg" . 2)  ("Cheese" . 1))))))
  (propagators
   ((prop p-add-recipe in-new-recipe -> internal-recipes)
    (lambda (new src)
      (let ((dish (car new)) (ings (cadr new)) (old (cell-value (system-find-cell (cell-system src) 'recipe-book 'internal-recipes))))
        (cons (acons dish ings old)
              (list (make-effect 'set-value (cons src #f)))))))
   ((prop p-get-ingredients in-dish-name -> out-ingredients)
    (lambda (dish src)
      (let* ((recipes (cell-value (system-find-cell (cell-system src) 'recipe-book 'internal-recipes)))
             (pair (assoc dish recipes))
             (ings (and pair (cdr pair))))
        (cons ings
              (list (make-effect 'set-value (cons src #f)))))))))

(define-category inventory
  (cells (Cell public-ingredients-decrement #f)
         (Cell out-low-stock-trigger       #f)
         (Cell internal-inventory
           '(("Tomato" . 50) ("Cheese" . 20) ("Dough" . 10)
             ("Flour" . 30) ("Egg" . 12))))
  (propagators
   ((prop p-update-stock public-ingredients-decrement -> out-low-stock-trigger)
    (lambda (decr src)
      (if (not decr) (cons #f '())
          (let* ((inv0 (cell-value (system-find-cell (cell-system src) 'inventory 'internal-inventory)))
                 (inv1 (let loop ((d decr) (inv inv0))
                         (if (null? d) inv
                             (let* ((item (car d)) (ing (car item)) (amt (cdr item))
                                    (p (assoc ing inv)) (cur (if p (cdr p) 0))
                                    (inv-without-ing (filter (lambda (p) (not (equal? (car p) ing))) inv)))
                               (loop (cdr d) (cons (cons ing (max 0 (- cur amt))) inv-without-ing))))))
                 (alerts (filter (lambda (e) (< (cdr e) 10)) inv1)))
            (cons (if (null? alerts) #f alerts)
                  (list (make-effect 'set-value (cons (system-find-cell (cell-system src) 'inventory 'internal-inventory) inv1))
                        (make-effect 'set-value (cons src #f))))))))))

(define-category forecasting
  (cells (Cell in-forecast-dish        #f)
         (Cell out-ingredient-forecast #f)
         (Cell internal-sales-counts   '()))
  (propagators
   ((prop p-update-forecast in-forecast-dish -> out-ingredient-forecast)
    (lambda (dish src)
      (let* ((counts0 (cell-value (system-find-cell (cell-system src) 'forecasting 'internal-sales-counts)))
             (old-pair (assoc dish counts0)) (old (if old-pair (cdr old-pair) 0))
             (counts1 (cons (cons dish (1+ old)) (filter (lambda (p) (not (equal? (car p) dish))) counts0)))
             (recipe-pair (assoc dish (cell-value (system-find-cell (cell-system src) 'recipe-book 'internal-recipes))))
             (recipe (and recipe-pair (cdr recipe-pair)))
             (forecast (and recipe (map (lambda (pr) (cons (car pr) (* (cdr pr) (1+ old)))) recipe))))
        (cons (if (null? forecast) #f forecast)
              (list (make-effect 'set-value (cons (system-find-cell (cell-system src) 'forecasting 'internal-sales-counts) counts1))
                    (make-effect 'set-value (cons src #f)))))))))

;;; --- Connection & Execution Definitions ---
(define-connections restaurant-connections
  (fan-out     p-fan-out-sales sales.public-dish-sold (recipe-book.in-dish-name forecasting.in-forecast-dish))
  (connector   p-recipes->inventory recipe-book.out-ingredients inventory.public-ingredients-decrement)
  (propagator  p-inventory->alert inventory.out-low-stock-trigger -> inventory.out-low-stock-trigger
               (lambda (alerts src)
                 (if alerts
                     (cons #f (cons (make-effect 'set-value (cons src #f))
                                    (map (lambda (e) (make-effect 'display (format #f "*** ALERT: Low stock for ~a (~a remaining)~%" (car e) (cdr e)))) alerts)))
                     (cons #f '()))))
  (propagator  p-display-forecast forecasting.out-ingredient-forecast -> forecasting.out-ingredient-forecast
               (lambda (fc src)
                 (if fc
                     (cons #f (list (make-effect 'set-value (cons src #f))
                                    (make-effect 'display (format #f ">>> FORECAST: ~a~%" fc))))
                     (cons #f '())))))

(define-execution restaurant-execution
  (trigger recipe-book.in-new-recipe (list "Pizza" `(("Dough" . 3) ("Tomato" . 2) ("Cheese" . 2))))
  (trigger sales.sale-event (cons "Pasta" 1))
  (run)
  (trigger sales.sale-event (cons "Pizza" 1))
  (run)
  (show-state "Final state"))

;;; --- System Assembly ---
(define-cpnet-system RestaurantSys
  (sales)
  (recipe-book)
  (inventory)
  (forecasting)
  (restaurant-connections)
  (restaurant-execution))
