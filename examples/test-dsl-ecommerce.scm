
;; 1) 필요한 모듈을 한 번만 로드
(use-modules (cpnet core)
             (cpnet system)
             (cpnet runtime)
             (cpnet dsl))

;;; --- Category Definitions ---
(define-category orders
  (cells (Cell new-order #f)
         (Cell order-for-inventory-check #f))
  (propagators
   ((prop p-process-order new-order -> order-for-inventory-check)
    (lambda (order src)
      (cons order (list (make-effect 'set-value (cons src #f))))))))

(define-category inventory
  (cells (Cell check-stock-request #f)
         (Cell order-to-ship #f)
         (Cell backorder-alert #f)
         (Cell internal-stock
           '(("laptop" . 10) ("mouse" . 30) ("keyboard" . 5))))
  (propagators
   ((prop p-check-stock check-stock-request -> order-to-ship)
    (lambda (req src)
      (if (not req) (cons #f '())
          (let* ((product (car req)) (quantity (cdr req))
                 (stock-list (cell-value (system-find-cell (cell-system src) 'inventory 'internal-stock)))
                 (stock-pair (assoc product stock-list))
                 (current-stock (if stock-pair (cdr stock-pair) 0)))
            (if (>= current-stock quantity)
                (let* ((new-stock-list (cons (cons product (- current-stock quantity))
                                             (filter (lambda (p) (not (equal? (car p) product))) stock-list))))
                  (cons req (list (make-effect 'set-value (cons (system-find-cell (cell-system src) 'inventory 'internal-stock) new-stock-list))
                                  (make-effect 'set-value (cons src #f)))))
                (cons #f (list (make-effect 'set-value (cons (system-find-cell (cell-system src) 'inventory 'backorder-alert) product))
                               (make-effect 'set-value (cons src #f)))))))))))

(define-category shipping
  (cells (Cell ship-request #f)
         (Cell shipped-log '()))
  (propagators
   ((prop p-ship-order ship-request -> shipped-log)
    (lambda (req src)
      (if req
          (let* ((log (cell-value (system-find-cell (cell-system src) 'shipping 'shipped-log)))
                 (new-log (cons req log)))
            (cons new-log (list (make-effect 'display (format #f "SHIPPED: ~a~%" req))
                                (make-effect 'set-value (cons src #f)))))
          (cons #f '()))))))

;;; --- Connection & Execution Definitions ---
(define-connections ecommerce-connections
  (connector p-order->inventory orders.order-for-inventory-check inventory.check-stock-request)
  (connector p-inventory->shipping inventory.order-to-ship shipping.ship-request)
  (propagator p-alert-display inventory.backorder-alert -> inventory.backorder-alert
              (lambda (product src)
                (if product
                    (cons #f
                          (list (make-effect 'set-value (cons src #f))
                                (make-effect 'display (format #f "!!! BACKORDER ALERT: ~a is out of stock!~%" product))))
                    (cons #f '())))))

(define-execution ecommerce-execution
  (trigger orders.new-order (cons "mouse" 5))
  (run)
  (trigger orders.new-order (cons "keyboard" 10))
  (run)
  (show-state "Final state"))

;;; --- System Assembly ---
(define-cpnet-system EcommerceSys
  (orders)
  (inventory)
  (shipping)
  (ecommerce-connections)
  (ecommerce-execution))
