(use-modules (cpnet core)
             (cpnet system)
             (cpnet runtime)
             (cpnet dsl))

;;; --- Component Definitions ---

(define-category supplier-procurement
  (cells (Cell order_in #f)
	 (Cell materials_out #f) 
	 (Cell warehouse_request #f)))

(define-category supplier-warehouse
  (cells (Cell request_in #f)
	 (Cell materials_ready #f)
	 (Cell stock '(("wood" . 1000)))))

(define-connections supplier-connections
  (propagator p-procure-order supplier-procurement.order_in -> supplier-procurement.warehouse_request
	      (lambda (order src)
		(if order
		    (cons order (list (make-effect 'set-value (cons src #f))))
		    (cons #f '()))))
  (connector c-supp-internal1 supplier-procurement.warehouse_request supplier-warehouse.request_in)
  (propagator p-prepare-materials supplier-warehouse.request_in -> supplier-warehouse.materials_ready
	      (lambda (req src)
		(if req
		    (let* ((material (car req))
			   (qty (cdr req))
			   (current-stock (cell-value (system-find-cell (cell-system src) 'supplier-warehouse 'stock)))
			   (stock-pair (assoc material current-stock))
			   (available (if stock-pair
					  (cdr stock-pair)
					  0)))
		      (if (>= available qty)
			  (let ((new-stock (cons (cons material (- available qty))
						 (filter (lambda (p) (not (equal? (car p) material)))
							 current-stock))))
			    (cons req (list (make-effect 'set-value (cons (system-find-cell (cell-system src) 'supplier-warehouse 'stock) new-stock))
					    (make-effect 'display (format #f "SUPPLIER: Preparing ~a ~a~%" qty material))
					    (make-effect 'set-value (cons src #f)))))
			  (cons #f (list (make-effect 'display (format #f "SUPPLIER: Not enough ~a in stock~%" material))
					 (make-effect 'set-value (cons src #f))))))
		    (cons #f '()))))
  (connector c-supp-internal2 supplier-warehouse.materials_ready supplier-procurement.materials_out))

(define-category factory-manufacturing
  (cells (Cell materials_in #f)
	 (Cell product_out #f)
	 (Cell to_assembly #f)
	 (Cell from_painting #f)
	 (Cell recipes '(("chair" . (("wood" . 5)))))))

(define-category factory-assembly
  (cells (Cell materials_in #f)
	 (Cell assembled_product_out #f)))

(define-category factory-painting
  (cells (Cell unpainted_in #f)
	 (Cell painted_out #f)))

(define-connections factory-connections
  (propagator p-send-to-assembly factory-manufacturing.materials_in -> factory-manufacturing.to_assembly
	      (lambda (materials src)
		(cons materials
		      (list (make-effect 'set-value (cons src #f))))))

  (connector c-fact-internal1 factory-manufacturing.to_assembly factory-assembly.materials_in)

  (propagator p-assemble factory-assembly.materials_in -> factory-assembly.assembled_product_out
	      (lambda (materials src)
		(if materials
		    (let* ((material-name (car materials))
			   (material-qty (cdr materials))
			   (recipes (cell-value (system-find-cell (cell-system src) 'factory-manufacturing 'recipes)))
			   (recipe-pair (assoc "chair" recipes))
			   (recipe-ings (cdr recipe-pair))
			   (wood-needed-pair (assoc "wood" recipe-ings))
			   (wood-per-chair (cdr wood-needed-pair)))
		      (if (and (equal? material-name "wood") (> wood-per-chair 0))
			  (let ((chairs-produced (floor (/ material-qty wood-per-chair))))
			    (cons `("unpainted_chair" . ,chairs-produced)
				  (list (make-effect 'display (format #f "FACTORY: Assembled ~a chairs~%" chairs-produced))
					(make-effect 'set-value (cons src #f)))))
			  (cons #f (list (make-effect 'set-value (cons src #f))))))
		    (cons #f '()))))
  (connector c-fact-internal2 factory-assembly.assembled_product_out factory-painting.unpainted_in)

  (propagator p-paint factory-painting.unpainted_in -> factory-painting.painted_out
	      (lambda (product src)
		(if product
		    (let ((qty (cdr product)))
		      (cons `("chair" . ,qty)
			    (list (make-effect 'display (format #f "FACTORY: Painted ~a chairs~%" qty))
				  (make-effect 'set-value (cons src #f)))))
		    (cons #f '()))))
  
  (connector c-fact-internal3 factory-painting.painted_out factory-manufacturing.from_painting)

  (propagator p-receive-from-painting factory-manufacturing.from_painting -> factory-manufacturing.product_out
	      (lambda (product src)
		(cons product (list (make-effect 'set-value (cons src #f)))))))

(define-category logistics-shipping
  (cells (Cell shipment_in #f)
	 (Cell delivery_out #f)
	 (Cell truck_request #f)
	 (Cell truck_assigned #f)))

(define-category logistics-fleet
  (cells (Cell assignment_request #f)
	 (Cell assignment_confirm #f)
	 (Cell trucks '(("truck1" . "available")))))

(define-connections logistics-connections
  (propagator p-request-truck logistics-shipping.shipment_in -> logistics-shipping.truck_request
	      (lambda (shipment src)
		(if shipment
		    (cons shipment (list (make-effect 'set-value (cons src #f))))
		    (cons #f '()))))

  (connector c-log-internal1 logistics-shipping.truck_request logistics-fleet.assignment_request)

  (propagator p-assign-truck logistics-fleet.assignment_request -> logistics-fleet.assignment_confirm
	      (lambda (req src)
		(if req
		    (cons req
			  (list (make-effect 'display (format #f "LOGISTICS: Truck assigned.~%"))
				(make-effect 'set-value (cons src #f)))) (cons #f '()))))

  (connector c-log-internal2 logistics-fleet.assignment_confirm logistics-shipping.truck_assigned)

  (propagator p-ship-with-truck logistics-shipping.truck_assigned -> logistics-shipping.delivery_out
	      (lambda (shipment src)
		(if shipment
		    (cons shipment
			  (list (make-effect 'display (format #f "LOGISTICS: Delivering ~a ~a~%" (cdr shipment) (car shipment)))
				(make-effect 'set-value (cons src #f)))) (cons #f '())))))

(define-category store-sales
  (cells (Cell customer_order_in #f)
	 (Cell delivery_in #f)
	 (Cell material_order_out #f)
	 (Cell discount_info #f)
	 (Cell inventory '(("chair" . 10)))
	 (Cell reorder_threshold 5)))

(define-category store-promotions
  (cells (Cell active_promo #f)
	 (Cell promo_status_out #f)))

(define-connections store-connections
  (propagator p-process-sale store-sales.customer_order_in -> store-sales.material_order_out
	      (lambda (order src)
		(if (not order) (cons #f '())
		    (let* ((product (car order))
			   (qty (cdr order))
			   (inv (cell-value (system-find-cell (cell-system src) 'store-sales 'inventory)))
			   (stock-pair (assoc product inv))
			   (current-stock (if stock-pair (cdr stock-pair) 0)))
		      (if (>= current-stock qty)
			  (let* ((new-stock (- current-stock qty))
				 (new-inv (cons (cons product new-stock)
						(filter (lambda (p) (not (equal? (car p) product))) inv)))
				 (threshold (cell-value (system-find-cell (cell-system src) 'store-sales 'reorder_threshold)))
				 (reorder-request (if (and (<= new-stock threshold) (> current-stock threshold))
						      '("wood" . 50)
						      #f)))
			    (cons reorder-request (list (make-effect 'display (format #f "STORE: Sold ~a ~a. Stock is now ~a~%" qty product new-stock))
							(make-effect 'set-value (cons (system-find-cell (cell-system src) 'store-sales 'inventory) new-inv))
							(make-effect 'set-value (cons src #f)))))
			  (cons #f (list (make-effect 'display (format #f "STORE: ~a is out of stock!~%" product))
					 (make-effect 'set-value (cons src #f)))))))))

  (propagator p-restock store-sales.delivery_in -> store-sales.inventory
	      (lambda (delivery src)
		(if (not delivery)
		    (cons (cell-value (system-find-cell (cell-system src) 'store-sales 'inventory)) '())
		    (let* ((product (car delivery))
			   (qty (cdr delivery))
			   (inv (cell-value (system-find-cell (cell-system src) 'store-sales 'inventory)))
			   (stock-pair (assoc product inv)) (current-stock (if stock-pair (cdr stock-pair) 0)))
		      (let ((new-inv (cons (cons product (+ current-stock qty))
					   (filter (lambda (p) (not (equal? (car p) product))) inv))))
			(cons new-inv
			      (list (make-effect 'display (format #f "STORE: Restocked ~a ~a. Stock is now ~a~%" qty product (+ current-stock qty)))
				    (make-effect 'set-value (cons src #f)))))))))

  (propagator p-check-promo store-promotions.active_promo -> store-promotions.promo_status_out
	      (lambda (v src)
		(if v (cons v (list (make-effect 'display (format #f "STORE: Promo '~a' is now active!~%" v))
				    (make-effect 'set-value (cons src #f)))) (cons #f '()))))

  (connector c-store-internal store-promotions.promo_status_out store-sales.discount_info))

;;; --- System Assembly ---
(define-cpnet-system SupplierSys
  (supplier-procurement)
  (supplier-warehouse)
  (supplier-connections))

(define-cpnet-system FactorySys
  (factory-manufacturing)
  (factory-assembly)
  (factory-painting)
  (factory-connections))

(define-cpnet-system LogisticsSys
  (logistics-shipping)
  (logistics-fleet)
  (logistics-connections))

(define-cpnet-system StoreSys
  (store-sales)
  (store-promotions)
  (store-connections))

;;; --- System Composition & Execution ---
(define-connections supply-chain-connections
  (connector c1 StoreSys.store-sales.material_order_out SupplierSys.supplier-procurement.order_in)
  (connector c2 SupplierSys.supplier-procurement.materials_out FactorySys.factory-manufacturing.materials_in)
  (connector c3 FactorySys.factory-manufacturing.product_out LogisticsSys.logistics-shipping.shipment_in)
  (connector c4 LogisticsSys.logistics-shipping.delivery_out StoreSys.store-sales.delivery_in))

(define-execution supply-chain-execution
  (show-state "Initial State")
  (display "\n--- Customer buying chairs ---\n")
  (trigger StoreSys.store-sales.customer_order_in '("chair" . 1)) (run)
  (trigger StoreSys.store-sales.customer_order_in '("chair" . 1)) (run)
  (trigger StoreSys.store-sales.customer_order_in '("chair" . 1)) (run)
  (trigger StoreSys.store-sales.customer_order_in '("chair" . 1)) (run)
  (display "\n--- This purchase will trigger a reorder and restock cycle ---\n")
  (trigger StoreSys.store-sales.customer_order_in '("chair" . 1)) (run)
  (display "\n--- Activating a promotion ---\n")
  (trigger StoreSys.store-promotions.active_promo "SUMMER SALE") (run)
  (display "\n--- One final purchase ---\n")
  (trigger StoreSys.store-sales.customer_order_in '("chair" . 1)) (run)
  (show-state "Final State"))

(compose-systems SupplyChainSys
		 (systems SupplierSys FactorySys LogisticsSys StoreSys)
		 (connections supply-chain-connections)
		 (execution supply-chain-execution))
