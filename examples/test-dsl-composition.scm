(use-modules (cpnet core)
             (cpnet system)
             (cpnet runtime)
             (cpnet dsl))

(define-cpnet-system SupplierSys
  (category procurement
	    (cells (Cell order_in #f)
		   (Cell materials_out #f)
		   (Cell warehouse_request #f)))
  (category warehouse
	    (cells (Cell request_in #f)
		   (Cell materials_ready #f)
		   (Cell stock '(("wood" . 1000)))))
  (connections
   (propagator p-procure-order procurement.order_in -> procurement.warehouse_request
	       (lambda (order src) (if order (cons order (list (make-effect 'set-value (cons src #f)))) (cons #f '()))))
   (connector c-supp-internal1 procurement.warehouse_request warehouse.request_in)
   (propagator p-prepare-materials warehouse.request_in -> warehouse.materials_ready
	       (lambda (req src)
		 (if req
		     (let* ((material (car req)) (qty (cdr req))
			    (current-stock (cell-value (hash-ref warehouse-cells 'stock)))
			    (stock-pair (assoc material current-stock))
			    (available (if stock-pair (cdr stock-pair) 0)))
		       (if (>= available qty)
			   (let ((new-stock (cons (cons material (- available qty))
						  (filter (lambda (p) (not (equal? (car p) material))) current-stock))))
			     (cons req (list (make-effect 'set-value (cons (hash-ref warehouse-cells 'stock) new-stock))
					     (make-effect 'display (format #f "SUPPLIER: Preparing ~a ~a~%" qty material))
					     (make-effect 'set-value (cons src #f)))))
			   (cons #f (list (make-effect 'display (format #f "SUPPLIER: Not enough ~a in stock~%" material))
					  (make-effect 'set-value (cons src #f))))))
		     (cons #f '()))))
   (connector c-supp-internal2 warehouse.materials_ready procurement.materials_out)))

(define-cpnet-system FactorySys
  (category manufacturing
	    (cells (Cell materials_in #f)
		   (Cell product_out #f)
		   (Cell to_assembly #f)
		   (Cell from_painting #f)
		   (Cell recipes '(("chair" . (("wood" . 5)))))))
  (category assembly
	    (cells (Cell materials_in #f)
		   (Cell assembled_product_out #f)))
  (category painting
	    (cells (Cell unpainted_in #f)
		   (Cell painted_out #f)))
  (connections
   (propagator p-send-to-assembly manufacturing.materials_in -> manufacturing.to_assembly
	       (lambda (materials src) (cons materials (list (make-effect 'set-value (cons src #f))))))
   (connector c-fact-internal1 manufacturing.to_assembly assembly.materials_in)
   (propagator p-assemble assembly.materials_in -> assembly.assembled_product_out
	       (lambda (materials src)
		 (if materials
		     (let* ((material-name (car materials)) (material-qty (cdr materials))
			    (recipes (cell-value (hash-ref manufacturing-cells 'recipes)))
			    (recipe-pair (assoc "chair" recipes)) (recipe-ings (cdr recipe-pair))
			    (wood-needed-pair (assoc "wood" recipe-ings)) (wood-per-chair (cdr wood-needed-pair)))
		       (if (and (equal? material-name "wood") (> wood-per-chair 0))
			   (let ((chairs-produced (floor (/ material-qty wood-per-chair))))
			     (cons `("unpainted_chair" . ,chairs-produced)
				   (list (make-effect 'display (format #f "FACTORY: Assembled ~a chairs~%" chairs-produced))
					 (make-effect 'set-value (cons src #f)))))
			   (cons #f (list (make-effect 'set-value (cons src #f))))))
		     (cons #f '()))))
   (connector c-fact-internal2 assembly.assembled_product_out painting.unpainted_in)
   (propagator p-paint painting.unpainted_in -> painting.painted_out
	       (lambda (product src)
		 (if product
		     (let ((qty (cdr product)))
		       (cons `("chair" . ,qty)
			     (list (make-effect 'display (format #f "FACTORY: Painted ~a chairs~%" qty))
				   (make-effect 'set-value (cons src #f)))))
		     (cons #f '()))))
   (connector c-fact-internal3 painting.painted_out manufacturing.from_painting)
   (propagator p-receive-from-painting manufacturing.from_painting -> manufacturing.product_out
	       (lambda (product src) (cons product (list (make-effect 'set-value (cons src #f))))))))

(define-cpnet-system LogisticsSys
  (category shipping
	    (cells (Cell shipment_in #f)
		   (Cell delivery_out #f)
		   (Cell truck_request #f)
		   (Cell truck_assigned #f)))
  (category fleet_management
	    (cells (Cell assignment_request #f)
		   (Cell assignment_confirm #f)
		   (Cell trucks '(("truck1" . "available")))))
  (connections
   (propagator p-request-truck shipping.shipment_in -> shipping.truck_request
	       (lambda (shipment src) (if shipment (cons shipment (list (make-effect 'set-value (cons src #f)))) (cons #f '()))))
   (connector c-log-internal1 shipping.truck_request fleet_management.assignment_request)
   (propagator p-assign-truck fleet_management.assignment_request -> fleet_management.assignment_confirm
	       (lambda (req src) (if req (cons req (list (make-effect 'display (format #f "LOGISTICS: Truck assigned.~%"))
							 (make-effect 'set-value (cons src #f)))) (cons #f '()))))
   (connector c-log-internal2 fleet_management.assignment_confirm shipping.truck_assigned)
   (propagator p-ship-with-truck shipping.truck_assigned -> shipping.delivery_out
	       (lambda (shipment src)
		 (if shipment
		     (cons shipment (list (make-effect 'display (format #f "LOGISTICS: Delivering ~a ~a~%" (cdr shipment) (car shipment)))
					  (make-effect 'set-value (cons src #f))))
		     (cons #f '()))))))

(define-cpnet-system StoreSys
  (category sales
	    (cells (Cell customer_order_in #f)
		   (Cell delivery_in #f)
		   (Cell material_order_out #f)
		   (Cell discount_info #f)
		   (Cell inventory '(("chair" . 10)))
		   (Cell reorder_threshold 5)))
  (category promotions
	    (cells (Cell active_promo #f)
		   (Cell promo_status_out #f)))
  (connections
   (propagator p-process-sale sales.customer_order_in -> sales.material_order_out
	       (lambda (order src)
		 (if (not order) (cons #f '())
		     (let* ((product (car order)) (qty (cdr order))
			    (inv (cell-value (hash-ref sales-cells 'inventory)))
			    (stock-pair (assoc product inv))
			    (current-stock (if stock-pair (cdr stock-pair) 0)))
		       (if (>= current-stock qty)
			   (let* ((new-stock (- current-stock qty))
				  (new-inv (cons (cons product new-stock) (filter (lambda (p) (not (equal? (car p) product))) inv)))
				  (threshold (cell-value (hash-ref sales-cells 'reorder_threshold)))
				  (reorder-request #f))
			     (when (and (<= new-stock threshold) (> current-stock threshold))
			       (set! reorder-request '("wood" . 50)))
			     (cons reorder-request
				   (list (make-effect 'display (format #f "STORE: Sold ~a ~a. Stock is now ~a~%" qty product new-stock))
					 (make-effect 'set-value (cons (hash-ref sales-cells 'inventory) new-inv))
					 (make-effect 'set-value (cons src #f)))))
			   (cons #f (list (make-effect 'display (format #f "STORE: ~a is out of stock!~%" product))
					  (make-effect 'set-value (cons src #f)))))))))
   (propagator p-restock sales.delivery_in -> sales.inventory
	       (lambda (delivery src)
		 (if (not delivery) (cons (cell-value (hash-ref sales-cells 'inventory)) '())
		     (let* ((product (car delivery)) (qty (cdr delivery))
			    (inv (cell-value (hash-ref sales-cells 'inventory)))
			    (stock-pair (assoc product inv))
			    (current-stock (if stock-pair (cdr stock-pair) 0)))
		       (let ((new-inv (cons (cons product (+ current-stock qty)) (filter (lambda (p) (not (equal? (car p) product))) inv))))
			 (cons new-inv (list (make-effect 'display (format #f "STORE: Restocked ~a ~a. Stock is now ~a~%" qty product (+ current-stock qty)))
					     (make-effect 'set-value (cons src #f)))))))))
   (propagator p-check-promo promotions.active_promo -> promotions.promo_status_out
	       (lambda (v src) (if v (cons v (list (make-effect 'display (format #f "STORE: Promo '~a' is now active!~%" v))
						   (make-effect 'set-value (cons src #f)))) (cons #f '()))))
   (connector c-store-internal promotions.promo_status_out sales.discount_info)))

(compose-systems SupplyChainSys
		 (systems SupplierSys FactorySys LogisticsSys StoreSys)
		 (connections
		  (connector c1 StoreSys.sales.material_order_out SupplierSys.procurement.order_in)
		  (connector c2 SupplierSys.procurement.materials_out FactorySys.manufacturing.materials_in)
		  (connector c3 FactorySys.manufacturing.product_out LogisticsSys.shipping.shipment_in)
		  (connector c4 LogisticsSys.shipping.delivery_out StoreSys.sales.delivery_in))
		 (execution
		  (show-state "Initial State")
		  (display "\n--- Customer buying chairs ---\n")
		  (trigger StoreSys.sales.customer_order_in '("chair" . 1)) (run)
		  (trigger StoreSys.sales.customer_order_in '("chair" . 1)) (run)
		  (trigger StoreSys.sales.customer_order_in '("chair" . 1)) (run)
		  (trigger StoreSys.sales.customer_order_in '("chair" . 1)) (run)
		  (display "\n--- This purchase will trigger a reorder and restock cycle ---\n")
		  (trigger StoreSys.sales.customer_order_in '("chair" . 1)) (run)
		  (display "\n--- Activating a promotion ---\n")
		  (trigger StoreSys.promotions.active_promo "SUMMER SALE") (run)
		  (display "\n--- One final purchase ---\n")
		  (trigger StoreSys.sales.customer_order_in '("chair" . 1)) (run)
		  (show-state "Final State")))
