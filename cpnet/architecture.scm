(define-module (cpnet architecture)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet core)
  #:use-module (cpnet category)
  #:export (make-component-interface
            interface-net
            cell-ref
            assemble-system
            make-connector-propagator
            make-fan-out-propagator))

;;; --- static ---

(define-record-type <component-interface>
  (make-component-interface-record net cells-hash)
  component-interface?
  (net interface-net)
  (cells-hash interface-cells-hash))

(define (cell-ref interface symbol)
  (hash-ref (interface-cells-hash interface) symbol))

(define (make-component-interface cell-names)
  (let ((cells-hash (make-hash-table)))
    (for-each (lambda (name)
                (hash-set! cells-hash name (make-cell name #f)))
              cell-names)
    (let* ((cells (hash-map->list (lambda (k v) v) cells-hash))
           (net (make-cpnet-category cells '())))
      (make-component-interface-record net cells-hash))))


;;; --- dynamic ---

(define (assemble-system component-nets connection-propagators)
  (let* ((all-objects
          (append-map category-objects component-nets))
         (all-morphisms
          (append-map category-morphisms component-nets))
         (system-net
          (make-cpnet-category all-objects all-morphisms)))
    (for-each
     (lambda (p) (category-add-morphism system-net p))
     connection-propagators)
    (category-validate system-net)
    system-net))


;;; --- dynamic interface ---

(define (make-connector-propagator id src tgt . maybe-map-fn)
  (let ((map-fn (if (null? maybe-map-fn)
                    (lambda (v) v)
                    (car maybe-map-fn))))
    (make-propagator id src tgt
		     (lambda (val src-cell)
		       (cons (map-fn val)
			     (list (make-effect 'set-value (cons src-cell #f))))))))

(define (make-fan-out-propagator id src tgt-cells)
  (make-propagator id src src
		   (lambda (val src-cell)
		     (let ((effects
			    (map (lambda (tgt-cell)
				   (make-effect 'set-value (cons tgt-cell val)))
				 tgt-cells)))
		       (cons #f (cons (make-effect 'set-value (cons src-cell #f))
				      effects))))))
