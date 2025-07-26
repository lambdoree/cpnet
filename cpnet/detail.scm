(define-module (cpnet detail)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:export (make-component-factory))

(define (make-component-factory interface-cell-specs private-cell-specs propagator-definer)
  (lambda (system prefix)
    (let* ((cell (lambda (name init-val) (make-cell (string->symbol (format #f "~a-~a" prefix name)) init-val)))
           (prop-id (lambda (name) (string->symbol (format #f "~a-~a" prefix name))))
           (cells (make-hash-table))
           (iface-hash (make-hash-table)))

      (for-each
       (lambda (spec)
         (let* ((name (car spec))
                (init-val (cadr spec))
                (new-cell (cell name init-val)))
           (hash-set! cells name new-cell)
           (hash-set! iface-hash name new-cell)))
       interface-cell-specs)

      (for-each
       (lambda (spec)
         (let* ((name (car spec))
                (init-val (cadr spec))
                (new-cell (cell name init-val)))
           (hash-set! cells name new-cell)))
       private-cell-specs)

      (system-add-objects system (hash-map->list (lambda (k v) v) cells))

      (let ((propagators (propagator-definer cells prop-id)))
        (system-add-morphisms system propagators))

      iface-hash)))
