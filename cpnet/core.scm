(define-module (cpnet core)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:use-module ((cpnet category) :prefix cat:)
  #:use-module ((cpnet functor) :prefix fun:)
  #:export (
	    cell?
	    make-cell
	    cell-id
	    cell-value
	    cell-set-value!
	    cell-merge-fn
	    cell-system
	    cell-set-system!
	    effect?
	    make-effect
	    effect-type
	    effect-payload
	    propagator?
	    make-propagator
	    propagator-equal?
	    propagator-compose
	    propagator-id-fn
	    make-cpnet-category
	    p-const
	    make-connector-propagator
	    make-fan-out-propagator
	    make-cpnet-functor))

(define-record-type <cell>
  (make-cell-record id value merge-fn system)
  cell?
  (id cell-id)
  (value cell-value set-cell-value!)
  (merge-fn cell-merge-fn)
  (system cell-system set-cell-system!))

(define-record-type <effect>
  (make-effect type payload)
  effect?
  (type effect-type)
  (payload effect-payload))

(define (default-merge-fn cell new-vals)
  (let* ((old-val (cell-value cell))
         (filtered-vals (delete old-val new-vals equal?)))
    (if (null? filtered-vals)
        (cons old-val '())
        (let ((unique-new-vals (delete-duplicates filtered-vals equal?)))
          (if (null? (cdr unique-new-vals))
              (cons (car unique-new-vals) '())
              (cons #f
                    (list (make-effect 'display
                                       (format #f "CONFLICT on cell ~a. Values: ~a. Reverting to bottom (#f).\n"
                                               (cell-id cell) new-vals)))))))))

(define (make-cell id init-val . maybe-merge-fn)
  (let ((merge-fn (if (null? maybe-merge-fn)
                      default-merge-fn
                      (car maybe-merge-fn))))
    (make-cell-record id init-val merge-fn #f)))

(define (cell-set-value! c new-val)
  (set-cell-value! c new-val)
  c)

(define (cell-set-system! c new-sys)
  (set-cell-system! c new-sys)
  c)

(define (make-propagator id src tgt fn)
  (cat:make-arrow id src tgt fn))

(define propagator? cat:arrow?)

(define (propagator-equal? p q)
  (eq? (cat:arrow-id p) (cat:arrow-id q)))

(define (propagator-compose g f)
  (unless (eq? (cat:arrow-cod f) (cat:arrow-dom g))
    (error "Cannot compose: cod(f) ≠ dom(g)"))
  (let* ((name (string->symbol
                (string-append
                 (symbol->string (cat:arrow-id g))
                 "_o_"
                 (symbol->string (cat:arrow-id f)))))
         (compose-fn (lambda (x src-cell)
                       (let* ((res-f ((cat:arrow-fn f) x src-cell))
                              (val-y (car res-f))
                              (effects-f (cdr res-f)))
                         (if (eq? val-y #f)
                             (cons #f effects-f)
                             (let* ((res-g ((cat:arrow-fn g) val-y (cat:arrow-cod f)))
                                    (val-z (car res-g))
                                    (effects-g (cdr res-g)))
                               (cons val-z (append effects-f effects-g))))))))
    (make-propagator name (cat:arrow-dom f) (cat:arrow-cod g) compose-fn)))

(define (propagator-id-fn cell)
  (let ((name (string->symbol
               (string-append "id-" (symbol->string (cell-id cell))))))
    (make-propagator name cell cell (lambda (x _) (cons x '())))))

(define (make-cpnet-category objects morphisms)
  (cat:make-category
   cat:arrow-dom cat:arrow-cod
   (lambda (g f) (propagator-compose g f))
   propagator-id-fn
   propagator-equal?
   cat:arrow-id
   objects
   morphisms))

(define (p-const id trigger-cell output-cell const-value)
  (make-propagator id trigger-cell output-cell
		   (lambda (val src-cell)
		     (if val
			 (cons const-value
			       (list (make-effect 'set-value (cons src-cell #f))))
			 (cons #f '())))))

(define (make-connector-propagator id from-cell to-cell)
  (make-propagator id from-cell to-cell
    (lambda (v src-cell)
      (if v
          (cons v (list (make-effect 'set-value (cons src-cell #f))))
          (cons #f '())))))

(define (make-fan-out-propagator id from-cell to-cells)
  (make-propagator id from-cell from-cell
    (lambda (val src-cell)
      (if val
          (let ((effects (map (lambda (to-cell)
                                (make-effect 'set-value (cons to-cell val)))
                              to-cells)))
            (cons #f (append effects (list (make-effect 'set-value (cons src-cell #f))))))
          (cons #f '())))))

(define (make-cpnet-functor src-cat tgt-cat cell-map)
  (let* ((F0 (lambda (obj)
               (let ((pair (assoc obj cell-map)))
                 (if pair
                     (cdr pair)
                     #f))))
         (F1 (lambda (p)
               (let ((id (cat:arrow-id p)))
                 (if (and (symbol? id) (string-prefix? "id-" (symbol->string id)))
                     (propagator-id-fn (F0 (cat:arrow-dom p)))
                     (make-propagator id
                                      (F0 (cat:arrow-dom p))
                                      (F0 (cat:arrow-cod p))
                                      (cat:arrow-fn p)))))))
    (fun:make-functor-record src-cat tgt-cat F0 F1)))

