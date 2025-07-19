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
	    make-unary-constraint
	    make-binary-constraint
	    make-cpnet-functor))

(define-record-type <cell>
  (make-cell-record id value merge-fn)
  cell?
  (id cell-id)
  (value cell-value set-cell-value!)
  (merge-fn cell-merge-fn))

(define-record-type <effect>
  (make-effect type payload)
  effect?
  (type effect-type)
  (payload effect-payload))

(define (default-merge-fn cell new-vals)
  (let ((old-val (cell-value cell))
        (first-val (car new-vals)))
    (if (every (lambda (v) (equal? v first-val)) (cdr new-vals))
        (cons first-val '())
        (cons old-val
              (list (make-effect 'display
                                 (format #f "CONFLICT on cell ~a. Values: ~a. Keeping old value ~a.\n"
                                         (cell-id cell) new-vals old-val)))))))

(define (make-cell id init-val . maybe-merge-fn)
  (let ((merge-fn (if (null? maybe-merge-fn)
                      default-merge-fn
                      (car maybe-merge-fn))))
    (make-cell-record id init-val merge-fn)))

(define (cell-set-value! c new-val)
  (set-cell-value! c new-val)
  c)

(define (make-propagator id src tgt fn)
  (cat:make-arrow id src tgt fn))

(define propagator? cat:arrow?)

(define (propagator-equal? p q)
  (or (eq? (cat:arrow-id p) (cat:arrow-id q))
      (and (eq? (cell-id (cat:arrow-dom p))
                (cell-id (cat:arrow-dom q)))
           (eq? (cell-id (cat:arrow-cod p))
                (cell-id (cat:arrow-cod q))))))

(define (propagator-compose g f)
  (unless (eq? (cat:arrow-cod f) (cat:arrow-dom g))
    (error "Cannot compose: cod(f) ≠ dom(g)"))
  (let* ((name (string->symbol
                (string-append
                 (symbol->string (cat:arrow-id g))
                 "_o_"
                 (symbol->string (cat:arrow-id f)))))
         (compose-fn (lambda (x)
                       (let* ((res-f ((cat:arrow-fn f) x))
                              (val-y (car res-f))
                              (effects-f (cdr res-f)))
                         (if (eq? val-y #f)
                             (cons #f effects-f)
                             (let* ((res-g ((cat:arrow-fn g) val-y))
                                    (val-z (car res-g))
                                    (effects-g (cdr res-g)))
                               (cons val-z (append effects-f effects-g))))))))
    (make-propagator name (cat:arrow-dom f) (cat:arrow-cod g) compose-fn)))

(define (propagator-id-fn cell)
  (let ((name (string->symbol
               (string-append "id-" (symbol->string (cell-id cell))))))
    (make-propagator name cell cell (lambda (x) (cons x '())))))

(define (make-cpnet-category objects morphisms)
  (cat:make-category
   cat:arrow-dom cat:arrow-cod
   (lambda (g f) (propagator-compose g f))
   propagator-id-fn
   propagator-equal?
   cat:arrow-id
   objects
   morphisms))

(define (make-unary-constraint cell-a cell-b fwd inv name)
  (let ((id-a (cell-id cell-a)) (id-b (cell-id cell-b)))
    (list
     (make-propagator
      (string->symbol (format #f "p-~a-~a->~a" name id-a id-b)) cell-a cell-b
      (lambda (a) (cons (fwd a) '())))
     (make-propagator
      (string->symbol (format #f "p-~a-~a->~a" name id-b id-a)) cell-b cell-a
      (lambda (b) (cons (inv b) '()))))))

(define (make-binary-constraint cell-a cell-b cell-c op-c op-a op-b name)
  (let ((id-a (cell-id cell-a)) (id-b (cell-id cell-b)) (id-c (cell-id cell-c)))
    (list
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-a id-b id-c)) cell-a cell-c
                      (lambda (a) (let ((b (cell-value cell-b))) (if b (cons (op-c a b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-b id-a id-c)) cell-b cell-c
                      (lambda (b) (let ((a (cell-value cell-a))) (if a (cons (op-c a b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-c id-b id-a)) cell-c cell-a
                      (lambda (c) (let ((b (cell-value cell-b))) (if b (cons (op-a c b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-b id-c id-a)) cell-b cell-a
                      (lambda (b) (let ((c (cell-value cell-c))) (if c (cons (op-a c b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-c id-a id-b)) cell-c cell-b
                      (lambda (c) (let ((a (cell-value cell-a))) (if a (cons (op-b c a) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-a id-c id-b)) cell-a cell-b
                      (lambda (a) (let ((c (cell-value cell-c))) (if c (cons (op-b c a) '()) (cons #f '()))))))))

(define (make-cpnet-functor src-cat tgt-cat cell-map)
  (let* ((F0 (lambda (obj)
               (let ((pair (assoc obj cell-map)))
                 (if pair
                     (cdr pair)
                     (error "make-cpnet-functor: object not in cell-map" obj)))))
         (F1 (lambda (p)
               (let ((id (cat:arrow-id p)))
                 (if (and (symbol? id) (string-prefix? "id-" (symbol->string id)))
                     (propagator-id-fn (F0 (cat:arrow-dom p)))
                     (make-propagator id
                                      (F0 (cat:arrow-dom p))
                                      (F0 (cat:arrow-cod p))
                                      (cat:arrow-fn p)))))))
    (fun:make-functor src-cat tgt-cat F0 F1)))
