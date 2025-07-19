(define-module (cpnet cpnet)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:use-module ((cpnet category) :prefix category:)
  #:use-module ((cpnet functor) :prefix functor:)
  #:use-module ((cpnet nt) :prefix nt:)
  #:export (
    cpnet-category?
    cpnet-make-category
    cpnet-category-validate
    cpnet-category-objects
    cpnet-category-morphisms
    cpnet-category-compose
    cpnet-category-add-object
    cpnet-category-remove-object
    cpnet-category-add-morphism
    cpnet-category-remove-morphism
    cpnet-category-has-object?
    cpnet-category-has-morphism?
    cpnet-category-dom-fn
    cpnet-category-cod-fn
    cpnet-category-compose-fn
    cpnet-category-equal-fn
    cpnet-category-id-fn
    cpnet-arrow?
    cpnet-make-arrow
    cpnet-arrow-id
    cpnet-arrow-dom
    cpnet-arrow-cod
    cpnet-arrow-fn
    cpnet-functor?
    cpnet-make-functor
    cpnet-functor-validate
    cpnet-compose-functor
    cpnet-functor-source
    cpnet-functor-target
    cpnet-functor-object-map
    cpnet-functor-morphism-map
    cpnet-natural-transformation?
    cpnet-make-natural-transformation
    cpnet-natural-transformation-validate
    cpnet-natural-transformation-source
    cpnet-natural-transformation-target
    cpnet-natural-transformation-component
    cpnet-cell?
    cpnet-make-cell
    cpnet-cell-id
    cpnet-cell-value
    cpnet-cell-set-value!
    cpnet-cell-merge-fn
    cpnet-effect?
    cpnet-make-effect
    cpnet-effect-type
    cpnet-effect-payload
    cpnet-propagator?
    cpnet-make-propagator
    cpnet-propagator-id
    cpnet-propagator-src
    cpnet-propagator-tgt
    cpnet-propagator-fn
    cpnet-propagator-equal?
    cpnet-propagator-compose
    cpnet-propagator-id-fn
    cpnet-make-cpnet-category
    cpnet-make-unary-constraint
    cpnet-make-binary-constraint
    cpnet-make-cpnet-functor)
  )

(define category? category:category?)
(define make-category category:make-category)
(define category-validate category:category-validate)
(define category-objects category:category-objects)
(define category-morphisms category:category-morphisms)
(define category-compose category:category-compose)
(define category-add-object category:category-add-object)
(define category-remove-object category:category-remove-object)
(define category-add-morphism category:category-add-morphism)
(define category-remove-morphism category:category-remove-morphism)
(define category-has-object? category:category-has-object?)
(define category-has-morphism? category:category-has-morphism?)
(define category-dom-fn category:category-dom-fn)
(define category-cod-fn category:category-cod-fn)
(define category-compose-fn category:category-compose-fn)
(define category-equal-fn category:category-equal-fn)
(define category-id-fn category:category-id-fn)
(define arrow? category:arrow?)
(define make-arrow category:make-arrow)
(define arrow-id category:arrow-id)
(define arrow-dom category:arrow-dom)
(define arrow-cod category:arrow-cod)
(define arrow-fn category:arrow-fn)
(define functor? functor:functor?)
(define make-functor functor:make-functor)
(define functor-validate functor:functor-validate)
(define compose-functor functor:compose-functor)
(define functor-source functor:functor-source)
(define functor-target functor:functor-target)
(define functor-object-map functor:functor-object-map)
(define functor-morphism-map functor:functor-morphism-map)
(define natural-transformation? nt:natural-transformation?)
(define make-natural-transformation nt:make-natural-transformation)
(define natural-transformation-validate nt:natural-transformation-validate)
(define natural-transformation-source nt:natural-transformation-source)
(define natural-transformation-target nt:natural-transformation-target)
(define natural-transformation-component nt:natural-transformation-component)

;; wrappers for public cpnet interface
(define cpnet-category? category?)
(define cpnet-make-category make-category)
(define cpnet-category-validate category-validate)
(define cpnet-category-objects category-objects)
(define cpnet-category-morphisms category-morphisms)
(define cpnet-category-compose category-compose)
(define cpnet-category-add-object category-add-object)
(define cpnet-category-remove-object category-remove-object)
(define cpnet-category-add-morphism category-add-morphism)
(define cpnet-category-remove-morphism category-remove-morphism)
(define cpnet-category-has-object? category-has-object?)
(define cpnet-category-has-morphism? category-has-morphism?)
(define cpnet-category-dom-fn category-dom-fn)
(define cpnet-category-cod-fn category-cod-fn)
(define cpnet-category-compose-fn category-compose-fn)
(define cpnet-category-equal-fn category-equal-fn)
(define cpnet-category-id-fn category-id-fn)
(define cpnet-arrow? arrow?)
(define cpnet-make-arrow make-arrow)
(define cpnet-arrow-id arrow-id)
(define cpnet-arrow-dom arrow-dom)
(define cpnet-arrow-cod arrow-cod)
(define cpnet-arrow-fn arrow-fn)
(define cpnet-functor? functor?)
(define cpnet-make-functor make-functor)
(define cpnet-functor-validate functor-validate)
(define cpnet-compose-functor compose-functor)
(define cpnet-functor-source functor-source)
(define cpnet-functor-target functor-target)
(define cpnet-functor-object-map functor-object-map)
(define cpnet-functor-morphism-map functor-morphism-map)
(define cpnet-natural-transformation? natural-transformation?)
(define cpnet-make-natural-transformation make-natural-transformation)
(define cpnet-natural-transformation-validate natural-transformation-validate)
(define cpnet-natural-transformation-source natural-transformation-source)
(define cpnet-natural-transformation-target natural-transformation-target)
(define cpnet-natural-transformation-component natural-transformation-component)
(define cpnet-cell? cell?)
(define cpnet-make-cell make-cell)
(define cpnet-cell-id cell-id)
(define cpnet-cell-value cell-value)
(define cpnet-cell-set-value! cell-set-value!)
(define cpnet-cell-merge-fn cell-merge-fn)
(define cpnet-effect? effect?)
(define cpnet-make-effect make-effect)
(define cpnet-effect-type effect-type)
(define cpnet-effect-payload effect-payload)
(define cpnet-propagator? propagator?)
(define cpnet-make-propagator make-propagator)
(define cpnet-propagator-id propagator-id)
(define cpnet-propagator-src propagator-src)
(define cpnet-propagator-tgt propagator-tgt)
(define cpnet-propagator-fn propagator-fn)
(define cpnet-propagator-equal? propagator-equal?)
(define cpnet-propagator-compose propagator-compose)
(define cpnet-propagator-id-fn propagator-id-fn)
(define cpnet-make-cpnet-category make-cpnet-category)
(define cpnet-make-unary-constraint make-unary-constraint)
(define cpnet-make-binary-constraint make-binary-constraint)
(define cpnet-make-cpnet-functor make-cpnet-functor)

(define propagator? arrow?)
(define propagator-id arrow-id)
(define propagator-src arrow-dom)
(define propagator-tgt arrow-cod)
(define propagator-fn arrow-fn)

(define-record-type cell
  (make-cell-record id value merge-fn)
  cell?
  (id       cell-id)
  (value    cell-value set-cell-value!)
  (merge-fn cell-merge-fn))

(define-record-type effect
  (make-effect type payload)
  effect?
  (type effect-type)
  (payload effect-payload))

(define (default-merge-fn cell new-vals)
  (let ((old-val (cell-value cell))
        (first-val (car new-vals)))
    (if (every (lambda (v) (equal? v first-val)) (cdr new-vals))
        ;; All new values are the same. This isn't really a conflict.
        (cons first-val '())
        ;; Real conflict.
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
  (make-arrow id src tgt fn))

(define (propagator-equal? p q)
  (or (eq? (arrow-id p) (arrow-id q))
      (and (eq? (cell-id (arrow-dom p))
                (cell-id (arrow-dom q)))
           (eq? (cell-id (arrow-cod p))
                (cell-id (arrow-cod q))))))

(define (propagator-compose g f)
  (unless (eq? (arrow-cod f) (arrow-dom g))
    (error "Cannot compose: cod(f) ≠ dom(g)"))
  (let* ((name (string->symbol
                (string-append
                 (symbol->string (arrow-id g))
                 "_o_"
                 (symbol->string (arrow-id f)))))
         (compose-fn (lambda (x)
                       (let* ((res-f ((arrow-fn f) x))
                              (val-y (car res-f))
                              (effects-f (cdr res-f)))
                         (if (eq? val-y #f)
                             (cons #f effects-f)
                             (let* ((res-g ((arrow-fn g) val-y))
                                    (val-z (car res-g))
                                    (effects-g (cdr res-g)))
                               (cons val-z (append effects-f effects-g))))))))
    (make-propagator name (arrow-dom f) (arrow-cod g) compose-fn)))

(define (propagator-id-fn cell)
  (let ((name (string->symbol
               (string-append "id-" (symbol->string (cell-id cell))))))
    (make-propagator name cell cell (lambda (x) (cons x '())))))

(define (make-cpnet-category objects morphisms)
  (make-category
   arrow-dom arrow-cod
   (lambda (g f) (propagator-compose g f))
   propagator-id-fn
   propagator-equal?
   arrow-id
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
     ;; Compute C from A and B
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-a id-b id-c)) cell-a cell-c
		      (lambda (a) (let ((b (cell-value cell-b))) (if b (cons (op-c a b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-b id-a id-c)) cell-b cell-c
		      (lambda (b) (let ((a (cell-value cell-a))) (if a (cons (op-c a b) '()) (cons #f '())))))

     ;; Compute A from C and B
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-c id-b id-a)) cell-c cell-a
		      (lambda (c) (let ((b (cell-value cell-b))) (if b (cons (op-a c b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-b id-c id-a)) cell-b cell-a
		      (lambda (b) (let ((c (cell-value cell-c))) (if c (cons (op-a c b) '()) (cons #f '())))))

     ;; Compute B from C and A
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-c id-a id-b)) cell-c cell-b
		      (lambda (c) (let ((a (cell-value cell-a))) (if a (cons (op-b c a) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a-~a,~a->~a" name id-a id-c id-b)) cell-a cell-b
		      (lambda (a) (let ((c (cell-value cell-c))) (if c (cons (op-b c a) '()) (cons #f '())))))
     )))

(define (make-cpnet-functor C D cell-map)
  (unless (and (category? C) (category? D))
    (error 'make-cpnet-functor "Source and target must be categories (cpnets)"))
  (unless (list? cell-map)
    (error 'make-cpnet-functor "cell-map must be an association list"))

  (define (F0 c)
    (let ((pair (assoc c cell-map)))
      (if pair
          (cdr pair)
          (error 'make-cpnet-functor (format #f "No mapping found for cell ~a" (cell-id c))))))

  (define (F1 p)
    (let ((src-c (arrow-dom p)))
      (if (and (eq? src-c (arrow-cod p))
               (eq? (arrow-id p) (arrow-id (propagator-id-fn src-c))))
          (propagator-id-fn (F0 src-c))
          (let* ((tgt-c (arrow-cod p))
                 (src-d (F0 src-c))
                 (tgt-d (F0 tgt-c))
                 (fn-p (arrow-fn p))
                 (id-p (arrow-id p))
                 (new-id (string->symbol (format #f "F(~a)" id-p))))
            (make-propagator new-id src-d tgt-d fn-p)))))

  (make-functor C D F0 F1))


