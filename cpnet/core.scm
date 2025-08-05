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
	    cell-type
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
	    make-cpnet-functor
            *nothing*
            default-merge-fn
            replace-merge-fn
            append-merge-fn
            max-merge-fn
            min-merge-fn
            map-maybe
            ))

(define (map-maybe f x)
  (if (list? x) (map f x) (f x)))

(define-record-type <cell>
  (make-cell-record id type value merge-fn system)
  cell?
  (id cell-id)
  (type cell-type)
  (value cell-value set-cell-value!)
  (merge-fn cell-merge-fn)
  (system cell-system set-cell-system!))

(define-record-type <effect>
  (make-effect type payload)
  effect?
  (type effect-type)
  (payload effect-payload))

(define (default-merge-fn cell new-vals)
  (let* ((old (cell-value cell))
         (unique-new-vals (delete-duplicates new-vals equal?)))
    (cond ((null? unique-new-vals) (cons old '()))
          ((and (= 1 (length unique-new-vals)) (equal? (car unique-new-vals) old))
           (cons old '()))
          ((= 1 (length unique-new-vals))
           (cons (car unique-new-vals) '()))
          (else
           (cons #f
                 (list (make-effect 'display
                                    (format #f "CONFLICT on cell ~a. Values: ~a. Reverting to bottom (#f).\n"
                                            (cell-id cell) new-vals))))))))

(define (make-cell id type init-val . maybe-merge-fn)
  (let ((merge-fn (if (null? maybe-merge-fn)
                      default-merge-fn
                      (car maybe-merge-fn))))
    (make-cell-record id type init-val merge-fn #f)))

(define (cell-set-value! c new-val)
  (set-cell-value! c new-val)
  c)

(define (cell-set-system! c new-sys)
  (set-cell-system! c new-sys)
  c)

(define (make-propagator id src tgt fn . priority)
  (apply cat:make-arrow id src tgt fn priority))

(define propagator? cat:arrow?)

(define (propagator-equal? p q)
  (eq? (cat:arrow-id p) (cat:arrow-id q)))

(define (propagator-compose g f)
  (unless (equal? (cat:arrow-cod f) (cat:arrow-dom g))
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
    (let ((prio-f (cat:arrow-priority f))
          (prio-g (cat:arrow-priority g)))
      (make-propagator name (cat:arrow-dom f) (cat:arrow-cod g) compose-fn (max prio-f prio-g)))))

(define (propagator-id-fn cell)
  (let ((name (string->symbol
               (string-append "id-" (symbol->string (cell-id cell))))))
    ;; Give identity propagators a high priority so they are considered
    ;; identities during composition validation before other propagators.
    (make-propagator name cell cell (lambda (x _) (cons x '())) 100)))

(define (make-cpnet-category objects morphisms)
  (cat:make-category
   cat:arrow-dom cat:arrow-cod
   (lambda (g f) (propagator-compose g f))
   propagator-id-fn
   propagator-equal?
   cat:arrow-id
   objects
   morphisms))

(define *nothing* (gensym "nothing"))

(define (replace-merge-fn cell new-vals)
  (if (null? new-vals)
      (cons (cell-value cell) '())
      (cons (car new-vals) '())))

(define (append-merge-fn cell new-vals)
  (let ((current (let ((val (cell-value cell)))
                   (if (list? val) val (if (not (eq? val #f)) (list val) '()))))
        (news (map (lambda (v) (if (list? v) v (if (not (eq? v #f)) (list v) '()))) new-vals)))
    (cons (delete-duplicates (apply append (cons current news)) equal?) '())))

(define (max-merge-fn cell new-vals)
  (if (null? new-vals)
      (cons (cell-value cell) '())
      (cons (apply max new-vals) '())))

(define (min-merge-fn cell new-vals)
  (if (null? new-vals)
      (cons (cell-value cell) '())
      (cons (apply min new-vals) '())))

(define (make-cpnet-functor name src-cat tgt-cat cell-map)
  (let* ((F0 (lambda (obj)
               (let ((pair (assoc obj cell-map)))
                 (if pair
                     (cdr pair)
                     #f))))
         (F1 (lambda (p)
               (let* ((id (cat:arrow-id p))
                      (id-str (symbol->string id)))
                 (if (string-contains id-str "id-")
                     (propagator-id-fn (F0 (cat:arrow-dom p)))
                     (let ((new-dom (map-maybe F0 (cat:arrow-dom p)))
                           (new-cod (map-maybe F0 (cat:arrow-cod p))))
                       (make-propagator id new-dom new-cod (cat:arrow-fn p))))))))
    (fun:make-functor-record name src-cat tgt-cat F0 F1)))

