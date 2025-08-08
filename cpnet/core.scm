(define-module (cpnet core)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:export (
	    cell?
	    make-cell
	    cell-id
	    cell-type
	    cell-value
	    cell-set-value!
            cell-lattice-id
	    cell-merge-fn
	    cell-system
	    cell-set-system!
	    effect?
	    make-effect
	    effect-type
	    effect-payload
            *nothing*
            map-maybe
            category-builder?
            make-category-builder
            builder-name
            builder-function
            *builder-registry*
            register-builder
	    register-lattice
	    builder-signature
            get-builder
            define-object
            ))

(define-record-type <category-builder>
  (make-category-builder-internal name builder-proc signature)
  category-builder?
  (name builder-name)
  (builder-proc builder-function)
  (signature builder-signature))

(define (make-category-builder name builder-proc . maybe-signature)
  (make-category-builder-internal name builder-proc (if (null? maybe-signature) '() (car maybe-signature))))

(define *builder-registry* (make-hash-table))
(define (register-builder cb)
  (hash-set! *builder-registry* (builder-name cb) cb))
(define (get-builder name)
  (hash-ref *builder-registry* name
            (lambda () (error "Unknown category:" name))))

(define-syntax-rule (define-object name) (begin))

(define-object Code)
(define-object category-builder)

(define (map-maybe f x)
  (if (list? x) (map f x) (f x)))

(define-record-type <lattice>
  (make-lattice bottom join)
  lattice?
  (bottom lattice-bottom)
  (join lattice-join))

(define *lattice-registry* (make-hash-table))
(define (register-lattice id bottom join-fn)
  (hash-set! *lattice-registry* id (make-lattice bottom join-fn)))

(define-record-type <cell>
  (make-cell-record id type value lattice-id system)
  cell?
  (id cell-id)
  (type cell-type)
  (value cell-value set-cell-value!)
  (lattice-id cell-lattice-id)
  (system cell-system set-cell-system!))

(define (cell-merge-fn cell)
  (let ((l (hash-ref *lattice-registry* (cell-lattice-id cell))))
    (if l (lattice-join l) (error "Lattice not found" (cell-lattice-id cell)))))

(define-record-type <effect>
  (make-effect type payload)
  effect?
  (type effect-type)
  (payload effect-payload))

(define (make-cell id type init-val . maybe-lattice-id)
  (let ((lattice-id (if (null? maybe-lattice-id) 'Default (car maybe-lattice-id))))
    (make-cell-record id type init-val lattice-id #f)))

(define (cell-set-value! c new-val)
  (set-cell-value! c new-val)
  c)

(define (cell-set-system! c new-sys)
  (set-cell-system! c new-sys)
  c)

(define *nothing* (gensym "nothing"))

(register-lattice 'Default #f
		  (lambda (cell new-vals)
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
							      (cell-id cell) new-vals)))))))))

(register-lattice 'Replace #f
		  (lambda (cell new-vals)
		    (if (null? new-vals)
			(cons (cell-value cell) '())
			(cons (car new-vals) '()))))

(register-lattice 'Set '()
		  (lambda (cell new-vals)
		    (let ((current (let ((val (cell-value cell)))
				     (if (list? val) val (if (not (eq? val #f)) (list val) '()))))
			  (news (map (lambda (v) (if (list? v) v (if (not (eq? v #f)) (list v) '()))) new-vals)))
		      (cons (delete-duplicates (apply append (cons current news)) equal?) '()))))

(register-lattice 'Max *nothing*
  (lambda (cell new-vals)
    (if (null? new-vals)
        (cons (cell-value cell) '())
        (let ((numbers (filter number? new-vals)))
          (if (null? numbers)
              (cons (cell-value cell) '())
              (cons (apply max numbers) '()))))))

(register-lattice 'Min *nothing*
  (lambda (cell new-vals)
    (if (null? new-vals)
        (cons (cell-value cell) '())
        (let ((numbers (filter number? new-vals)))
          (if (null? numbers)
              (cons (cell-value cell) '())
              (cons (apply min numbers) '()))))))

(register-lattice 'Bool #f
  (lambda (cell new-vals)
    (if (null? new-vals)
        (cons (cell-value cell) '())
        (let ((bool-vals (filter boolean? new-vals)))
          (if (null? bool-vals)
              (cons (cell-value cell) '())
              (cons (any identity bool-vals) '()))))))

(register-lattice 'Maybe *nothing*
  (lambda (cell new-vals)
    (let ((defined-vals (filter (lambda (v) (not (eq? v *nothing*))) new-vals)))
      (cond
       ((null? defined-vals) (cons *nothing* '()))
       ((= 1 (length defined-vals)) (cons (car defined-vals) '()))
       (else (cons (car defined-vals)
                   (list (make-effect 'display
                                      (format #f "CONFLICT on Maybe cell ~a. Values: ~a. Taking first value.\n"
                                              (cell-id cell) defined-vals)))))))))


