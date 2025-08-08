(define-module (cpnet system)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:use-module ((cpnet core) :prefix core:)
  #:use-module ((cpnet category) :prefix cat:)
  #:use-module ((cpnet functor) :prefix fun:)
  #:export (make-system
            system?
            system-name
            system-get-net
            system-get-cell-tables
            system-add-cell-table
            system-find-cell
            system-add-objects
            system-add-morphisms
            system-add-propagator!
            system-get-category-table
            system-find-propagator
            system-find-category-name-for-cat
            system-functors
            system-nts
            system-add-functor!
            system-add-nt!
            system-remove-subsystem!
            add-subsystem!
            propagator?
	    make-propagator
	    propagator-equal?
	    propagator-compose
	    propagator-id-fn
	    make-cpnet-category
	    make-cpnet-functor
            make-branch-propagator
            ))

(define-record-type <cpnet-system>
  (make-system-record name net cell-tables functors nts)
  system?
  (name system-name)
  (net system-get-net)
  (cell-tables system-get-cell-tables)
  (functors system-functors system-set-functors!)
  (nts system-nts system-set-nts!))

(define (make-system . name)
  (make-system-record (if (null? name) #f (car name))
                      (make-cpnet-category '() '())
                      (make-hash-table)
                      '()
                      '()))

(define (string-split-by-substring s sub)
  (if (string-null? sub)
      (error "string-split-by-substring: delimiter cannot be empty")
      (let loop ((str s) (result '()))
        (let ((pos (string-contains str sub)))
          (if pos
              (loop (substring str (+ pos (string-length sub)))
                    (cons (substring str 0 pos) result))
              (reverse (cons str result)))))))

(define (system-remove-subsystem! system prefix-sym)
  (let* ((prefix-str (symbol->string prefix-sym))
         (net (system-get-net system))
         (tables (system-get-cell-tables system)))
    ;; Remove morphisms
    (for-each
     (lambda (mor)
       (when (string-prefix? prefix-str (symbol->string (cat:arrow-id mor)))
         (cat:category-remove-morphism net mor)))
     (list-copy (cat:category-morphisms net)))
    ;; Remove objects (cells)
    (for-each
     (lambda (obj)
       (when (string-prefix? prefix-str (symbol->string (core:cell-id obj)))
         (cat:category-remove-object net obj)))
     (list-copy (cat:category-objects net)))
    ;; Remove cell tables
    (for-each
     (lambda (key)
       (when (string-prefix? prefix-str (symbol->string key))
         (hash-remove! tables key)))
     (hash-map->list (lambda (k v) k) tables))))

(define (system-add-objects system . objects)
  (let ((net (system-get-net system))
        (obj-list (if (list? (car objects)) (car objects) objects)))
    (for-each (lambda (obj)
                (when (core:cell? obj)
                  (core:cell-set-system! obj system))
                (cat:category-add-object net obj))
              obj-list)))

(define (system-add-morphisms system . morphisms)
  (let ((net (system-get-net system))
        (mor-list (if (list? (car morphisms)) (car morphisms) morphisms)))
    (for-each (lambda (mor) (cat:category-add-morphism net mor)) mor-list)))

(define (system-add-cell-table system cat-name table)
  (hash-set! (system-get-cell-tables system) cat-name table))

(define (system-find-cell system cat-name cell-name)
  (let* ((tables (system-get-cell-tables system))
         (cat-table (hash-ref tables cat-name #f)))
    (if cat-table
        (hash-ref cat-table cell-name #f)
        #f)))


(define (system-add-propagator! system propagator)
  (system-add-morphisms system propagator))

(define (system-get-category-table system cat-name)
  (hash-ref (system-get-cell-tables system) cat-name #f))

(define (system-find-propagator system cat-name prop-name)
  (let* ((net (system-get-net system))
         (mangled-id-str (if cat-name
                             (format #f "~a.~a" cat-name prop-name)
                             (symbol->string prop-name)))
         (mangled-id (string->symbol mangled-id-str))
         (prop (cat:category-find-morphism-by-id net mangled-id)))
    (if prop
        prop
        (cat:category-find-morphism-by-suffix net prop-name))))

(define (system-find-category-name-for-cat system cat)
  (let* ((tables (system-get-cell-tables system))
         (cat-objs (cat:category-objects cat))
         (found-pair (find (lambda (pair)
                             (let* ((cat-name (car pair))
                                    (table (cdr pair))
                                    (table-cells (hash-map->list (lambda (k v) v) table)))
                               (if (null? cat-objs)
                                   (null? table-cells)
                                   (and (= (length cat-objs) (length table-cells))
                                        (null? (lset-difference eq? cat-objs table-cells))))))
                           (hash-map->list cons tables))))
    (if found-pair
        (car found-pair)
        #f)))

(define (system-add-functor! system functor)
  (system-set-functors! system (cons functor (system-functors system))))

(define (system-add-nt! system nt)
  (system-set-nts! system (cons nt (system-nts system))))

(define (make-propagator id src tgt fn . priority)
  (let ((real-src (if (and (list? src) (= 1 (length src)) (not (core:cell? (car src)))) (car src) src))
        (real-tgt (if (and (list? tgt) (= 1 (length tgt)) (not (core:cell? (car tgt)))) (car tgt) tgt)))
    (apply cat:make-arrow id real-src real-tgt fn priority)))

(define propagator? cat:arrow?)

(define (propagator-equal? p q)
  (eq? (cat:arrow-id p) (cat:arrow-id q)))

(define (propagator-compose g f)
  (unless (equal? (cat:arrow-cod f) (cat:arrow-dom g))
    (error "Cannot compose: cod(f) ≠ dom(g)"))
  (let* ((g-id-str (symbol->string (cat:arrow-id g)))
         (f-id-str (symbol->string (cat:arrow-id f)))
         (delim "_o_")
         (g-parts (string-split-by-substring g-id-str delim))
         (f-parts (string-split-by-substring f-id-str delim))
         (name (string->symbol (string-join (append g-parts f-parts) delim)))
         (compose-fn (lambda (x src-cell)
                       (let* ((res-f ((cat:arrow-fn f) x src-cell))
                              (val-y (car res-f))
                              (effects-f (cdr res-f)))
                         (if (eq? val-y core:*nothing*)
                             (cons core:*nothing* effects-f)
                             (let* ((res-g ((cat:arrow-fn g) val-y (cat:arrow-cod f)))
                                    (val-z (car res-g))
                                    (effects-g (cdr res-g)))
                               (cons val-z (append effects-f effects-g))))))))
    (let ((prio-f (cat:arrow-priority f))
          (prio-g (cat:arrow-priority g)))
      (make-propagator name (cat:arrow-dom f) (cat:arrow-cod g) compose-fn (max prio-f prio-g)))))

(define (propagator-id-fn cell)
  (let ((name (string->symbol
               (string-append "id-" (symbol->string (core:cell-id cell))))))
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
		     (let ((new-dom (core:map-maybe F0 (cat:arrow-dom p)))
			   (new-cod (core:map-maybe F0 (cat:arrow-cod p))))
		       (make-propagator id new-dom new-cod (cat:arrow-fn p))))))))
    (fun:make-functor-record name src-cat tgt-cat F0 F1)))

(define (make-branch-propagator id cond-cell then-cell else-cell result-cell)
  (make-propagator
   id
   (list cond-cell then-cell else-cell)
   result-cell
   (lambda (vals _)
     (let ((p? (list-ref vals 0))
           (x  (list-ref vals 1))
           (y  (list-ref vals 2)))
       (cond
        ((eq? p? #t) (cons x '()))
        ((eq? p? #f) (cons y '()))
        (else (cons core:*nothing* '())))))))

(define (add-subsystem! target-system source-system-or-proc)
  (let* ((source-system (if (procedure? source-system-or-proc)
                            (source-system-or-proc)
                            source-system-or-proc))
         (source-prefix (system-name source-system))
         (old->new-cell-map (make-hash-table)))
    (when (not source-prefix)
      (error "Cannot compose an unnamed system" source-system))
    (let ((source-net (system-get-net source-system)))
      (for-each
       (lambda (old-cell)
         (let* ((new-id (string->symbol (format #f "~a.~a" source-prefix (core:cell-id old-cell))))
                (new-cell (core:make-cell new-id (core:cell-type old-cell) (core:cell-value old-cell) (core:cell-lattice-id old-cell))))
           (hash-set! old->new-cell-map old-cell new-cell)))
       (cat:category-objects source-net))
      (system-add-objects target-system (hash-map->list (lambda (k v) v) old->new-cell-map)))
    (let ((source-net (system-get-net source-system)))
      (for-each
       (lambda (old-mor)
         (let* ((map-one (lambda (c) (hash-ref old->new-cell-map c #f)))
                (old-dom (cat:arrow-dom old-mor))
                (old-cod (cat:arrow-cod old-mor))
                (new-dom (core:map-maybe map-one old-dom))
                (new-cod (core:map-maybe map-one old-cod)))
           (if (and new-dom (if (list? new-dom) (not (member #f new-dom)) #t)
                    new-cod (if (list? new-cod) (not (member #f new-cod)) #t))
               (let* ((old-fn (cat:arrow-fn old-mor))
                      (new-mor-id (string->symbol (format #f "~a.~a" source-prefix (cat:arrow-id old-mor)))))
                 (system-add-morphisms target-system (list (make-propagator new-mor-id new-dom new-cod old-fn (cat:arrow-priority old-mor)))))
               (format (current-error-port) "Warning: could not map morphism ~a during subsystem merge.\n" (cat:arrow-id old-mor)))))
       (cat:category-morphisms source-net)))
    (hash-for-each
     (lambda (cat-name table)
       (let* ((mangled-cat-name (string->symbol (format #f "~a.~a" source-prefix cat-name)))
              (new-table (make-hash-table)))
         (hash-for-each
          (lambda (cell-name old-cell)
            (let ((new-cell (hash-ref old->new-cell-map old-cell)))
              (hash-set! new-table cell-name new-cell)))
          table)
         (system-add-cell-table target-system mangled-cat-name new-table)))
     (system-get-cell-tables source-system))))

