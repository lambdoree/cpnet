(define-module (cpnet system)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:use-module ((cpnet core) :prefix core:)
  #:use-module ((cpnet category) :prefix cat:)
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
            system-add-branch-propagator
            system-remove-subsystem!
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
                      (core:make-cpnet-category '() '())
                      (make-hash-table)
                      '()
                      '()))

(define (system-remove-subsystem! system prefix-sym)
  (let* ((prefix-str (symbol->string prefix-sym))
         (net (system-get-net system))
         (tables (system-get-cell-tables system)))
    ;; Remove morphisms
    (for-each
     (lambda (mor)
       (when (string-prefix? prefix-str (symbol->string (cat:arrow-id mor)))
         (cat:category-remove-morphism net mor)))
     (cat:category-morphisms net))
    ;; Remove objects (cells)
    (for-each
     (lambda (obj)
       (when (string-prefix? prefix-str (symbol->string (core:cell-id obj)))
         (cat:category-remove-object net obj)))
     (cat:category-objects net))
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

(define (system-add-branch-propagator sys cat-name cond-name then-name else-name result-name)
  (let* ((table (system-get-category-table sys cat-name))
         (c (and table (hash-ref table cond-name #f)))
         (t (and table (hash-ref table then-name #f)))
         (e (and table (hash-ref table else-name #f)))
         (r (and table (hash-ref table result-name #f)))
         (id (string->symbol (format #f "branch-~a" (gensym)))))
    (if (and c t e r)
        (system-add-propagator! sys
          (core:make-branch-propagator id c t e r))
        (error "system-add-branch-propagator: cell not found"
               (list cat-name cond-name then-name else-name result-name)))))

