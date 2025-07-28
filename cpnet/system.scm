(define-module (cpnet system)
  #:use-module (srfi srfi-9)
  #:use-module ((cpnet core) :prefix core:)
  #:use-module ((cpnet category) :prefix cat:)
  #:export (make-system
            system?
            system-get-net
            system-get-cell-tables
            system-add-cell-table
            system-find-cell
            system-add-objects
            system-add-morphisms))

(define-record-type <cpnet-system>
  (make-system-record net cell-tables)
  system?
  (net system-get-net)
  (cell-tables system-get-cell-tables))

(define (make-system)
  (make-system-record (core:make-cpnet-category '() '())
                      (make-hash-table)))

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
  (let ((cat-table (hash-ref (system-get-cell-tables system) cat-name)))
    (if cat-table
        (hash-ref cat-table cell-name #f)
        #f)))
