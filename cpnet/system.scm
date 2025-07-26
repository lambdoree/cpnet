(define-module (cpnet system)
  #:use-module (srfi srfi-9)
  #:use-module ((cpnet core) :prefix core:)
  #:use-module ((cpnet category) :prefix cat:)
  #:export (make-system
            system?
            system-get-net
            system-add-objects
            system-add-morphisms))

(define-record-type <cpnet-system>
  (make-system-record net)
  system?
  (net system-get-net))

(define (make-system)
  (make-system-record (core:make-cpnet-category '() '())))

(define (system-add-objects system . objects)
  (let ((net (system-get-net system))
        (obj-list (if (list? (car objects)) (car objects) objects)))
    (for-each (lambda (obj) (cat:category-add-object net obj)) obj-list)))

(define (system-add-morphisms system . morphisms)
  (let ((net (system-get-net system))
        (mor-list (if (list? (car morphisms)) (car morphisms) morphisms)))
    (for-each (lambda (mor) (cat:category-add-morphism net mor)) mor-list)))
