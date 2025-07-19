(define-module (cpnet category)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:export (category?
	    category-objects
	    category-morphisms
	    category-dom-fn
	    category-cod-fn
	    category-compose-fn
	    category-equal-fn
	    category-id-fn
	    category-validate
	    make-category
	    _category-objects-h
	    _category-morphisms-h

	    category-compose
	    category-add-object
	    category-remove-object
	    category-add-morphism
	    category-remove-morphism
            category-has-object?
            category-has-morphism?

	    make-arrow
	    arrow?
	    arrow-id
	    arrow-dom
	    arrow-cod
	    arrow-fn
	    ))

(define-record-type <category>
  (make-category-record dom-fn cod-fn compose-fn id-fn equal-fn mor-id-fn objs mors)
  category?
  (dom-fn category-dom-fn)
  (cod-fn category-cod-fn)
  (compose-fn category-compose-fn)
  (id-fn category-id-fn)
  (equal-fn category-equal-fn)
  (mor-id-fn category-mor-id-fn)
  (objs _category-objects-h)
  (mors _category-morphisms-h))

(define-record-type <arrow>
  (make-arrow id dom cod fn)
  arrow?
  (id arrow-id)
  (dom arrow-dom)
  (cod arrow-cod)
  (fn arrow-fn))

(define (category-objects cat)
  (hash-map->list (lambda (k v) k) (_category-objects-h cat)))
(define (category-morphisms cat)
  (hash-map->list (lambda (k v) v) (_category-morphisms-h cat)))

(define (category-has-object? cat obj)
  (hash-ref (_category-objects-h cat) obj #f))

(define (category-has-morphism? cat m)
  (hash-ref (_category-morphisms-h cat) ((category-mor-id-fn cat) m) #f))

(define (make-category dom-fn cod-fn compose-fn id-fn equal-fn mor-id-fn obj-list mor-list)
  (let ((objs (make-hash-table))
        (mors (make-hash-table)))
    (for-each (lambda (o) (hash-set! objs o #t)) obj-list)
    (for-each (lambda (m) (hash-set! mors (mor-id-fn m) m)) mor-list)
    (make-category-record dom-fn cod-fn compose-fn id-fn equal-fn mor-id-fn objs mors)))

(define (category-add-object cat obj)
  (let ((id-arrow ((category-id-fn cat) obj)))
    (unless (category-has-object? cat obj)
      (hash-set! (_category-objects-h cat) obj #t))
    (unless (category-has-morphism? cat id-arrow)
      (category-add-morphism cat id-arrow))
    cat))

(define (category-remove-object cat obj)
  (let ((objs (_category-objects-h cat))
        (morphs (_category-morphisms-h cat))
        (dom (category-dom-fn cat))
        (cod (category-cod-fn cat)))
    (hash-remove! objs obj)
    (hash-for-each
     (lambda (k a)
       (when (or (equal? (dom a) obj)
                 (equal? (cod a) obj))
         (hash-remove! morphs k)))
     morphs)
    cat))

(define (category-add-morphism cat arrow)
  (unless (category-has-morphism? cat arrow)
    (hash-set! (_category-morphisms-h cat) (arrow-id arrow) arrow))
  cat)

(define (category-remove-morphism cat arrow)
  (let ((morphs (_category-morphisms-h cat))
        (mor-id-fn (category-mor-id-fn cat)))
    (when (hash-ref morphs (mor-id-fn arrow) #f)
      (hash-remove! morphs (mor-id-fn arrow)))
    cat))

(define (category-compose cat g f)
  (let ((dom     (category-dom-fn     cat))
        (cod     (category-cod-fn     cat))
        (id-fn   (category-id-fn      cat))
        (eq-fn   (category-equal-fn   cat))
        (comp-fn (category-compose-fn cat)))
    (if (equal? (cod f) (dom g))
        (if (eq-fn f (id-fn (dom f)))
            g
            (if (eq-fn g (id-fn (dom g)))
                f
                (comp-fn g f)))
        (error "category-compose: cod(f) is not equal to dom(g)" f g))))

(define (category-validate cat)
  (let* ((dom       (category-dom-fn cat))
         (cod       (category-cod-fn cat))
         (comp      (lambda (g f) (category-compose cat g f)))
         (id        (category-id-fn cat))
         (equal-fn  (category-equal-fn cat))
         (morphs-h  (_category-morphisms-h cat)))
    (hash-for-each
     (lambda (_ f)
       (let ((l (comp f (id (dom f))))
             (r (comp (id (cod f)) f)))
         (unless (and (equal-fn l f)
                      (equal-fn r f))
           (error 'category-validate
                  (format #f "Unit law violation for morphism ~a" f)))))
     morphs-h)
    (let ((all-arrows (hash-map->list (lambda (_ v) v) morphs-h)))
      (for-each
       (lambda (f)
         (for-each
          (lambda (g)
            (when (equal? (cod f) (dom g))
              (for-each
               (lambda (h)
                 (when (equal? (dom h) (cod g))
                   (let ((left  (comp (comp h g) f))
                         (right (comp h (comp g f))))
                     (unless (equal-fn left right)
                       (error 'category-validate
                              (format #f
                                      "Associativity law violation on arrows f=~a, g=~a, h=~a"
                                      f g h))))))
               all-arrows)))
          all-arrows))
       all-arrows))
    #t))

