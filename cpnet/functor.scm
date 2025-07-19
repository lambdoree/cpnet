(define-module (cpnet functor)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (cpnet category)
  #:export (make-functor
            functor?
            functor-source
            functor-target
            functor-object-map
            functor-morphism-map
            functor-validate
            compose-functor))

(define-record-type functor
  (make-functor-record source-cat target-cat object-map morphism-map)
  functor?
  (source-cat       functor-source)
  (target-cat       functor-target)
  (object-map       functor-object-map)
  (morphism-map     functor-morphism-map))

(define (make-functor C D F0 F1)
  (unless (and (category? C)
               (category? D)
               (procedure? F0)
               (procedure? F1))
    (error 'make-functor
           "make-functor requires two categories and two procedures"))
  (make-functor-record C D F0 F1))

(define (all-composable-pairs C)
  (append-map
   (λ (f)
     (append-map
      (λ (g)
        (if (equal? ((category-cod-fn C) f)
                    ((category-dom-fn C) g))
            (list (cons f g))
            '()))
      (category-morphisms C)))
   (category-morphisms C)))

(define (functor-validate F)
  (unless (functor? F)
    (error 'functor-validate "Not a functor"))
  (let* ((C      (functor-source       F))
         (D      (functor-target       F))
         (F0     (functor-object-map   F))
         (F1     (functor-morphism-map F))
         (domC   (category-dom-fn      C))
         (codC   (category-cod-fn      C))
         (compC  (category-compose-fn  C))
         (idC     (lambda (x) (category-id-fn C x)))
         (compD  (category-compose-fn  D))
         (idD     (lambda (x) (category-id-fn D x)))
         (eqD     (category-equal-fn D)))
    (for-each
     (lambda (pair)
       (let* ((f   (car pair))
              (g   (cdr pair))
              (lhs (F1 (compC g f)))
              (rhs (compD (F1 g) (F1 f))))
         (unless (eqD lhs rhs)
           (error 'functor-validate
                  (format #f "Composition law violated on ~a, ~a" f g)))))
     (all-composable-pairs C))
    (for-each
     (lambda (x)
       (let* ((lhs (F1 (idC x)))
              (rhs (idD (F0 x))))
         (unless (eqD lhs rhs)
           (error 'functor-validate
                  (format #f "Identity law violated at ~a" x)))))
     (category-objects C))
    #t))

(define (compose-functor G F)
  (unless (and (functor? G) (functor? F))
    (error 'compose-functor "Arguments must be functors: ~a, ~a" G F))
  (let ((C  (functor-source F))
        (D  (functor-target F))
        (D' (functor-source G))
        (E  (functor-target G)))
    (unless (eq? D D')
      (error 'compose-functor "Source/target mismatch: cannot compose" G F))
    (make-functor-record
     C
     E
     (lambda (x) ((functor-object-map G)
             ((functor-object-map F) x)))
     (lambda (m) ((functor-morphism-map G)
             ((functor-morphism-map F) m))))))
