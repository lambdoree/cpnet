(define-module (cpnet nt)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (cpnet category)
  #:use-module (cpnet functor)
  #:export (make-natural-transformation
            natural-transformation?
            natural-transformation-source
            natural-transformation-target
            natural-transformation-component
            natural-transformation-validate))

(define-record-type natural-transformation
  (make-natural-transformation-record source-functor target-functor component)
  natural-transformation?
  (source-functor   natural-transformation-source)
  (target-functor   natural-transformation-target)
  (component        natural-transformation-component))

(define (make-natural-transformation F G comp)
  (unless (and (functor? F)
               (functor? G)
               (eq? (functor-source F) (functor-source G))
               (eq? (functor-target F) (functor-target G))
               (procedure? comp))
    (error 'make-natural-transformation
           "make-natural-transformation requires two compatible functors and a procedure"))
  (let* ((J    (functor-source      F))
         (objs (category-objects    J))
         (T    (functor-target      F))
         (domT (category-dom-fn     T))
         (codT (category-cod-fn     T))
         (eqT  (category-equal-fn   T))
         (morsT (category-morphisms T))
         (F0   (functor-object-map  F))
         (G0   (functor-object-map  G))
         (alist
          (map (lambda (x)
                 (let ((etax (comp x)))
                   (unless (and (member etax morsT eqT)
                                (equal? (domT etax) (F0 x))
                                (equal? (codT etax) (G0 x)))
                     (error 'make-natural-transformation
                            (format #f "Invalid component for object ~a" x)))
                   (cons x etax)))
               objs)))
    (make-natural-transformation-record
     F G
     (lambda (x)
       (cdr (assoc x alist))))))

(define (natural-transformation-validate eta)
  (unless (natural-transformation? eta)
    (error 'natural-transformation-validate "Not a natural-transformation"))
  (let* ((F     (natural-transformation-source    eta))
         (G     (natural-transformation-target    eta))
         (etac    (natural-transformation-component eta))
         (C     (functor-source                   F))
         (mors  (category-morphisms   C))
         (domC  (category-dom-fn       C))
         (codC  (category-cod-fn       C))
         (T     (functor-target        F))
         (compT (category-compose-fn   T))
         (eqT   (category-equal-fn     T))
         (F1    (functor-morphism-map  F))
         (G1    (functor-morphism-map  G)))
    (for-each
     (lambda (f)
       (let* ((X   (domC f))
	      (Y   (codC f))
	      (etaX  (etac X))
	      (etaY  (etac Y))
	      (Ff  (F1 f))
	      (Gf  (G1 f))
	      (lhs (compT Gf etaX))
	      (rhs (compT etaY Ff)))
         (unless (eqT lhs rhs)
           (error 'natural-transformation-validate
                  (format #f "Naturality fails on arrow ~a" f)))))
     mors)
    #t))
