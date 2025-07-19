(define-module (cpnet nt)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module ((cpnet category) :prefix cat:)
  #:use-module ((cpnet functor) :prefix fun:)
  #:export (make-natural-transformation
            natural-transformation?
            natural-transformation-source
            natural-transformation-target
            natural-transformation-component
            natural-transformation-validate))

(define-record-type <natural-transformation>
  (make-natural-transformation-record source target component)
  natural-transformation?
  (source    natural-transformation-source)
  (target    natural-transformation-target)
  (component natural-transformation-component))

(define (make-natural-transformation F G comp)
  (unless (and (fun:functor? F)
               (fun:functor? G)
               (eq? (fun:functor-source F) (fun:functor-source G))
               (eq? (fun:functor-target F) (fun:functor-target G))
               (procedure? comp))
    (error 'make-natural-transformation
           "make-natural-transformation requires two compatible functors and a procedure"))
  (let* ((J    (fun:functor-source      F))
         (objs (cat:category-objects    J))
         (T    (fun:functor-target      F))
         (domT (cat:category-dom-fn     T))
         (codT (cat:category-cod-fn     T))
         (eqT  (cat:category-equal-fn   T))
         (morsT (cat:category-morphisms T))
         (F0   (fun:functor-object-map  F))
         (G0   (fun:functor-object-map  G))
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
         (C     (fun:functor-source                   F))
         (mors  (cat:category-morphisms   C))
         (domC  (cat:category-dom-fn       C))
         (codC  (cat:category-cod-fn       C))
         (T     (fun:functor-target        F))
         (compT (cat:category-compose-fn   T))
         (eqT   (cat:category-equal-fn     T))
         (F1    (fun:functor-morphism-map  F))
         (G1    (fun:functor-morphism-map  G)))
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
