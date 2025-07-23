(use-modules (srfi srfi-1)
             ((cpnet category) :prefix cat:)
             (cpnet core)
             (cpnet functor)
             (cpnet nt)
             ((cpnet runtime) :prefix rt:))

(display "\n\n========================================================\n")
(display "       Using Natural Transformations as Adapters\n")
(display "========================================================\n\n")

(define Input (make-cell 'Input #f))
(define Output (make-cell 'Output #f))
(define f (make-propagator 'f Input Output (lambda (x _) (cons x '()))))
(define C (make-cpnet-category (list Input Output) (list f)))
(display "Defined an abstract interface C (Input -> Output).\n")
(cat:category-validate C)

(define A (make-cell 'A #f)) (define B (make-cell 'B #f))
(define X (make-cell 'X #f)) (define Y (make-cell 'Y #f))
(define D (make-cpnet-category (list A B X Y) '()))
(rt:runtime-show-state D "Target network D with cells A,B,X,Y")

(define pAB (make-propagator 'p_A->B A B (lambda (a _) (cons (+ a 10) '()))))
(cat:category-add-morphism D pAB)
(define F0-F (lambda (obj) (if (eq? (cell-id obj) 'Input) A B)))
(define F1-F (lambda (morph)
               (if (eq? (cat:arrow-id morph) 'f)
                   pAB
                   (propagator-id-fn (F0-F (cat:arrow-dom morph))))))
(define F (make-functor C D F0-F F1-F))
(display "\n--- Defined Functor F (implements interface as A->B via '+10') ---\n")
(functor-validate F)

(define pXY (make-propagator 'p_X->Y X Y (lambda (x _) (cons (+ x 10) '()))))
(cat:category-add-morphism D pXY)
(define G0-G (lambda (obj) (if (eq? (cell-id obj) 'Input) X Y)))
(define G1-G (lambda (morph)
               (if (eq? (cat:arrow-id morph) 'f)
                   pXY
                   (propagator-id-fn (G0-G (cat:arrow-dom morph))))))
(define G (make-functor C D G0-G G1-G))
(display "--- Defined Functor G (implements interface as X->Y via '+10') ---\n")
(functor-validate G)
(display "Functors F and G are valid.\n")

(define pAX (make-propagator 'p_A->X A X (lambda (a _) (cons a '()))))
(define pBY (make-propagator 'p_B->Y B Y (lambda (b _) (cons b '()))))
(cat:category-add-morphism D pAX)
(cat:category-add-morphism D pBY)

(define (eta-component obj)
  (if (eq? (cell-id obj) 'Input) pAX pBY))

(define eta (make-natural-transformation F G eta-component))
(display "\n--- Defined Natural Transformation eta: F -> G (the adapter) ---\n")
(display "eta components: p_A->X (identity), p_B->Y (identity)\n")

(natural-transformation-validate eta)
(display "Natural transformation eta is valid.\n")
(display "success\n")

(display "\n--- Testing the adapter functionality ---\n")
(display "Setting A=5 in F's domain...\n")
(cell-set-value! A 5)
(rt:runtime-show-state D "Before settling")

(rt:runtime-execute-effects (rt:runtime-settle! D))
(rt:runtime-show-state D "After settling")

(if (and (= (cell-value B) 15)
         (= (cell-value Y) 15))
    (display "success\n"))
