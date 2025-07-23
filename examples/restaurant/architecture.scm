(define-module (examples restaurant architecture)
  #:use-module (cpnet core)
  #:use-module (cpnet category)
  #:export (RecipeAdapterNet
            sold-dish-name
            ingredients-to-use))

(define sold-dish-name (make-cell 'sold-dish-name #f))
(define ingredients-to-use (make-cell 'ingredients-to-use #f))

(define abstract-recipes
  '( (spaghetti (pasta 1) (sauce 1))
     (salad (lettuce 1) (tomato 1)) ))

(define p-recipe-lookup
  (make-propagator 'recipe-lookup sold-dish-name ingredients-to-use
                   (lambda (dish)
                     (let ((recipe (assoc dish abstract-recipes)))
                       (if recipe
                           ;; On success, produce ingredients and an effect to clear the input
                           (cons (cdr recipe) (list (make-effect 'set-value (cons sold-dish-name #f))))
                           ;; On failure, produce no value and a warning effect
                           (cons #f (list (make-effect 'display (format #f "WARN: No recipe for ~a\n" dish)))))))))

(define RecipeAdapterNet
  (make-cpnet-category (list sold-dish-name ingredients-to-use) (list p-recipe-lookup)))
