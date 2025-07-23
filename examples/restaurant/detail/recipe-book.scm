(define-module (examples restaurant detail recipe-book)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:use-module (cpnet detail)
  #:use-module (examples restaurant static-architecture)
  #:export (ImplementedRecipeBookNet))

;;; --- Private State Cells ---
(define cell-recipes
  (make-cell 'recipes
             (list (cons 'spaghetti '((pasta 1) (sauce 1)))
                   (cons 'salad '( (lettuce 1) (tomato 1))))))

;;; --- Propagators ---
(define p-add-recipe
  (make-event-propagator 'add-recipe in-new-recipe cell-recipes
    (lambda (new-recipe)
      (if (and (list? new-recipe) (>= (length new-recipe) 2))
          (let ((new-recipe-entry (cons (car new-recipe) (cdr new-recipe))))
            (cons (cons new-recipe-entry (cell-value cell-recipes))
                  (list (make-effect 'display (format #f "\nINFO: Recipe for ~a added.\n" (car new-recipe))))))
        (cons #f '())))))

(define p-recipe-lookup
  (make-event-propagator 'recipe-lookup in-dish-name out-ingredients
    (lambda (dishes)
      (if (and dishes (list? dishes))
          (let* ((current-recipes (cell-value cell-recipes))
                 (results (map (lambda (dish)
                                 (let ((recipe (assoc dish current-recipes)))
                                   (if recipe
                                       (cons (cdr recipe) '())
                                     (cons '() (list (make-effect 'display (format #f "WARN: No recipe for ~a\n" dish)))))))
                               dishes))
                 (all-ingredients (apply append (map car results)))
                 (all-effects (apply append (map cdr results))))
            (cons all-ingredients all-effects))
        (cons #f '())))))

;;; --- Component Implementation ---
(define ImplementedRecipeBookNet
  (implement-component RecipeBookNet
    (list cell-recipes)
    (list p-recipe-lookup p-add-recipe)))
