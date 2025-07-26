(define-module (examples restaurant detail recipe-book)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet detail)
  #:export (make-recipe-book-component))

(define (define-recipe-book-propagators cells prop-id)
  (let ((in-dish-name (hash-ref cells 'in-dish-name))
        (out-ingredients (hash-ref cells 'out-ingredients))
        (in-new-recipe (hash-ref cells 'in-new-recipe))
        (cell-recipes (hash-ref cells 'recipes)))
    (list
     (make-propagator (prop-id "add-recipe") in-new-recipe cell-recipes
                      (lambda (new-recipe src-cell)
                        (if (and new-recipe (list? new-recipe) (>= (length new-recipe) 2))
                            (let ((new-recipe-entry (cons (car new-recipe) (cdr new-recipe))))
                              (cons (cons new-recipe-entry (cell-value cell-recipes))
                                    (list (make-effect 'display (format #f "\nINFO: Recipe for ~a added.\n" (car new-recipe)))
                                          (make-effect 'set-value (cons src-cell #f)))))
                            (if new-recipe
                                (cons #f (list (make-effect 'set-value (cons src-cell #f))))
                                (cons #f '())))))
     (make-propagator (prop-id "recipe-lookup") in-dish-name out-ingredients
                      (lambda (dishes src-cell)
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
                              (cons all-ingredients (append all-effects (list (make-effect 'set-value (cons src-cell #f))))))
                            (cons #f '())))))))

(define make-recipe-book-component
  (make-component-factory
   '((in-dish-name #f) (out-ingredients #f) (in-new-recipe #f))
   (list (list 'recipes
               (list (cons 'spaghetti '((pasta 1) (sauce 1)))
                     (cons 'salad '( (lettuce 1) (tomato 1))))))
   define-recipe-book-propagators))
