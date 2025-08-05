(define-module (cpnet conditional)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:export (conditional-engine))

(define-object Bool)
(define-object Data)

(define-category conditional-category
  (objects
   (instance p Bool #f replace-merge-fn)
   (instance a Data #f replace-merge-fn)
   (instance b Data #f replace-merge-fn)
   (instance result Data #f replace-merge-fn))
  (morphisms
   ((morphism branch-true (p a) -> result)
    (lambda (vals _) (if (car vals) (cons (cadr vals) '()) (cons *nothing* '()))))
   ((morphism branch-false (p b) -> result)
    (lambda (vals _) (if (not (car vals)) (cons (cadr vals) '()) (cons *nothing* '()))))))

(define-cpnet-system conditional-engine
  (conditional-category))
