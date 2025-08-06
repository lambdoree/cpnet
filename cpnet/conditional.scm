(define-module (cpnet conditional)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:export (conditional-engine))

(define-object Bool)
(define-object Data)

(define-category conditional-category
  (objects
   (instance p Bool #f replace-merge-fn)
   (instance a Data #f replace-merge-fn)
   (instance b Data #f replace-merge-fn)
   (instance result Data #f replace-merge-fn)))

(define-cpnet-system conditional-engine
  (conditional-category)
  (system-add-branch-propagator (current-system) 'conditional-category 'p 'a 'b 'result))
