(use-modules (cpnet core)
             (cpnet runtime)
             (cpnet category)
             (cpnet lambda))

(display "--- CP-Net Lambda Calculus Demo: Higher-Order Functions ---\n")

(define id-input (make-cell 'id-input #f))
(define id-output (make-cell 'id-output #f))
(define p-id (make-propagator 'p-id id-input id-output (lambda (v _) (cons v '()))))
(define ID-Net (make-cpnet-category (list id-input id-output) (list p-id)))
(define id-func-representation (list 'pair id-input id-output))

(define func-holder (make-cell 'func-holder #f))
(define arg (make-cell 'arg 555))
(define result (make-cell 'result #f))
(define p-app (p-apply 'app func-holder arg result))

(define Main-Net
  (make-cpnet-category
   (append (list func-holder arg result) (category-objects ID-Net))
   (append (list p-app) (category-morphisms ID-Net))))

(display "\n--- Applying IDENTITY function to value 555 ---\n")
(cell-set-value! func-holder id-func-representation)
(runtime-settle! Main-Net)

(let ((final-result (cell-value result)))
  (format #t "Final Result: ~a (Expected: 555)\n" final-result)
  (if (equal? final-result 555)
      (display "Success: Higher-order function application successful.\n")
      (display "Failure: Higher-order function application failed.\n")))
