(define-module (examples test-declarative-loader-error)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet apply)
  #:use-module (cpnet sexp-loader)
  #:use-module (cpnet runtime))

(display "\n--- [Testing Declarative Loader Error Handling] ---\n")

;; The builders used by the .sexp file must be registered first.
(define-category const-42
  (objects
   (instance out Data #f 'Replace))
  (morphisms
   ((morphism make-const () -> out)
    (lambda (vals _) (cons 42 '())))))

(register-builder (make-category-builder
                   'const-42
                   const-42
                   '((inputs ()) (outputs (out)))))

(define-category SourceTestInterface
  (objects
   (instance result Data #f 'Replace)))

(register-builder (make-category-builder
                   'SourceTestInterface
                   SourceTestInterface
                   '()))


(let ((err #f))
  (catch #t
    (lambda ()
      (load-system-from-file "examples/test-arity-mismatch.sexp"))
    (lambda (key . args)
      (set! err (cons key args))))
  (if err
      (begin
        (format #t "Caught expected error: ~s\n" err)
        (let ((original-err (if (eq? (car err) 'misc-error)
                                (car (cdddr err))
                                err)))
          (if (and (list? original-err)
                   (> (length original-err) 1)
                   (eq? (car original-err) 'sexp-loader-arity-mismatch)
                   (string-contains (cadr original-err) "Arity mismatch"))
              (display "OK: Error message is correct.\n")
              (display "FAIL: Error message is incorrect.\n"))))
      (format (current-error-port) "FAIL: Did not catch expected arity mismatch error!\n")))
