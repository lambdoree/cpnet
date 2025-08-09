(define-module (examples test-oscillation)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime))

(define-category flip-flop-interface
  (objects
   (instance A Bool #f)
   (instance B Bool #f)))

(define-cpnet-system TestOscillation
  (flip-flop-interface)
  (propagator p-A->B
              (get-cell 'flip-flop-interface 'A)
              -> (get-cell 'flip-flop-interface 'B)
              (lambda (vals _) (cons (not (car vals)) '())))
  (propagator p-B->A
              (get-cell 'flip-flop-interface 'B)
              -> (get-cell 'flip-flop-interface 'A)
              (lambda (vals _) (cons (not (car vals)) '()))))

(parameterize ((current-system (TestOscillation)))
  (show-state "--- Test Oscillation: Before ---")
  (run)
  (show-state "--- Test Oscillation: After ---"))

(display "\n--- [Testing Oscillation Error Mode] ---\n")
(let ((err #f))
  (catch #t
    (lambda ()
      (parameterize ((current-system (TestOscillation))
                     (*oscillation-mode* 'error))
        (run)))
    (lambda (key . args)
      (set! err (cons key args))))
  (if err
      (begin
        (format #t "Caught expected error: ~s\n" err)
        (let ((original-err (if (eq? (car err) 'misc-error)
                                (if (> (length err) 3) (car (cdddr err)) err)
                                err)))
          (if (and (list? original-err)
                   (> (length original-err) 0)
                   (eq? (car original-err) 'oscillation-detected))
              (display "OK: Error key is correct.\n")
              (display "FAIL: Error key is incorrect.\n"))))
      (format (current-error-port) "FAIL: Did not catch expected oscillation error!\n")))
