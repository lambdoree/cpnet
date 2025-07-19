(define-module (cpnet runtime)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet cpnet)
  #:export (runtime-settle!
            runtime-execute-effects
            runtime-show-state))

(define (runtime-execute-effects effects)
  (for-each
   (lambda (effect)
     (case (effect-type effect)
       ('display (display (effect-payload effect)))
       (else (format #t "Unknown effect: ~a\n" (effect-type effect)))))
   effects))

(define (runtime-show-state C title)
  (display (format #f "\n--- [~a] cpnet state ---\n" title))
  (for-each
   (lambda (c)
     (display (format #f "Cell ~a: ~a\n" (cell-id c) (cell-value c))))
   (sort (category-objects C) (lambda (a b) (string<? (symbol->string (cell-id a)) (symbol->string (cell-id b))))))
  (display "--------------------------------\n"))

(define (runtime-settle! C)
  (let loop ((made-change? #t) (iter 0) (all-effects '()))
    (if (not made-change?)
        (reverse all-effects)
        (if (> iter 10)
            (begin
              (display "Warning: runtime-settle! reached maximum iterations\n")
              (reverse all-effects))
            (let ((changed-this-pass? #f)
                  (effects-this-pass '())
                  (potential-updates (make-hash-table)))

              (for-each
               (lambda (m)
                 (let* ((src (arrow-dom m))
                        (tgt (arrow-cod m))
                        (src-val (cell-value src)))
                   (when (and (not (eq? src-val #f)) (eq? (cell-value tgt) #f))
                     (let ((result ((arrow-fn m) src-val)))
                       (when (not (eq? (car result) #f))
                         (let ((current (hash-ref potential-updates tgt '())))
                           (hash-set! potential-updates tgt (cons result current))))))))
               (category-morphisms C))

              (hash-for-each
               (lambda (cell updates)
                 (let* ((values (map car updates))
                        (propagator-effects (apply append (map cdr updates)))
                        (merge-fn (cell-merge-fn cell)))
                   (set! effects-this-pass (append propagator-effects effects-this-pass))

                   (if (null? (cdr updates))
                       (let ((val-to-set (car values)))
                         (cell-set-value! cell val-to-set)
                         (set! changed-this-pass? #t))
                       (let* ((resolved-result (merge-fn cell values))
                              (resolved-val (car resolved-result))
                              (merge-effects (cdr resolved-result)))
                         (set! effects-this-pass (append merge-effects effects-this-pass))
                         (when (not (eq? (cell-value cell) resolved-val))
                           (cell-set-value! cell resolved-val)
                           (set! changed-this-pass? #t))))))
               potential-updates)

              (loop changed-this-pass?
                    (+ 1 iter)
                    (append effects-this-pass all-effects)))))))
