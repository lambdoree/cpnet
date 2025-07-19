(define-module (cpnet runtime)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:use-module ((cpnet cpnet) :prefix cpnet:)
  #:export (
	    cpnet-runtime-settle!
	    cpnet-runtime-execute-effects
	    cpnet-runtime-show-state))

(define (cpnet-runtime-execute-effects effects)
  (for-each
   (lambda (effect)
     (case (cpnet:effect-type effect)
       ('display (display (cpnet:effect-payload effect)))
       (else (format #t "Unknown effect: ~a\n" (cpnet:effect-type effect)))))
   effects))

(define (cpnet-runtime-show-state C title)
  (display (format #f "\n--- [~a] cpnet state ---\n" title))
  (for-each
   (lambda (c)
     (display (format #f "Cell ~a: ~a\n"
                      (cpnet:cell-id c)
                      (cpnet:cell-value c))))
   (sort (cpnet:category-objects C)
         (lambda (a b)
           (string<?
             (symbol->string (cpnet:cell-id a))
             (symbol->string (cpnet:cell-id b))))))
  (display "--------------------------------\n"))

;; wrappers for public cpnet runtime interface
(define (cpnet-runtime-settle! C)
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
                 (let* ((src (cpnet:arrow-dom m))
                        (tgt (cpnet:arrow-cod m))
                        (src-val (cpnet:cell-value src)))
                   (when (and (not (eq? src-val #f))
                              (eq? (cpnet:cell-value tgt) #f))
                     (let ((result ((cpnet:arrow-fn m) src-val)))
                       (when (not (eq? (car result) #f))
                         (let ((current (hash-ref potential-updates tgt '())))
                           (hash-set! potential-updates tgt (cons result current))))))))
               (cpnet:category-morphisms C))

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
