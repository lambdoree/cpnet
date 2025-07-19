(define-module (cpnet cpnet)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet category)
  #:export (cell?
            make-cell
            cell-id
            cell-value
            cell-set-value!
            cell-merge-fn
            effect?
            make-effect
            effect-type
            effect-payload
            propagator?
            make-propagator
            propagator-id
            propagator-src
            propagator-tgt
            propagator-fn
            propagator-equal?
            propagator-compose
            propagator-id-fn
            execute-effects
            show-cpnet-state
            cpnet-settle!
            make-cpnet-category
            make-adder-constraint
            make-multiplier-constraint)
  )

(define-record-type cell
  (make-cell-record id value merge-fn)
  cell?
  (id       cell-id)
  (value    cell-value set-cell-value!)
  (merge-fn cell-merge-fn))

(define-record-type effect
  (make-effect type payload)
  effect?
  (type effect-type)
  (payload effect-payload))

(define-record-type propagator
  (make-propagator-record id src tgt fn)
  propagator?
  (id  propagator-id)
  (src propagator-src)
  (tgt propagator-tgt)
  (fn  propagator-fn))

(define (default-merge-fn cell new-vals)
  (let ((old-val (cell-value cell)))
    (cons old-val
          (list (make-effect 'display
                             (format #f "CONFLICT on cell ~a. Values: ~a. Keeping old value ~a.\n"
                                     (cell-id cell) new-vals old-val))))))

(define (make-cell id init-val . maybe-merge-fn)
  (let ((merge-fn (if (null? maybe-merge-fn)
                      default-merge-fn
                      (car maybe-merge-fn))))
    (make-cell-record id init-val merge-fn)))

(define (cell-set-value! c new-val)
  (set-cell-value! c new-val)
  c)


(define (make-propagator id src tgt fn)
  (make-propagator-record id src tgt fn))

(define (propagator-equal? p q)
  (eq? (propagator-id p) (propagator-id q)))

(define (propagator-compose g f)
  (unless (eq? (propagator-tgt f) (propagator-src g))
    (error "Cannot compose: cod(f) ≠ dom(g)"))
  (let* ((name (string->symbol
                (string-append
                 (symbol->string (propagator-id g))
                 "_o_"
                 (symbol->string (propagator-id f)))))
         (compose-fn (lambda (x)
                       (let* ((res-f ((propagator-fn f) x))
                              (val-y (car res-f))
                              (effects-f (cdr res-f)))
                         (if (eq? val-y #f)
                             (cons #f effects-f)
                             (let* ((res-g ((propagator-fn g) val-y))
                                    (val-z (car res-g))
                                    (effects-g (cdr res-g)))
                               (cons val-z (append effects-f effects-g))))))))
    (make-propagator name (propagator-src f) (propagator-tgt g) compose-fn)))

(define (propagator-id-fn cell)
  (let ((name (string->symbol
               (string-append "id-" (symbol->string (cell-id cell))))))
    (make-propagator name cell cell (lambda (x) (cons x '())))))

(define (safe-div x y)
  (if (zero? y)
      0
      (/ x y)))

(define (execute-effects effects)
  (for-each
   (lambda (effect)
     (case (effect-type effect)
       ('display (display (effect-payload effect)))
       (else (format #t "Unknown effect: ~a\n" (effect-type effect)))))
   effects))

(define (show-cpnet-state C title)
  (display (format #f "\n--- [~a] cpnet state ---\n" title))
  (for-each
   (lambda (c)
     (display (format #f "Cell ~a: ~a\n" (cell-id c) (cell-value c))))
   (sort (category-objects C) (lambda (a b) (string<? (symbol->string (cell-id a)) (symbol->string (cell-id b))))))
  (display "--------------------------------\n"))

(define (cpnet-settle! C)
  (let loop ((made-change? #t) (iter 0) (all-effects '()))
    (if (not made-change?)
        (reverse all-effects)
        (if (> iter 10)
            (begin
              (display "Warning: cpnet-settle! reached maximum iterations\n")
              (reverse all-effects))
            (let ((changed-this-pass? #f)
                  (effects-this-pass '())
                  (potential-updates (make-hash-table)))

              (for-each
               (lambda (m)
                 (let* ((src (propagator-src m))
                        (tgt (propagator-tgt m))
                        (src-val (cell-value src)))
                   (when (and (not (eq? src-val #f)) (eq? (cell-value tgt) #f))
                     (let ((result ((propagator-fn m) src-val)))
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

(define (make-cpnet-category objects morphisms)
  (make-category
   propagator-src propagator-tgt
   (lambda (g f) (propagator-compose g f))
   propagator-id-fn
   propagator-equal?
   propagator-id
   objects
   morphisms))

(define (make-adder-constraint cell-a cell-b cell-c)
  (let* ((id-a (cell-id cell-a)) (id-b (cell-id cell-b)) (id-c (cell-id cell-c)))
    (list
     (make-propagator (string->symbol (format #f "p-~a->~a" id-a id-c)) cell-a cell-c
      (lambda (a) (let ((b (cell-value cell-b))) (if b (cons (+ a b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-b id-c)) cell-b cell-c
      (lambda (b) (let ((a (cell-value cell-a))) (if a (cons (+ a b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-c id-a)) cell-c cell-a
      (lambda (c) (let ((b (cell-value cell-b))) (if b (cons (- c b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-b id-a)) cell-b cell-a
      (lambda (b) (let ((c (cell-value cell-c))) (if c (cons (- c b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-c id-b)) cell-c cell-b
      (lambda (c) (let ((a (cell-value cell-a))) (if a (cons (- c a) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-a id-b)) cell-a cell-b
      (lambda (a) (let ((c (cell-value cell-c))) (if c (cons (- c a) '()) (cons #f '())))))
     )))

(define (make-multiplier-constraint cell-a cell-b cell-c)
  (let ((id-a (cell-id cell-a)) (id-b (cell-id cell-b)) (id-c (cell-id cell-c)))
    (list
     (make-propagator (string->symbol (format #f "p-~a->~a" id-a id-c)) cell-a cell-c
      (lambda (a) (let ((b (cell-value cell-b))) (if b (let ((p (* a b))) (cons p (list (make-effect 'display (format #f "Multiplier: ~a computed as ~a\n" id-c p))))) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-b id-c)) cell-b cell-c
      (lambda (b) (let ((a (cell-value cell-a))) (if a (let ((p (* a b))) (cons p (list (make-effect 'display (format #f "Multiplier: ~a computed as ~a\n" id-c p))))) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-c id-a)) cell-c cell-a (lambda (c) (let ((b (cell-value cell-b))) (if b (cons (safe-div c b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-b id-a)) cell-b cell-a (lambda (b) (let ((c (cell-value cell-c))) (if c (cons (safe-div c b) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-c id-b)) cell-c cell-b (lambda (c) (let ((a (cell-value cell-a))) (if a (cons (safe-div c a) '()) (cons #f '())))))
     (make-propagator (string->symbol (format #f "p-~a->~a" id-a id-b)) cell-a cell-b (lambda (a) (let ((c (cell-value cell-c))) (if c (cons (safe-div c a) '()) (cons #f '())))))
    )))


