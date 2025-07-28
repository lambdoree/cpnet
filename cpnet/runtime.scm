(define-module (cpnet runtime)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 hash-table)
  #:use-module ((cpnet core) :prefix core:)
  #:use-module ((cpnet category) :prefix cat:)
  #:use-module (cpnet system)
  #:export (
	    runtime-settle!
	    runtime-step!
	    runtime-execute-effects
	    runtime-show-state))

(define (get-net system-or-net)
  (if (system? system-or-net)
      (system-get-net system-or-net)
      system-or-net))

(define (runtime-execute-effects effects)
  (let ((changed? #f))
    (for-each
     (lambda (effect)
       (case (core:effect-type effect)
         ((display) (display (core:effect-payload effect)))
         ((set-value)
          (let* ((payload (core:effect-payload effect))
                 (cell (car payload))
                 (new-val (cdr payload)))
            (when (not (equal? (core:cell-value cell) new-val))
              (core:cell-set-value! cell new-val)
              (set! changed? #t))))
         ((connect)
          #t)
         (else (format #t "Unknown effect: ~a\n" (core:effect-type effect)))))
     effects)
    changed?))

(define (runtime-show-state system-or-net title)
  (display (format #f "\n--- [~a] cpnet state ---\n" title))
  (if (system? system-or-net)
      (let* ((cell-tables (system-get-cell-tables system-or-net))
             (cat-names (sort (hash-map->list (lambda (k v) k) cell-tables)
                              (lambda (a b) (string<? (symbol->string a) (symbol->string b))))))
        (for-each
         (lambda (cat-name)
           (let* ((table (hash-ref cell-tables cat-name))
                  (cell-names (sort (hash-map->list (lambda (k v) k) table)
                                    (lambda (a b) (string<? (symbol->string a) (symbol->string b))))))
             (for-each
              (lambda (cell-name)
                (let ((cell (hash-ref table cell-name)))
                  (display (format #f "Cell ~a.~a: ~a\n"
                                   cat-name
                                   cell-name
                                   (core:cell-value cell)))))
              cell-names)))
         cat-names))
      (let ((C (get-net system-or-net)))
        (for-each
         (lambda (c)
           (display (format #f "Cell ~a: ~a\n"
                            (core:cell-id c)
                            (core:cell-value c))))
         (sort (cat:category-objects C)
               (lambda (a b)
                 (string<?
                  (symbol->string (core:cell-id a))
                  (symbol->string (core:cell-id b))))))))
  (display "--------------------------------\n"))

(define (_runtime-collect-updates C)
  (let ((effects '())
        (potential-updates (make-hash-table)))
    (for-each
     (lambda (m)
       (let* ((src (cat:arrow-dom m))
              (tgt (cat:arrow-cod m))
              (src-val (core:cell-value src)))
         (when (not (eq? src-val #f))
           (let ((result ((cat:arrow-fn m) src-val src)))
             (when (not (null? (cdr result)))
               (set! effects (append (cdr result) effects)))
             (when (not (eq? (car result) #f))
               (let ((current (hash-ref potential-updates tgt '())))
                 (hash-set! potential-updates tgt (cons (car result) current))))))))
     (cat:category-morphisms C))
    (values potential-updates effects)))

(define (_runtime-apply-updates potential-updates)
  (let ((changed? #f)
        (effects '()))
    (hash-for-each
     (lambda (cell values)
       (let* ((merge-fn (core:cell-merge-fn cell))
              (resolved-result (merge-fn cell values))
              (resolved-val (car resolved-result))
              (merge-effects (cdr resolved-result)))
         (set! effects (append merge-effects effects))
         (when (not (equal? (core:cell-value cell) resolved-val))
           (core:cell-set-value! cell resolved-val)
           (set! changed? #t))))
     potential-updates)
    (values changed? effects)))

(define (runtime-step! system-or-net)
  (let ((C (get-net system-or-net)))
    (let-values (((potential-updates propagator-effects) (_runtime-collect-updates C))
                 ((changed-by-merge? merge-effects) (_runtime-apply-updates potential-updates)))
      (values (append propagator-effects merge-effects)))))


(define (_runtime-handle-dynamic-connections C connect-effects connect-once-effects)
  (let ((changed? #f))
    (for-each
     (lambda (effect)
       (let* ((payload (core:effect-payload effect))
              (src (car payload)) (tgt (cdr payload)))
         (unless (eq? src tgt)
           (let ((p (core:make-propagator
                     (string->symbol (format #f "dyn-conn-~a->~a" (core:cell-id src) (core:cell-id tgt)))
                     src tgt
                     (lambda (v _) (cons v '())))))
             (unless (cat:category-has-morphism? C p)
               (cat:category-add-morphism C p)
               (set! changed? #t))))))
     connect-effects)
    (for-each
     (lambda (effect)
       (let* ((payload (core:effect-payload effect))
              (src (car payload)) (tgt (cdr payload)))
         (unless (eq? src tgt)
           (let ((p (core:make-propagator
                     (string->symbol (format #f "dyn-conn-once-~a->~a" (core:cell-id src) (core:cell-id tgt)))
                     src tgt
                     (lambda (v s) (cons v (list (core:make-effect 'set-value (cons s #f))))))))
             (unless (cat:category-has-morphism? C p)
               (cat:category-add-morphism C p)
               (set! changed? #t))))))
     connect-once-effects)
    changed?))

(define (runtime-settle! system-or-net)
  (let ((C (get-net system-or-net)))
    (let loop ((made-change? #t) (all-effects '()))
      (if (not made-change?)
          (reverse all-effects)
          (let-values (((potential-updates propagator-effects) (_runtime-collect-updates C)))
            (let-values (((changed-by-merge? merge-effects) (_runtime-apply-updates potential-updates)))
              (let* ((effects-this-pass (append propagator-effects merge-effects))
                     (is-connect? (lambda (e) (eq? (core:effect-type e) 'connect)))
                     (is-connect-once? (lambda (e) (eq? (core:effect-type e) 'connect-once)))
                     (connect-effects (filter is-connect? effects-this-pass))
                     (connect-once-effects (filter is-connect-once? effects-this-pass))
                     (other-effects (filter (lambda (e) (not (or (is-connect? e) (is-connect-once? e)))) effects-this-pass))
                     (changed-by-plain-effects? (runtime-execute-effects other-effects))
                     (network-changed-by-connect?
                      (_runtime-handle-dynamic-connections C connect-effects connect-once-effects)))
                (loop (or changed-by-merge? changed-by-plain-effects? network-changed-by-connect?)
                      (append effects-this-pass all-effects)))))))))
