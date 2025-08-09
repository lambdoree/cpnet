(define-module (cpnet runtime)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:use-module ((cpnet core) :prefix core:)
  #:use-module ((cpnet category) :prefix cat:)
  #:use-module (cpnet system)
  #:export (
            current-system
            *oscillation-mode*
            *deterministic-execution*
	    runtime-settle!
	    runtime-step!
	    runtime-execute-effects
	    runtime-show-state
            *effect-log*
            clear-effect-log!
            log-effects
            transaction?
            tx-id
            tx-scope
            tx-effects))

(define current-system (make-parameter #f))
(define *oscillation-mode* (make-parameter 'warn)) ;; 'warn or 'error
(define *deterministic-execution* (make-parameter #t)) ;; #t for sorted, #f for shuffled

(define VALIDATE_ON_EFFECTS #t)
(define MAX_STEPS 1000)

(define-record-type <transaction>
  (make-tx id scope effects)
  transaction?
  (id tx-id)
  (scope tx-scope)
  (effects tx-effects))

(define *effect-log* '())

(define (clear-effect-log!)
  (set! *effect-log* '()))

(define (log-effects scope effects)
  (let ((tx (make-tx (gensym "tx-") scope effects)))
    (set! *effect-log* (cons tx *effect-log*))))

(define (get-net system-or-net)
  (if (system? system-or-net)
      (system-get-net system-or-net)
      system-or-net))

(define (runtime-execute-effects effects system)
  (let ((changed? #f)
        (activated-cells '()))
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
              (set! changed? #t)
              (set! activated-cells (cons cell activated-cells)))))
         ((set-cell)
           (let* ((payload (core:effect-payload effect))
                  (cell (car payload))
                  (new-val (cadr payload)))
             (when (not (equal? (core:cell-value cell) new-val))
               (core:cell-set-value! cell new-val)
               (set! changed? #t)
               (set! activated-cells (cons cell activated-cells)))))
         ((remove-subsystem)
          (system-remove-subsystem! system (core:effect-payload effect))
          (set! changed? #t))
         ((add-subsystem)
          (let* ((payload (core:effect-payload effect))
                 (name (car payload))
                 (cat-proc (cadr payload))
                 (sub-sys (cat-proc name)))
            (add-subsystem! system sub-sys)
            (set! changed? #t)))
         ((add-morphisms)
           (let* ((payload (core:effect-payload effect))
                  (resolve-ref (lambda (ref)
                                 (if (core:cell? ref) ref
                                     (let ((cat-name (car ref))
                                           (cell-name (cadr ref)))
                                       (or (system-find-cell system cat-name cell-name)
                                           (error "add-morphisms effect: could not resolve cell ref" ref))))))
                  (new-mors (map (lambda (desc)
                                   (let* ((src-refs (car desc))
                                          (tgt-refs (cadr desc))
                                          (fn (caddr desc))
                                          (src (core:map-maybe resolve-ref src-refs))
                                          (tgt (core:map-maybe resolve-ref tgt-refs)))
                                     (make-propagator (gensym "dyn-wire-") src tgt fn)))
                                 payload)))
             (system-add-morphisms system new-mors)
             (for-each
              (lambda (mor)
                (let ((dom (cat:arrow-dom mor)))
                  (if (list? dom)
                      (set! activated-cells (append dom activated-cells))
                      (when dom (set! activated-cells (cons dom activated-cells))))))
              new-mors)
             (set! changed? #t)))
         (else (format #t "Unknown effect: ~a\n" (core:effect-type effect)))))
     effects)
    (when (and changed? VALIDATE_ON_EFFECTS system)
      (unless (cat:category-validate (system-get-net system))
        (error "Category axiom violated after effects.")))
    (values changed? (delete-duplicates activated-cells))))

;; Fisher-Yates shuffle for lists
(define (list-shuffle lst)
  (let ((vec (list->vector lst)))
    (let loop ((i (- (vector-length vec) 1)))
      (if (> i 0)
          (let* ((j (random (+ i 1)))
                 (tmp (vector-ref vec i)))
            (vector-set! vec i (vector-ref vec j))
            (vector-set! vec j tmp)
            (loop (- i 1)))
          (vector->list vec)))))

(define (_runtime-snapshot system)
  (let* ((net (get-net system))
         (objs (cat:category-objects net))
         (sorted-objs (sort objs (lambda (a b) (string<? (symbol->string (core:cell-id a))
                                                           (symbol->string (core:cell-id b)))))))
    (map core:cell-value sorted-objs)))

(define (runtime-show-state system-or-net title)
  (display (format #f "\n--- [~a] cpnet state ---\n" title))
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
              (symbol->string (core:cell-id b)))))))
  (display "--------------------------------\n"))

(define (_runtime-collect-updates C active-cells)
  (let ((effects '())
        (potential-updates (make-hash-table))
        (executed-mors '()))
    (let* ((all-mors (cat:category-morphisms C))
           (compute-mors (filter (lambda (m)
                                   (let* ((id-sym (cat:arrow-id m))
                                          (id-str (symbol->string id-sym)))
                                     (not (string-contains id-str "id-"))))
                                 all-mors))
           (relevant-mors (if (not active-cells)
                              compute-mors
                              (filter (lambda (m)
                                        (let ((dom (cat:arrow-dom m)))
                                          (or (null? dom)
                                              (if (list? dom)
                                                  (any (lambda (d) (member d active-cells eq?)) dom)
                                                  (member dom active-cells)))))
                                      compute-mors)))
           (execution-order-mors (if (*deterministic-execution*)
                                     (sort relevant-mors
                                           (lambda (a b)
                                             (let ((pa (cat:arrow-priority a))
                                                   (pb (cat:arrow-priority b)))
                                               (if (= pa pb)
                                                   (string<? (symbol->string (cat:arrow-id a))
                                                             (symbol->string (cat:arrow-id b)))
                                                   (> pa pb)))))
                                     (list-shuffle relevant-mors))))
      (for-each
       (lambda (m)
         (let* ((srcs (cat:arrow-dom m))
                (tgts (cat:arrow-cod m)))
           ;; Source가 유효한지 확인. (리스트인 경우 #f가 없는지 확인)
           (let ((valid-sources?
                  (if (list? srcs)
                      (every identity srcs)
                      srcs)))
             (when valid-sources?
               (set! executed-mors (cons m executed-mors))
               (let* ((vals (if (list? srcs)
                                (map (lambda (s) (if (core:cell? s) (core:cell-value s) s)) srcs)
                                (list (if (core:cell? srcs) (core:cell-value srcs) srcs))))
                      (result ((cat:arrow-fn m) vals srcs))
                      (prop-val (car result))
                      (prop-effects (cdr result)))
                 (when (not (null? prop-effects))
                   (set! effects (append effects prop-effects)))
                 (when (not (eq? prop-val core:*nothing*))
                   (if (list? tgts)
                       (if (and (list? prop-val) (= (length prop-val) (length tgts)))
                           (for-each
                            (lambda (tgt v)
                              (when (and tgt (not (eq? v core:*nothing*)))
                                (let ((current (hash-ref potential-updates tgt '())))
                                  (hash-set! potential-updates tgt (cons v current)))))
                            tgts prop-val)
                           (error "Propagator output list length does not match targets" m))
                       (when tgts
                         (let ((current (hash-ref potential-updates tgts '())))
                           (hash-set! potential-updates tgts (cons prop-val current)))))))))))
       execution-order-mors))
    (values potential-updates effects (reverse executed-mors))))

(define (_runtime-apply-updates potential-updates)
  (let ((changed? #f)
        (effects '())
        (changed-cells '())
        (updates (hash-map->list cons potential-updates)))
    (for-each
     (lambda (update)
       (let* ((cell (car update))
              (values (cdr update))
              (merge-fn (core:cell-merge-fn cell))
              (resolved-result (merge-fn cell values))
              (resolved-val (car resolved-result))
              (merge-effects (cdr resolved-result)))
         (set! effects (append merge-effects effects))
         (when (not (equal? (core:cell-value cell) resolved-val))
           (core:cell-set-value! cell resolved-val)
           (set! changed-cells (cons cell changed-cells))
           (set! changed? #t))))
     updates)
    (values changed? effects changed-cells)))

(define (runtime-step! system-or-net active-cells)
  (let* ((system (if (system? system-or-net) system-or-net (core:cell-system (car (cat:category-objects system-or-net)))))
         (C (get-net system)))
    (let-values (((potential-updates propagator-effects executed-mors)
                  (_runtime-collect-updates C active-cells)))
      (let-values (((changed-by-merge? merge-effects changed-cells)
                    (_runtime-apply-updates potential-updates)))
        (let* ((effects-this-pass (append propagator-effects merge-effects)))
          (values changed-by-merge?
                  executed-mors
                  changed-cells
                  effects-this-pass))))))

(define (runtime-settle! system-or-net)
  (let ((all-effects '()))
    (let-values (((changed? initial-trace changed-cells effects) (runtime-step! system-or-net #f)))
      (set! all-effects (append all-effects effects))
      (let loop ((n 0)
                 (changed? changed?)
                 (full-trace initial-trace)
                 (active-cells changed-cells)
                 (history (list (_runtime-snapshot system-or-net))))
        (if (and changed? (< n MAX_STEPS))
            (let-values (((step-changed? step-trace step-changed-cells step-effects)
                          (runtime-step! system-or-net active-cells)))
              (set! all-effects (append all-effects step-effects))
              (if (not step-changed?)
                  ;; System has converged, terminate loop.
                  (loop (+ n 1) #f (append full-trace step-trace) '() history)
                  ;; System has changed, check for oscillation.
                  (let ((snapshot (_runtime-snapshot system-or-net)))
                    (if (member snapshot history equal?)
                        (let ((step-num (+ n 1)))
                          (cond
                           ((eq? (*oscillation-mode*) 'error)
                            (error 'oscillation-detected (format #f "at step ~a" step-num)))
                           ((eq? (*oscillation-mode*) 'warn)
                            (begin
                              (format (current-error-port) "Warning: runtime-settle! detected an oscillation at step ~a.\n" step-num)
                              (values (reverse (append full-trace step-trace)) all-effects)))
                           (else
                            (error 'unknown-oscillation-mode (*oscillation-mode*)))))
                        (loop (+ n 1)
                              step-changed?
                              (append full-trace step-trace)
                              step-changed-cells
                              (cons snapshot history))))))
            (begin
              (when (and changed? (>= n MAX_STEPS))
                (format (current-error-port) "Warning: runtime-settle! reached MAX_STEPS (~a) and did not converge.\n" MAX_STEPS))
              (values (reverse full-trace) all-effects)))))))
