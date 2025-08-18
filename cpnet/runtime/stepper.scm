(define-module (cpnet runtime stepper)
  #:use-module (ice-9 match)
  #:use-module (cpnet log)
  #:use-module (icnu icnu)
  #:use-module (icnu rewrite)
  #:use-module (cpnet runtime state)
  #:use-module (srfi srfi-13)
  #:export (stepper-run!))

;; Allow other components to supply which cpnet cells are emitted as a
;; concise "final state" summary. By default the stepper does not assume any
;; domain-specific cell names; callers should pass an explicit list when desired.
(define *stepper-state-cells* '())

;; 재작성(rewrite) 스텝퍼를 고정점(fixed point)에 도달하거나 최대 패스 횟수에 이를 때까지 실행합니다.
;; 이 함수는 ICν 넷의 실제 계산을 수행하는 실행 엔진입니다.
(define (stepper-run! net outdir state-cells)
  ;; Run the rewrite stepper until a fixed point or max-passes.
  ;; Improvements:
  ;; - Temporarily raise debug level for clearer pass diagnostics.
  ;; - Increase max passes to allow multi-stage fold/if cascades.
  (let* ((old-debug (debug-level?))
         (max-passes 64)
         (cells (if state-cells state-cells *stepper-state-cells*)))
    (parameterize ()
      ;; Ensure we emit useful diagnostics for a normal run; callers can
      ;; still override global debug level if desired.
      (set-debug-level! 2)
      (debugf 1 "Stepper run started. Max passes: ~a\n" max-passes)
      (letrec ((loop (lambda (i changed-in-cycle)
                       (if (and changed-in-cycle (< i max-passes))
                           (begin
                             (debugf 2 "--- Pass ~a ---\n" (+ i 1))
                             ;; when outdir is provided, save snapshot (IC^ν S-expression)
                             (when outdir
                               (let ((path (format #f "~a/~a-pass-start.icnu"
                                                   outdir
                                                   (let ((s (number->string (+ i 1))))
                                                     (string-append (make-string (max 0 (- 3 (string-length s))) #\0) s)))))
                                 (call-with-output-file path
                                   (lambda (p) (write (pretty-print net (list (cons 'show-nu? #t))) p)))))

                             ;; Execute passes in a stable order. Run semantic folds (const, if)
                             ;; before structural cleanups (copy-fold) to avoid breaking gadgets
                             ;; that the semantic passes rely on.
                             (let* ((c_const (rewrite-pass-const-fold! net))
                                    (c_if (rewrite-pass-if-fold! net))
                                    (c_A (rewrite-pass-A! net))
                                    (c_copy (rewrite-pass-copy-fold! net))
                                    (c_wire (rewrite-pass-wire-cleanup! net))
                                    (c_merge (rewrite-pass-AA-merge! net))
                                    (any-change? (or c_const c_if c_A c_copy c_wire c_merge)))
                               (debugf 2 "Pass ~a changes: CONST:~a, IF:~a, A:~a, C:~a, WIRE:~a, MERGE:~a\n"
                                       (+ i 1) c_const c_if c_A c_copy c_wire c_merge)
                               (loop (+ i 1) any-change?)))
                           (begin
                             (if (>= i max-passes)
                                 (warnf "Stepper warning: max passes (~a) reached.\n" max-passes)
                                 (debugf 1 "Stepper run finished in ~a passes.\n" i))
                             ;; when outdir requested, emit end-of-pass snapshot
                             (when outdir
                               (let ((path (format #f "~a/~a-pass-end.icnu"
                                                   outdir
                                                   (let ((s (number->string (+ i 1))))
                                                     (string-append (make-string (max 0 (- 3 (string-length s))) #\0) s)))))
                                 (call-with-output-file path
                                   (lambda (p) (write (pretty-print net (list (cons 'show-nu? #t))) p)))))
                             ;; Emit a concise cpnet state summary if a non-empty `cells` list is provided.
                             (when (and outdir (not (null? cells)))
                               (let ((state-path (format #f "~a/final-state.txt" outdir)))
                                 (call-with-output-file state-path
                                   (lambda (port)
                                     (emit-cpnet-state-to-port net "Final cpnet state" cells port)))))

                             ;; restore caller's debug level
                             (set-debug-level! old-debug)
                             net)))))
        (loop 0 #t)))))
