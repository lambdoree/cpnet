(define-module (cpnet iteration)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:export (iteration-category))

(define-object Bool)
(define-object Data)

;; A generic, pure, two-state-variable iteration engine.
;; It is controlled by external logic that provides start values,
;; termination condition and the calculation for the next state.
(define-category iteration-category
  (objects
   ;; --- Inputs from Client ---
   (instance start_A Data #f replace-merge-fn)
   (instance start_B Data #f replace-merge-fn)
   (instance next_A Data #f replace-merge-fn)
   (instance next_B Data #f replace-merge-fn)
   (instance keep_running Bool #f replace-merge-fn)

   ;; --- Internal State: is_first_run orchestrates the data flow ---
   (instance current_A Data #f replace-merge-fn)
   (instance current_B Data #f replace-merge-fn)
   (instance is_first_run Bool #f replace-merge-fn)

   ;; --- Outputs to Client ---
   (instance final_A Data #f replace-merge-fn)
   (instance final_B Data #f replace-merge-fn))
  (morphisms
   ;; When start_A is set, it signals the beginning of a new run.
   ((morphism p-trigger-run (start_A) -> is_first_run)
    (lambda (vals _) (cons #t '())))

   ;; On the first run, initialize current state from start values.
   ((morphism p-initialize (is_first_run start_A start_B) -> (current_A current_B))
    (lambda (vals _)
      (if (car vals)
          (cons (list (cadr vals) (caddr vals)) '())
          (cons *nothing* '()))))

   ;; Feedback loop: on subsequent runs, update current state from next values.
   ((morphism p-feedback (is_first_run keep_running next_A next_B) -> (current_A current_B))
    (lambda (vals _)
      (let ((is-first (car vals)) (is-running (cadr vals)))
        (if (and (not is-first) is-running)
            (cons (list (caddr vals) (cadddr vals)) '())
            (cons *nothing* '())))))

   ;; Reset the first_run flag after initialization.
   ((morphism p-reset-first-run (is_first_run current_A) -> is_first_run)
    (lambda (vals _)
      (let ((is-first (car vals))
            (curr-a (cadr vals)))
        (if (and is-first (not (eq? curr-a #f)))
            (cons #f '())
            (cons *nothing* '())))))

   ;; When the loop terminates, propagate the final state to the output cells.
   ((morphism p-output-final (keep_running current_A current_B) -> (final_A final_B))
    (lambda (vals _)
      (if (not (car vals))
          (cons (list (cadr vals) (caddr vals)) '())
          (cons *nothing* '()))))))
