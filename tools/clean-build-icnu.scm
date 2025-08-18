(use-modules (ice-9 format)
             (srfi srfi-1)
             (icnu icnu)
             (icnu tools icnu-validate))

(define files
  ;; Primary generated artifacts to post-process. Add more paths here if needed.
  (list "build/SmartHomeSystem.icnu"
        "build/display-panel-system.icnu"
        "build/composed.icnu"))

;; Normalize forms:
;; - Replace occurrences of (list X (quote p))  -> (X p)
;; - Replace occurrences of (list X (quote r))  -> (X r)
;; - Replace occurrences of (list X (quote l))  -> (X l)
;; - Preserve other structured forms by walking recursively.
(define (normalize-form f)
  (cond
    ((null? f) '())
    ((not (pair? f)) f)
    ;; Handle printed literal-list forms: (list a (quote p)) -> (a p)
    ((and (symbol? (car f)) (eq? (car f) 'list))
     (let ((a (cadr f))
           (b (caddr f)))
       (cond
         ;; (list A (quote sym)) -> (A sym)
         ((and (pair? b) (eq? (car b) 'quote) (symbol? (cadr b)))
          (let ((sym (cadr b)))
            (list (normalize-form a) sym)))
         ;; (list A sym) -> (A sym)
         ((symbol? b) (list (normalize-form a) (normalize-form b)))
         ;; fallback: recursively normalize elements
         (else (cons 'list (map normalize-form (cdr f)))))))
    ;; Generic pair: normalize head and tail recursively
    (else (cons (normalize-form (car f)) (normalize-form (cdr f))))))

(define (process-file path)
  (format #t "Processing ~a ...\n" path)
  (unless (file-exists? path)
    (format #t "  Skipping: file not found.\n")
    (return-from process-file #f))
  (let ((sexpr (call-with-input-file path read)))
    (let ((normalized (normalize-form sexpr)))
      ;; Overwrite file with normalized S-expression
      (call-with-output-file path
        (lambda (out)
          (write normalized out)))
      ;; Try parsing and validating the resulting surface
      (parameterize ((*link-conflict-mode* 'overwrite-injection))
        (let ((ok #t))
          (with-handlers ((exn:fail:read?
                           (lambda (e)
                             (format #t "  ERROR: parse failed for ~a: ~a\n" path (exception-message e))
                             (set! ok #f)
                             #f))
                          (exn:fail:contract?
                           (lambda (e)
                             (format #t "  ERROR: parse/validation failure for ~a: ~a\n" path (exception-message e))
                             (set! ok #f)
                             #f)))
            (let ((net (parse-net normalized)))
              (if (validate-ir net)
                  (format #t "  OK: parsed and validated successfully.\n")
                  (begin
                    (format #t "  WARN: validation returned errors for ~a\n" path)
                    (set! ok #f))))))
      ok))))

;; Run over files list
(for-each process-file files)

(format #t "Post-processing complete.\n")
