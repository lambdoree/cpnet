(define-module (cpnet sexp-loader)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:use-module (cpnet dsl)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:export (load-system-from-sexp
            load-system-from-file))

(define (load-system-from-sexp sexp)
  (let* ((sys-name (cadr sexp))
         (sys (make-system sys-name))
         (clauses (cddr sexp)))
    (parameterize ((current-system sys))
      ;; First, apply the interface category if it exists.
      (let ((interface-clause (find (lambda (c) (eq? (car c) 'interface)) clauses)))
        (when interface-clause
          (let ((builder-name (cadr interface-clause)))
            (let ((builder (get-builder builder-name)))
              (if builder
                  ((builder-function builder)) ; Apply to current-system
                  (error "SEXP Loader: Unknown interface category builder" builder-name))))))

      ;; Then, process other clauses like subsystems and wires.
      (for-each
       (lambda (clause)
         (let ((clause-type (car clause)))
           (case clause-type
             ((interface)
              ;; Already handled, do nothing.
              #t)
             ((subsystems)
              (for-each
               (lambda (sub-def)
                 (let* ((real-def (car sub-def))
                        (instance-name (car real-def))
                        (builder-name (cadr real-def)))
                   (let ((builder (get-builder builder-name)))
                     (if builder
                         (add-subsystem! sys ((builder-function builder) instance-name))
                         (error "SEXP Loader: Unknown subsystem builder" builder-name)))))
               (cdr clause)))
             ((wires)
              (for-each
               (lambda (wire-def)
                 (let* ((real-wire-def (car wire-def))
                        (from-ref (car real-wire-def))
                        (arrow (cadr real-wire-def))
                        (to-ref (caddr real-wire-def)))
                   (if (eq? arrow '->)
                       (let ((from-cell (apply get-cell from-ref))
                             (to-cell (apply get-cell to-ref)))
                         (wire from-cell to-cell))
                       (format (current-error-port) "SEXP Loader: Malformed wire definition: ~a\n" wire-def))))
               (cdr clause)))
             (else
              (format (current-error-port) "SEXP Loader: Unknown clause type `~a` in system definition.\n" clause-type)))))
       clauses))
    sys))

(define (load-system-from-file file-path)
  (call-with-input-file file-path
    (lambda (port)
      (load-system-from-sexp (read port)))))
