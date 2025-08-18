(use-modules (ice-9 command-line)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (cpnet runtime stepper)
             (icnu icnu))

(define (main args)
  (let* ((net-path #f)
         (triggers-path #f)
         (sequence '())
         (outdir "out")
         (max-passes 16)
         (state-cells '()))
    ;; Minimal CLI parser: `args` is the list passed by the caller (program-arguments).
    (let loop ((rest args))
      (when (pair? rest)
        (let ((k (car rest))
              (v (and (pair? (cdr rest)) (cadr rest))))
          (cond
           ((or (string=? k "--net") (string=? k "-n"))
            (when v (set! net-path v))
            (loop (if (pair? (cdr rest)) (cddr rest) '())))
           ((string=? k "--triggers")
            (when v (set! triggers-path v))
            (loop (if (pair? (cdr rest)) (cddr rest) '())))
           ((string=? k "--sequence")
            (when v (set! sequence (string-split v #\,)))
            (loop (if (pair? (cdr rest)) (cddr rest) '())))
           ((string=? k "--out")
            (when v (set! outdir v))
            (loop (if (pair? (cdr rest)) (cddr rest) '())))
           ((string=? k "--max-passes")
            (when v (set! max-passes (string->number v)))
            (loop (if (pair? (cdr rest)) (cddr rest) '())))
           ((string=? k "--state-cells")
            ;; comma-separated list of fully-qualified symbol names, e.g.
            ;; --state-cells SmartHomeSystem.home-automation.is_home,display-panel-system.alert_panel.siren_on
            (when v
              (set! state-cells
                    (map string->symbol (filter (lambda (s) (not (string-null? s)))
                                                (string-split v #\,)))))
            (loop (if (pair? (cdr rest)) (cddr rest) '())))
           (else
            ;; Unknown/positional arg: skip it
            (loop (cdr rest)))))))

    (unless net-path
      (display "Error: --net is required.\n")
      (exit 1))

    (display (format #f "Net: ~a\n" net-path))
    (display (format #f "Outdir: ~a\n" outdir))

    (let* ((net-sexp (call-with-input-file net-path read))
           (main-net (parse-net net-sexp)))
      (system (string-append "mkdir -p " outdir))
      ;; Pass the optional state-cells list (or #f if none supplied) into the stepper.
      (stepper-run! main-net outdir (if (null? state-cells) #f state-cells))
      (let ((final-path (string-append outdir "/final.icnu")))
        (call-with-output-file final-path
          (lambda (port)
            (write (pretty-print main-net '((show-nu? . #t))) port)))
      (display (format #f "Final net written to ~a\n" final-path))))))

(main (program-arguments))
