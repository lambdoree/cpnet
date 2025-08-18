#!/usr/bin/env guile
!#
(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-11)
             (icnu icnu)
             (cpnet runtime stepper)
             (cpnet runtime state))

(define (usage)
  (display "Usage: guile -L . tools/run-icnu-file.scm -- <file.icnu> [--out <dir>] [--state-cells a,b,...]\n")
  (display "Example: guile -L . tools/run-icnu-file.scm -- composed.icnu --out out --state-cells SmartHomeSystem.home-automation.is_home,display-panel-system.alert_panel.siren_on\n"))

(define (parse-opts args)
  "간단한 옵션 파서: --file F, --out D, --state-cells CSV 또는 첫 번째 위치 인수를 파일로 처리."
  (let ((file #f) (outdir "out") (state-cells #f))
    (let loop ((rest args))
      (when (pair? rest)
        (let ((k (car rest))
              (v (and (pair? (cdr rest)) (cadr rest))))
          (cond
           ((or (string=? k "--file") (string=? k "-f"))
            (when v (set! file v))
            (loop (if (pair? (cdr rest)) (cddr rest) '())))
           ((string=? k "--out")
            (when v (set! outdir v))
            (loop (if (pair? (cdr rest)) (cddr rest) '())))
           ((string=? k "--state-cells")
            (when v
              (set! state-cells
                    (map string->symbol
                         (filter (lambda (s) (not (string-null? s)))
                                 (string-split v #\,)))))
            (loop (if (pair? (cdr rest)) (cddr rest) '())))
           ((string-prefix? "-" k)
            (display (format #f "Unknown option: ~a\n" k))
            (usage)
            (exit 1))
           (else
            ;; 위치 인수: 첫 미지정 값을 파일로 사용
            (when (not file) (set! file k))
            (loop (cdr rest)))))))
    (values file outdir state-cells)))

(define (main args)
  (let-values (((file outdir state-cells) (parse-opts args)))
    (unless file
      (usage)
      (exit 1))
    (format #t "Input IC^ν surface: ~a~%" file)
    (format #t "Outdir: ~a~%" outdir)
    (when state-cells
      (format #t "State cells to emit: ~a~%" state-cells))
    ;; Read & parse
    (let* ((sexpr (call-with-input-file file read))
           (net (parse-net sexpr)))
      ;; Ensure outdir exists
      (system (string-append "mkdir -p " outdir))
      ;; Call stepper; stepper-run! signature: (stepper-run! net outdir state-cells)
      ;; If state-cells is #f, the stepper will skip final-state emission.
      (stepper-run! net outdir state-cells)
      ;; Write final pretty-printed net for inspection
      (let ((final-path (string-append outdir "/final.icnu")))
        (call-with-output-file final-path
          (lambda (port)
            (write (pretty-print net '((show-nu? . #t))) port)))
        (format #t "Final net written to ~a~%" final-path)))))

;; Entrypoint: program-arguments contains args after "--" when invoked as:
;;   guile -L . tools/run-icnu-file.scm -- <args...>
(if (null? (program-arguments))
    (begin (usage) (exit 1))
    (main (program-arguments)))

;; 간단 실행 예시(스크립트 내부 주석):
;; guile -L . tools/run-icnu-file.scm -- composed.icnu --out out
;; guile -L . tools/run-icnu-file.scm -- composed.icnu --out out --state-cells SmartHomeSystem.home-automation.is_home,display-panel-system.alert_panel.siren_on
