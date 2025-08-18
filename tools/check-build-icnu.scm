(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-13)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (icnu icnu)
             (icnu tools icnu-validate))

(define files
  (list "build/SmartHomeSystem.icnu"
        "build/display-panel-system.icnu"
        "build/composed.icnu"))

(define forbidden-patterns
  ;; 텍스트 레벨에서 남아 있으면 안 되는 패턴들
  (list "IC_" "IC-" "mk-node" "mk-wire" "mk-par" "mk-nu" "(list " "(quote " "'quote" "(list" "(quote" "list "))

;; 안전하게 파일 전체를 문자열로 읽기
(define (slurp-file path)
  (call-with-input-file path get-string-all))

(define (contains-any? txt patterns)
  (let ((found '()))
    (for-each (lambda (pat)
                (when (and (string? txt) (string-contains txt pat))
                  (set! found (cons pat found))))
              patterns)
    (reverse found)))

(define (check-file path)
  (format #t "=== Checking file: ~a\n" path)
  (if (not (file-exists? path))
      (begin
        (format #t "  MISSING: file does not exist.\n\n")
        #f)
      (let* ((txt (slurp-file path))
             (bad (contains-any? txt forbidden-patterns)))
        (if (null? bad)
            (format #t "  Text-scan: OK (no obvious IC_/mk-/list/quote artifacts found)\n")
            (begin
              (format #t "  Text-scan: WARN - found forbidden textual patterns:\n")
              (for-each (lambda (p) (format #t "    - ~a\n" p)) bad)))
        ;; Try parsing S-expression and validating via parse-net + validate-ir
        (let ((ok #t))
          ;; NOTE: with-handlers removed due to environment issues.
          ;; Script will now halt on first parse/validation error.
          (let* ((sexpr (call-with-input-file path read))
                 (param-net (parameterize ((*link-conflict-mode* 'overwrite-injection))
                              (parse-net sexpr))))
            (if (validate-ir param-net)
                (format #t "  PARSE+VALIDATE: OK\n")
                (begin
                  (format #t "  PARSE+VALIDATE: FAIL (see messages above)\n")
                  (set! ok #f))))
          (format #t "\n")
          ok))))

;; Run checks and summarize
(let ((results (map check-file files)))
  (let ((all-ok (every identity results)))
    (if all-ok
        (format #t "Summary: All build/*.icnu passed basic checks.\n")
        (format #t "Summary: Some files reported warnings/errors above. Please inspect.\n")))
  (newline))
