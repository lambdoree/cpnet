(define-module (cpnet artifact)
  #:use-module (ice-9 format)
  #:use-module (cpnet lowerer)
  #:use-module (icnu icnu)
  #:use-module (icnu tools icnu-validate)
  #:use-module (cpnet system)
  #:export (validate-surface-sanity
            write-system-icnu))

;; 생성된 ICν surface S-expression에 대해 간단한 유효성 검사를 수행합니다.
;; 심각한 문제가 발견되면 빌드 실패를 명시적으로 알리기 위해 에러를 발생시킵니다.
(define (validate-surface-sanity sexpr sys-name)
  "Perform lightweight sanity checks on a generated IC^ν surface S-expression.
   Raises an error on serious issues to make build failures explicit.

   Checks performed:
   - there is at least one (nu ...) binder somewhere (we expect local scopes)
   - presence of at least one operational node name prefix (eq-const, lt-const,
     gt-const, if-impl, and-impl, or-impl, not-impl, add-prim) somewhere in the tree
   - presence of at least one 'l or 'r port usage (we expect auxiliary ports to be used)
"
  (let ((msgs '())
	(has-nu? #f)
	(has-op? #f)
	(has-lr? #f)
	(op-prefixes '("eq-const" "lt-const" "gt-const" "if-impl" "and-impl" "or-impl" "not-impl" "add-prim")))
    (letrec ((walk
	      (lambda (form)
		(cond
		 ((pair? form)
                  (when (and (not has-nu?) (eq? (car form) 'nu)) (set! has-nu? #t))
                  (walk (car form))
                  (walk (cdr form)))
		 ((symbol? form)
                  (let ((s (symbol->string form)))
                    ;; detect l/r ports
                    (when (or (string=? s "l") (string=? s "r"))
		      (set! has-lr? #t))
                    ;; detect any op prefix
                    (for-each
                     (lambda (pref)
		       (when (and (not has-op?) (string-prefix? pref s))
			 (set! has-op? #t)))
                     op-prefixes)))
		 (else #f)))))
      (walk sexpr))
    ;; Collect messages for missing properties
    (when (not has-nu?)
      (set! msgs (cons "No '(nu ...)' binder found in generated surface." msgs)))
    (when (not has-op?)
      (set! msgs (cons (format #f "No operation nodes found (prefixes: ~a)." op-prefixes) msgs)))
    (when (not has-lr?)
      (set! msgs (cons "No occurrences of auxiliary ports 'l' or 'r' found in wires/endpoints." msgs)))
    (if (null? msgs)
	#t
	(begin
          (format (current-output-port) "IC^ν Sanity check FAILED for system ~a:\n" sys-name)
          (for-each (lambda (m) (format (current-output-port) "  - ~a\n" m)) (reverse msgs))
          (format (current-output-port) "Hint: ensure the DSL->IC^ν lowerer expanded IC_* helpers into\n      node/wire/nu/par forms and that final target cells are wired into `.r` from `.p` principals.\n")
          (error 'icnu-sanity-failed (format #f "IC^ν sanity checks failed for ~a" sys-name))))))

;; 주어진 시스템(`sys`)에 대한 ICν surface s-expression을 `out-path`에 씁니다.
;; 생성된 코드가 유효한지 간단한 검사를 수행하고, 문제가 있으면 빌드를 실패시킵니다.
(define (write-system-icnu sys out-path)
  "Write IC^ν surface s-expression for `sys` to `out-path`.
   This now runs simple sanity checks (per feedback.md) and fails the build if
   the generated surface looks like a pure p->p wiring skeleton without
   the expected operation nodes / auxiliary port wiring."
  (let* ((s (system->icnu-surface sys))
         (expanded (expand-icnu-helpers s))
         (normalized (normalize-mk expanded))
         (name (or (system-name sys) "unnamed")))
    ;; Sanity-check the normalized surface and abort on serious problems.
    (validate-surface-sanity normalized name)
    ;; ensure build directory exists
    (system "mkdir -p build")
    (call-with-output-file out-path
      (lambda (port)
        ;; Write the normalized surface (expanded + normalized) so build/*.icnu
        ;; contains only primitive node/wire/par/nu forms (no IC_*/mk-* helpers
        ;; or quoted-list artifacts).
        (write normalized port)))
    out-path))
