(define-module (cpnet runtime state)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (icnu icnu)
  #:use-module (cpnet log)
  #:use-module (cpnet core)
  #:use-module (icnu rewrite)
  #:export (state-value-of
            emit-cpnet-state
            emit-cpnet-state-to-port))

;; Special sentinel used to represent "unspecified" in read/eval-safe way.
;; We avoid reader syntax like #<...> which Guile treats as an unknown object
;; during compilation; represent it as a normal symbol instead.
(define *unspecified* (string->symbol "#<unspecified>"))

;; The old literal-node? and literal->value helpers are now superseded by the
;; more robust `resolve-literal-ep` function imported from the rewrite engine.
;; 재작성 엔진의 리터럴 해석기(`resolve-literal-ep`)를 사용하여
;; 주어진 cell 심볼의 최종 값을 넷에서 읽어옵니다.
(define (state-value-of net cell-sym)
  "Resolve a 'value' for cell-sym by delegating to the rewrite engine's robust
   literal resolver. This provides a consistent view of values between the
   rewrite passes and the final state reader."
  (let* ((r-ep (cons cell-sym 'r))
         (p (peer net r-ep))
         (val (if p (resolve-literal-ep net p) *unresolved*)))
    (if (eq? val *unresolved*)
        *unspecified*
        val)))

;; 주어진 값을 사람이 읽기 좋은 문자열 형태로 변환합니다.
(define (pp-value v)
  (cond
   ((eq? v *unspecified*) "#<unspecified>")
   ((boolean? v) (if v "#t" "#f"))
   ((number? v) (number->string v))
   ((symbol? v) (symbol->string v))
   ((string? v) v)
   (else (format #f "~a" v))))

;; 지정된 cell들의 상태를 주어진 포트(port)로 출력합니다.
(define (emit-cpnet-state-to-port net title cells port)
  (format port "~a~%" (string-append "--- " title " --- cpnet state ---"))
  (for-each
   (lambda (c)
     (format port "~a: ~a~%" (symbol->string c) (pp-value (state-value-of net c))))
   cells)
  (format port "--------------------------------~%"))

;; 지정된 cell들의 상태를 현재 출력 포트(stdout)로 출력합니다.
(define (emit-cpnet-state net title cells)
  (emit-cpnet-state-to-port net title cells (current-output-port)))
