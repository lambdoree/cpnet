(define-module (cpnet log)
  #:use-module (ice-9 format)
  #:export (debug-level? set-debug-level! set-debug-log! debugf warnf debugf-limited debug-once))

;; 디버그 레벨 로깅 도입:
;; - *debug-level* 은 정수 수준 (0 = off). 기본 1 (조정: 기본 디버그를 켜서 런타임 진단을 쉽게 함).
;; - set-debug-log! 은 기존 호환용으로 boolean을 받아 레벨 0/1로 변환합니다.
;; - debugf(level, fmt, ...) 로 호출하면 현재 레벨 >= level 일 때 출력합니다.
;; 추가 유틸:
;; - debugf-limited : 키별로 최대 N회만 출력하도록 제한 (노이즈 많은 로그 제한)
;; - debug-once     : 키별로 한 번만 출력
(define *debug-level* (make-parameter 1))

;; 현재 디버그 레벨을 반환합니다.
(define (debug-level?) (*debug-level*))
;; 디버그 레벨을 설정합니다.
(define (set-debug-level! n) (*debug-level* n))
;; 호환성: 기존 set-debug-log! 호출을 지원 (boolean 또는 정수)
;; boolean 값을 받아 레벨 0 또는 1로 변환합니다.
(define (set-debug-log! v)
  (if (boolean? v)
      (set-debug-level! (if v 1 0))
      (set-debug-level! v)))

;; 현재 디버그 레벨이 주어진 레벨보다 높거나 같을 경우 디버그 메시지를 출력합니다.
(define (debugf level fmt . args)
  (when (>= (debug-level?) level)
    (apply format (current-output-port) fmt args)
    (force-output (current-output-port))))

;; 레벨 1의 디버그 메시지로 경고를 출력합니다.
(define (warnf fmt . args)
  ;; 기존 경고 메시지는 레벨 1로 취급
  (apply debugf (cons 1 (cons fmt args))))

;; 키별 출력 카운터 (문자열 키로 관리)
(define *debug-counts* (make-hash-table))

;; debugf-limited:
;; key : any serializable key (symbol/string)
;; limit : 최대 출력 회수 (정수)
;; level : debug 레벨 문맥
;; fmt . args : format 문자열/인자
;; 주어진 키에 대해 최대 N번까지만 디버그 메시지를 출력합니다.
(define (debugf-limited key limit level fmt . args)
  (when (>= (debug-level?) level)
    (let ((kstr (if (symbol? key) (symbol->string key) (format #f "~a" key))))
      (let ((cnt (hash-ref *debug-counts* kstr 0)))
        (when (< cnt limit)
          (hash-set! *debug-counts* kstr (+ cnt 1))
          (apply format (current-output-port) fmt args)
          (force-output (current-output-port)))))))

;; debug-once: 해당 key에 대해 한 번만 출력합니다.
(define (debug-once key level fmt . args)
  (apply debugf-limited (cons key (cons 1 (cons level (cons fmt args))))))

;; (set-debug-level! 2)
