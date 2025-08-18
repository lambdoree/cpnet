(define-module (icnu tools icnu-validate)
  #:use-module (icnu icnu)
  #:use-module (srfi srfi-1)
  #:export (validate-ir))

;; 주어진 넷(net)의 내부 표현(IR)이 구조적으로 올바른지 검증합니다.
;; - 모든 노드의 에이전트 타입이 유효한지 확인합니다.
;; - 모든 링크가 양방향으로 올바르게 설정되었는지 확인합니다.
(define (validate-ir net)
  (let ((errors '()))
    ;; Check 1: All nodes are A, C, or E (or V for free names).
    (hash-for-each
     (lambda (name agent)
       (unless (memq agent '(A C E V))
         (set! errors (cons `(invalid-agent ,name ,agent) errors))))
     (net-nodes net))

    ;; Check 2: links must be well-formed and reciprocal
    (let ((links (net-links net)))
      (hash-for-each
       (lambda (port peer)
         (unless (and (pair? port) (pair? peer))
           (set! errors (cons `(bad-link-format ,port ,peer) errors)))
         (let ((recip (hash-ref links peer #f)))
           (unless (equal? recip port)
             (set! errors (cons `(non-reciprocal-link ,port ,peer ,recip) errors)))))
       links))

    (if (null? errors)
        #t
        (begin
          (display "IR Validation Failed:\n")
          (for-each (lambda (e) (write e) (newline)) (reverse errors))
          #f))))
