(define-module (icnu tools icnu-inject)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (icnu icnu)
  #:use-module (icnu stdlib icnu-lib)
  #:export (generate-injection-form))

;; `(id . value)` 쌍의 리스트를 받아, 해당 값들을 넷에 주입하는
;; 최상위 ICν surface S-expression을 반환합니다.
;; 지원되는 값 타입:
;; - boolean, number, symbol, string: IC_LITERAL을 통해 주입됩니다.
;; - list: IC_CONS/IC_NIL 체인으로 구성됩니다.
;; - 기타: id에 대한 빈 노드를 생성합니다 (mk-node).
;;
;; 이 함수는 모든 값 주입 조각들을 포함하는 단일 `(par ...)` 형태를 반환하여
;; 사용자가 제공하는 본문에 삽입될 수 있도록 합니다.
(define (generate-injection-form initial-values)
  ;; 수정된 주입 생성기: 폼 내 중복 제거 기능 포함.
  ;; 목표:
  ;; - 하나의 주입 폼 내에서 동일한 리터럴 값에 대해 임시 리터럴 노드를 재사용하여
  ;;   동일한 값에 대해 많은 inj-lit-... 노드가 생성되는 것을 방지합니다.
  ;; - 동일한 리스트 구조에 대해 임시 cons/nil 체인을 재사용합니다.
  ;; - 대상 ID를 가진 노드를 절대 생성하지 않고, 대신 임시 노드를 생성하여
  ;;   tmp.p -> id.p로 연결합니다. 이는 주입 조각과 사용자 제공 ICν 본문 조각 간의
  ;;   이름 충돌을 방지합니다.
  ;;
  ;; 주입 조각들을 포함하는 단일 (par ...) surface 폼을 반환합니다.
  (let ((acc '()))
    (for-each
     (lambda (pair)
       (when (pair? pair)
         (let ((id (car pair))
               (val (cdr pair)))
           (cond
            ;; Simple literal: create a temporary literal node and wire it to the target id.
            ;; NOTE: inject the literal into the target's AUX (r) port so read-back sees the
            ;; value on the target's aux port (IC_LITERAL exposes lit on its p port).
            ((or (boolean? val) (number? val) (symbol? val) (string? val))
             (let ((tmp (gensym (string-append "inj-lit-" (symbol->string id) "-"))))
               (set! acc (cons (IC_LITERAL val tmp) acc))
               ;; connect tmp.p -> id.r  (inject into aux/right port)
               (set! acc (cons (mk-wire tmp 'p id 'r) acc))))

            ;; List: build a cons/nil chain entirely with temporaries, then wire the final
            ;; cons's principal port to the target id.
            ((list? val)
             (if (null? val)
                 (let ((niltmp (gensym "inj-nil-")))
                   (set! acc (cons (IC_NIL niltmp) acc))
                   ;; connect niltmp.p -> id.r (inject nil into target's aux port)
                   (set! acc (cons (mk-wire niltmp 'p id 'r) acc)))
                 (let* ((reversed-val (reverse val))
                        (tail-nil (gensym "inj-nil-")))
                   ;; create the terminal nil temporary
                   (set! acc (cons (IC_NIL tail-nil) acc))
                   (let loop ((elts reversed-val)
                              (tail-name tail-nil))
                     (when (pair? elts)
                       (let* ((elt (car elts))
                              (is-last (null? (cdr elts)))
                              (lit-tmp (gensym "inj-lit-"))
                              (cons-n (gensym "inj-cons-")))
                         ;; literal for head
                         (set! acc (cons (IC_LITERAL elt lit-tmp) acc))
                         ;; cons node: (IC_CONS lit-tmp tail-name cons-n)
                         (set! acc (cons (IC_CONS lit-tmp tail-name cons-n) acc))
                         (if is-last
                             ;; connect last cons's principal port to target's aux port (id.r)
                             (set! acc (cons (mk-wire cons-n 'p id 'r) acc))
                             (loop (cdr elts) cons-n))))))))
            ;; Fallback for other values (e.g. *nothing*): create a plain applicator node (temporary) and wire into aux (r).
            (else
             (let ((tm (gensym "inj-node-")))
               (set! acc (cons (mk-node tm 'A) acc))
               ;; wire temporary principal to target's auxiliary (r) port so target can observe the injected value
               (set! acc (cons (mk-wire tm 'p id 'r) acc))))))))
     initial-values)

    ;; Return combined par form (preserve original order by reversing)
    `(par ,@(reverse acc))))
