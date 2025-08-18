(define-module (cpnet scenario)
  #:use-module (srfi srfi-11)
  #:use-module (cpnet core)
  #:use-module (cpnet dsl)
  #:use-module (cpnet lowerer)
  #:use-module (icnu icnu)
  #:use-module (icnu stdlib icnu-lib)
  #:use-module (cpnet runtime stepper)
  #:use-module (cpnet runtime state)
  #:export (run
            trigger
            *pending-triggers*
            *initial-icnu-surface*
            *cells-to-show*
            show-state
            ))

;; 실행 대기 중인 트리거(값 변경 요청)들을 저장하는 파라미터입니다.
(define *pending-triggers* (make-parameter '()))
;; 컴파일된 초기 ICν S-expression을 저장하는 파라미터입니다.
(define *initial-icnu-surface* (make-parameter #f))
;; `show-state`나 `run` 실행 시 출력할 cell들의 리스트를 저장하는 파라미터입니다.
(define *cells-to-show* (make-parameter '()))

;; 특정 cell의 값을 변경하라는 요청(트리거)을 대기열에 추가하는 매크로입니다.
(define-syntax-rule (trigger cell-expr val)
  (*pending-triggers* (cons (cons cell-expr val) (*pending-triggers*))))

;; 대기 중인 트리거들을 적용하여 시스템을 실행하고, 결과를 출력하는 매크로입니다.
(define-syntax run
  (syntax-rules ()
    ((_ title)
     (begin
       (let* ((base-sexp (*initial-icnu-surface*))
              (trigs     (reverse (*pending-triggers*)))
              (trigger-items
               (apply append
                      (map (lambda (p)
                             (let* ((cell (car p))
                                    (val  (cdr p))
                                    (cid  (cell-id cell))
                                    (tmp  (gensym "trig-lit-"))
                                    (frag (IC_LITERAL val tmp)))
                               (list frag
                                     `(wire (,tmp p) (,cid r)))))
                           trigs)))
              (base-items
               (if (and (pair? base-sexp) (eq? (car base-sexp) 'par))
                   (cdr base-sexp)
                   (list base-sexp)))
              (full-sexp (cons 'par (append base-items trigger-items)))
              (expanded-sexp (expand-icnu-helpers full-sexp))
              (normalized    (normalize-mk expanded-sexp))
              (net (parameterize ((*link-conflict-mode* 'overwrite-injection))
                     (parse-net normalized))))
         (let* ((title-str (if (string? title) title (symbol->string title)))
                (sanitized (list->string (filter (lambda (c) (not (memq c '(#\( #\) #\[ #\])))) (string->list title-str))))
                (outdir    (string-append "out/"
                                          (string-join (string-split sanitized #\space) "-"))))
           (system (string-append "mkdir -p " outdir))
           (stepper-run!   net outdir (*cells-to-show*))
           (emit-cpnet-state net title (*cells-to-show*)))
         (*pending-triggers* '()))))
    ((_)
     (run "Untitled Scenario"))))

;; 현재 런타임 넷의 상태를 지정된 메시지와 함께 출력하는 매크로입니다.
(define-syntax-rule (show-state msg)
  (emit-cpnet-state (current-runtime-net) msg (*cells-to-show*)))
