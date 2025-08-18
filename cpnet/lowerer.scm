(define-module (cpnet lowerer)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (cpnet core)
  #:use-module (cpnet log)
  #:use-module (cpnet system)
  #:use-module (cpnet category)
  #:use-module (icnu icnu)
  #:use-module (icnu stdlib icnu-lib)
  #:use-module (icnu rewrite)
  #:use-module (icnu tools icnu-inject)
  #:export (expand-icnu-helpers
            normalize-mk
            system->icnu-surface
            unquote-if-quoted
            ))

;; --- IC^nu helper expansion ---
(define *icnu-expanders* (make-hash-table))
(for-each
 (lambda (pair) (hash-set! *icnu-expanders* (car pair) (cdr pair)))
 `((IC_TRUE . ,IC_TRUE) (IC_FALSE . ,IC_FALSE) (IC_IF . ,IC_IF)
   (IC_CHURCH-ENCODE . ,IC_CHURCH-ENCODE) (IC_Y . ,IC_Y)
   (JOIN-REPLACE . ,JOIN-REPLACE) (JOIN-MAX . ,JOIN-MAX)
   (IC_LITERAL . ,IC_LITERAL) (IC_EQ_CONST . ,IC_EQ_CONST)
   (IC_LT_CONST . ,IC_LT_CONST) (IC_GT_CONST . ,IC_GT_CONST)
   (IC_AND . ,IC_AND) (IC_OR . ,IC_OR) (IC_NOT . ,IC_NOT) (IC_COPY . ,IC_COPY)
   (IC_PRIM_ADD . ,IC_PRIM_ADD) (IC_APPLY . ,IC_APPLY)
   (IC_MK_TRUE . ,IC_MK_TRUE) (IC_MK_FALSE . ,IC_MK_FALSE)
   (IC_CHURCH-RUN . ,IC_CHURCH-RUN) (IC_CHURCH-APPLY . ,IC_CHURCH-APPLY)
   (IC_CONS . ,IC_CONS) (IC_NIL . ,IC_NIL) (IC_FIRST . ,IC_FIRST)
   (IC_REST . ,IC_REST) (IC_FOLD . ,IC_FOLD)
   (mk-node . ,mk-node) (mk-wire . ,mk-wire) (mk-par . ,mk-par) (mk-nu . ,mk-nu)))

;; 주어진 `x`가 `(quote ...)` 형태이면 unquote하고, 아니면 그대로 반환합니다.
(define (unquote-if-quoted x)
  (if (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)))
      (cadr x)
      x))

;; ICν S-expression 폼을 재귀적으로 순회하며 `IC_*` 헬퍼 함수들을
;; 저수준의 원시 넷(net) 형태로 확장합니다.
(define (expand-icnu-helpers form)
  (letrec ((walk (lambda (f)
                   (if (not (pair? f))
                       f
                       (let* ((op (car f))
                              (args (cdr f))
                              (proc (hash-ref *icnu-expanders* op #f)))
                         (if proc
                             (walk (apply proc (map unquote-if-quoted args)))
                             (cons op (map walk args))))))))
    (walk form)))

;; S-expression 폼 내의 `mk-*` 헬퍼들을 `node`, `wire` 등 표준 형태로 정규화합니다.
(define (normalize-mk form)
  (cond
   ((not (pair? form)) form)
   ((null? form) '())
   ((and (symbol? (car form)) (eq? (car form) 'mk-node))
    (let ((name (cadr form)) (agent (caddr form)))
      (list 'node (normalize-mk name) (normalize-mk agent))))
   ((and (symbol? (car form)) (eq? (car form) 'mk-wire))
    (let ((a (cadr form)) (b (caddr form)))
      (list 'wire (normalize-mk a) (normalize-mk b))))
   ((and (symbol? (car form)) (eq? (car form) 'mk-par))
    (cons 'par (map normalize-mk (cdr form))))
   ((and (symbol? (car form)) (eq? (car form) 'mk-nu))
    (let ((raw-names (cadr form))
          (body (caddr form)))
      (let ((names-val (unquote-if-quoted raw-names)))
        (list 'nu (normalize-mk names-val) (normalize-mk body)))))
   ((and (symbol? (car form)) (eq? (car form) 'list))
    (if (and (>= (length form) 3)
             (let ((b (caddr form)))
               (and (pair? b) (eq? (car b) 'quote) (symbol? (cadr b)))))
        (let ((a (cadr form))
              (b (caddr form)))
          (list (normalize-mk a) (cadr b)))
        (cons 'list (map normalize-mk (cdr form)))))
   (else
    (cons (normalize-mk (car form)) (normalize-mk (cdr form))))))

;; propagator의 icnu 본문이 특별한 심볼 'out'을 참조하는지 확인합니다.
(define (body-has-out? body)
  (let ((found? #f))
    (letrec ((walk (lambda (form)
                     (when (not found?)
                       (cond
                        ((eq? form 'out) (set! found? #t))
                        ((pair? form) (walk (car form)) (walk (cdr form)))
                        (else #f))))))
      (walk body))
    found?))

;; icnu 본문 내의 'out' 심볼을 주어진 `target-sym`으로 치환합니다.
(define (subst-out-in-body body target-sym)
  (letrec ((walk (lambda (form)
                   (cond
                    ((eq? form 'out) target-sym)
                    ((pair? form) (cons (walk (car form)) (walk (cdr form))))
                    (else form)))))
    (walk body)))

;; 주어진 폼이 `(par ...)` 형태가 아니면 `(par ...)`로 감쌉니다.
(define (wrap-in-par form)
  (if (and (pair? form) (eq? (car form) 'par))
      form
      (cons 'par (if (pair? form) form (list form)))))

;; icnu 본문을 확장하고 정규화하는 과정을 하나로 묶은 헬퍼 함수입니다.
(define (expand-and-normalize body)
  (let* ((raw-par   (wrap-in-par body))
         (expanded  (expand-icnu-helpers raw-par))
         (normalized (normalize-mk expanded)))
    normalized))

;; 시스템의 모든 cell에 대해 값을 복사할 수 있는 C(copy) 노드 가젯들을 생성합니다.
(define (build-copy-gadgets system)
  (let ((frags '()))
    (let ((tables (system-get-cell-tables system)))
      (hash-for-each
       (lambda (_cat table)
         (hash-for-each
          (lambda (_k cell)
            (let* ((cid (cell-id cell))
                   (cnode (gensym (string-append (symbol->string cid) "-copy-"))))
              (set! frags
                    (cons
                     `(nu (,cnode)
                          (par (node ,cnode C)
                               (wire (,cid r) (,cnode p))
                               (wire (,cnode l) (,cid p))))
                     frags))))
          table))
       tables))
    (reverse frags)))

;; 정규화된 icnu 본문이 특정 대상 ID의 'r' 포트에 값을 쓰는지 확인합니다.
(define (body-writes-target-r? normalized target-id)
  (letrec ((recur
            (lambda (f)
              (match f
                (('wire (src spt) (tgt tpt))
                 (and (eq? tpt 'r) (eq? tgt target-id)))
                (('par . fs) (any recur fs))
                (('nu _ body) (recur body))
                (_ #f)))))
    (recur normalized)))

;; 1:1 propagator의 경우, 본문이 대상의 'r' 포트에 직접 쓰지 않으면,
;; 결과를 'r' 포트로 연결하는 `(nu ...)` 래퍼를 자동으로 추가합니다.
(define (make-out-nu-fragment-if-needed normalized target-id)
  (if (body-writes-target-r? normalized target-id)
      normalized
      (let ((fresh (gensym "out-")))
        (if (and (pair? normalized) (eq? (car normalized) 'par))
            `(nu (,fresh)
                 (par ,@(cdr normalized)
                      (wire (,fresh p) (,target-id r))))
            `(nu (,fresh)
                 (par ,normalized
                      (wire (,fresh p) (,target-id r))))))))

;; 단일 사상(morphism)을 저수준 ICν S-expression 조각으로 변환합니다.
(define (morphism->fragments mor)
  (let ((body (arrow-icnu-body mor))
        (cod  (arrow-cod mor)))
    (if body
        (let* ((body-forms (unquote-if-quoted body))
               (single-target-and-out?
                (and (not (list? cod)) (cell? cod) (body-has-out? body-forms)))
               (body*  (if single-target-and-out?
                           (subst-out-in-body body-forms (gensym "out-"))
                           body-forms))
               (norm   (expand-and-normalize `(par ,@body*))))
          (if single-target-and-out?
              (list (make-out-nu-fragment-if-needed norm (cell-id cod)))
              (list norm)))
        '())))

;; `(icnu ...)` 본문이 없는 propagator에 대해, 모든 소스에서 모든 타겟으로 `p` -> `p` 와이어를 연결하는
;; 폴백(fallback) 와이어링을 생성합니다.
(define (collect-fallback-wires mors)
  (let ((seen (make-hash-table))
        (ws '()))
    (for-each
     (lambda (mor)
       (let ((body (arrow-icnu-body mor)))
         (when (not body)
           (let* ((dom (arrow-dom mor))
                  (cod (arrow-cod mor))
                  (doms (if (list? dom) dom (list dom)))
                  (cods (if (list? cod) cod (list cod))))
             (for-each
              (lambda (d)
                (for-each
                 (lambda (c)
                   (when (and (cell? d) (cell? c))
                     (let* ((src (cell-id d))
                            (tgt (cell-id c))
                            (key (format #f "~a->~a" src tgt)))
                       (unless (or (equal? src tgt) (hash-ref seen key #f))
                         (hash-set! seen key #t)
                         (set! ws (cons `(wire (,src p) (,tgt p)) ws))))))
                 cods))
              doms)))))
     mors)
    (reverse ws)))

;; 시스템의 모든 사상들을 ICν 조각으로 변환하여 리스트로 수집합니다.
(define (collect-morphism-fragments mors)
  (apply append (map morphism->fragments mors)))

;; 정규화된 넷에서 `.r` 포트에 쓰는 모든 소스들을 수집합니다.
(define (collect-writers normalized)
  (let ((h (make-hash-table)))
    (letrec ((recur
              (lambda (f)
                (match f
                  (('wire (src spt) (tgt tpt))
                   (when (and (eq? spt 'p) (eq? tpt 'r))
                     (let* ((k (cons tgt 'r))
                            (xs (hash-ref h k '())))
                       (hash-set! h k (cons src xs)))))
                  (('par . fs) (for-each recur fs))
                  (('nu _ body) (recur body))
                  (_ #f)))))
      (recur normalized))
    h))

;; "단일 작성자" 규칙을 강제합니다: 하나의 `.r` 포트에는 여러 개의 소스가 쓸 수 없습니다.
;; 단, 모든 소스가 동일한 리터럴 값을 쓰는 경우는 휴리스틱하게 허용합니다.
(define (assert-single-writer! normalized where-label)
  (let ((h (collect-writers normalized))
        (net (parameterize ((*link-conflict-mode* 'overwrite-injection))
               (parse-net normalized))))
    (hash-for-each
     (lambda (k xs)
       (let ((uniq (delete-duplicates xs)))
         (when (> (length uniq) 1)
           (let ((resolved-vals (map (lambda (s) (resolve-literal-ep net (cons s 'p))) uniq)))
             (unless (and (not (find (lambda (v) (eq? v *unresolved*)) resolved-vals))
                        (let ((first (car resolved-vals)))
                          (every (lambda (x) (equal? x first)) (cdr resolved-vals))))
               (if (every (lambda (s) (is-literal-node? net s)) uniq)
                   (warnf "single-writer heuristic: (~a) multiple literal writers to ~a.r: ~a — allowing (heuristic)\n"
                          where-label (car k) uniq)
                   (error (format #f "single-writer violation (~a): ~a.r has multiple writers: ~a"
                                  where-label (car k) uniq))))))))
     h)))

;; 정규화된 넷에서 완전히 동일한 와이어 정의를 중복 제거합니다.
(define (dedupe-identical-wires normalized)
  (let ((seen (make-hash-table)))
    (define (stamp w)
      (match w
        (('wire (src spt) (tgt tpt))
         (format #f "~a.~a->~a.~a" src spt tgt tpt))
        (_ #f)))
    (define (walk f)
      (match f
        (('wire _ _)
         (let ((k (stamp f)))
           (if (and k (hash-ref seen k #f))
               '()
               (begin (when k (hash-set! seen k #t))
                      (list f)))))
        (('par . fs)
         (let ((items (append-map walk fs)))
           (list (cons 'par items))))
        (('nu ns body)
         (let* ((items (walk body))
                (body1 (cond
                         ((null? items) '(par))
                         ((null? (cdr items)) (car items))
                         (else (cons 'par items)))))
           (list (list 'nu ns body1))))
        (_ (list f))))
    (let* ((items (walk normalized)))
      (cond
        ((null? items) '(par))
        ((null? (cdr items)) (car items))
        (else (cons 'par items))))))

;; 전체 CPNet 시스템을 저수준의 ICν surface S-expression으로 변환하는 메인 함수입니다.
;; 이 함수는 lowering 과정의 핵심입니다.
(define (system->icnu-surface system)
  (let* ((cell-nodes '())
         (initial-values '())
         (_ (hash-for-each
             (lambda (_cat table)
               (hash-for-each
                (lambda (_k cell)
                  (set! cell-nodes (cons `(node ,(cell-id cell) A) cell-nodes))
                  (let ((val (cell-value cell)))
                    (when (not (eq? val *nothing*))
                      (set! initial-values (cons (cons (cell-id cell) val) initial-values)))))
                table))
             (system-get-cell-tables system)))
         (cell-nodes (reverse cell-nodes))
         (initial-val-frags (generate-injection-form (reverse initial-values)))
         (mors        (category-morphisms (system-get-net system)))
         (copy-gadgets (build-copy-gadgets system))
         (body-frags  (collect-morphism-fragments mors))
         (fallback-ws (collect-fallback-wires mors)))
    (letrec ((fix-form
              (lambda (f)
                (cond
                 ((not (pair? f)) f)
                 ((and (pair? f) (eq? (car f) 'wire))
                  (let* ((raw-e1 (cadr f))
                         (raw-e2 (caddr f))
                         (e1 (fix-form raw-e1))
                         (e2 (fix-form raw-e2)))
                    (let* ((sname (and (pair? e1) (car e1)))
                           (sport (and (pair? e1) (cadr e1)))
                           (tname (and (pair? e2) (car e2)))
                           (tport (and (pair? e2) (cadr e2))))
                      (if (and (symbol? tname)
                               (string-contains (symbol->string tname) ".")
                               (eq? tport 'r)
                               (not (eq? sport 'p)))
                          `(wire ,(list sname 'p) ,e2)
                          `(wire ,e1 ,e2)))))
                 (else (cons (fix-form (car f)) (fix-form (cdr f))))))))
      (let ((fixed-body-frags (map fix-form body-frags)))
        (let* ((initial-val-items
                (if (and (pair? initial-val-frags) (eq? 'par (car initial-val-frags)))
                    (cdr initial-val-frags)
                    (list initial-val-frags)))
               (net `(par
                      ,@initial-val-items
                      ,@cell-nodes
                      ,@copy-gadgets
                      ,@fallback-ws
                      ,@fixed-body-frags))
               (deduped-net (dedupe-identical-wires net)))
          (assert-single-writer! deduped-net "composed")
          deduped-net)))))
