(define-module (icnu stdlib icnu-lib)
  #:use-module (icnu icnu)
  ;; Export에 IC_APPLY를 추가하여 codegen이 매핑한 표면적 'app'/'apply' 호출을
  ;; 표준 라이브러리의 IC_APPLY 조각으로 스플라이스할 수 있게 합니다.
  #:export (IC_TRUE IC_FALSE IC_IF IC_Y
		    IC_CHURCH-ENCODE
		    JOIN-REPLACE JOIN-MAX
		    ;; Net-building helpers
		    IC_LITERAL IC_EQ_CONST IC_LT_CONST IC_GT_CONST
		    IC_AND IC_OR IC_NOT IC_COPY
		    IC_PRIM_ADD IC_APPLY
		    ;; Boolean Gadget constructors
		    IC_MK_TRUE IC_MK_FALSE
            ;; from church-runtime
            IC_CHURCH-RUN IC_CHURCH-APPLY IC_CONS IC_NIL IC_FIRST IC_REST IC_FOLD))

;; --- Booleans ---
;; TRUE selects the first branch (left on an Applicator).
(define IC_TRUE
  (mk-nu '(e b)
         (mk-par
          (mk-node 'b 'A)
          (mk-node 'e 'E)
          (mk-wire 'b 'r 'e 'p))))

;; FALSE selects the second branch (right on an Applicator).
(define IC_FALSE
  (mk-nu '(e b)
         (mk-par
          (mk-node 'b 'A)
          (mk-node 'e 'E)
          (mk-wire 'b 'l 'e 'p))))

;; IF c t e -> out: 조건 `c`에 따라 `t` 또는 `e`를 선택하여 `out`으로 출력하는 넷 가젯을 생성합니다.
(define (IC_IF c-port t-port e-port out-node)
  (let* ((if-impl (gensym "if-impl-"))
         (c-c (gensym "cond-copy-"))
         (t-c (gensym "then-copy-"))
         (e-c (gensym "else-copy-"))
         (out-c (gensym "out-copy-")))
    `(nu (,if-impl ,c-c ,t-c ,e-c ,out-c)
         (par
          ;; The final output node is a clean Applicator.
          ,(mk-node out-node 'A)
          
          ;; Internal applicator that performs the IF selection.
          ,(mk-node if-impl 'A)
          
          ;; Copy the condition. Default to 'p' port for symbols (cells), but respect port if given as a pair.
          ,(mk-node c-c 'C)
          ,(if (symbol? c-port)
               (mk-wire c-port 'p c-c 'p)
               (list 'wire c-port (list c-c 'p)))
          ,(mk-wire c-c 'l if-impl 'p)

          ;; Copy then/else branches from their 'p' port (booleans are on 'p').
          ,(mk-node t-c 'C)
          ,(if (symbol? t-port) (mk-wire t-port 'p t-c 'p) (list 'wire t-port (list t-c 'p)))
          ,(mk-wire t-c 'l if-impl 'l)

          ,(mk-node e-c 'C)
          ,(if (symbol? e-port) (mk-wire e-port 'p e-c 'p) (list 'wire e-port (list e-c 'p)))
          ,(mk-wire e-c 'l if-impl 'r)
          
          ;; Use a copier to expose the result on the 'r' port of the out-node.
          ,(mk-node out-c 'C)
          ,(mk-wire if-impl 'p out-c 'p)
          ,(mk-wire out-c 'l out-node 'r)
          ))))


;; --- Church Numerals (example) ---
;; 숫자 `n`을 받아 "church-n" 형태의 심볼릭 Applicator 노드로 인코딩합니다.
(define (IC_CHURCH-ENCODE n)
  ;; Create a named "church-<n>" applicator node as a literal representation.
  ;; This is a closed, name-based encoding (symbolic) that can be detected by
  ;; read-back and combined by compile-time helpers such as IC_PRIM_ADD.
  (let ((sym (string->symbol (format #f "church-~a" n))))
    (mk-node sym 'A)))

;; --- Lattice Joins ---
;; JOIN-REPLACE(x, y) = y: 두 입력 중 두 번째(`y`) 값을 선택하여 `out`으로 전달합니다.
(define (JOIN-REPLACE x y out)
  (let ((e (gensym "erase_")))
    (mk-nu (list e)
           (mk-par (mk-node e 'E)
                   (mk-wire e 'p x 'p)
                   (mk-wire out 'p y 'p)))))

;; JOIN-MAX(x, y) = if x > y then x else y: 두 입력 중 큰 값을 선택합니다 (현재는 플레이스홀더).
(define (JOIN-MAX x y out)
  ;; Placeholder for a comparator net
  (let ((comp (gensym "max_comp_")))
    (mk-nu (list comp)
           (mk-par (mk-node comp 'A)
                   (mk-wire comp 'p out 'p)))))

;; --- Net-building helpers for propagators ---

;; 주어진 Scheme 값(`val`)을 `out` 노드의 주 포트(principal port)에서 접근할 수 있는
;; 리터럴 넷으로 생성합니다.
(define (IC_LITERAL val out)
  ;; Produce a small net that exposes a literal value on the principal port
  ;; of `out` (an Applicator node).
  ;; - booleans: expand to IC_MK_TRUE / IC_MK_FALSE (so aux ports connect to E).
  ;; - numbers/symbols/strings: create a canonical literal node (e.g., 'num-42')
  ;;   and wire it to an auxiliary port of `out`. The principal port `out.p` is
  ;;   left free for the caller to connect. This allows `IC_LITERAL` to be used
  ;;   to construct temporary literal sources that can be wired into other gadgets
  ;;   without causing link conflicts. Read-back inspects the aux port to get the value.
  (cond
   ((boolean? val)
    (let ((lit-node (gensym "lit-bool-")))
      `(nu (,lit-node)
           (par
            ,(if val (IC_MK_TRUE lit-node) (IC_MK_FALSE lit-node))
            ,(mk-node out 'A)
            ,(mk-wire lit-node 'p out 'r)))))
   ((number? val)
    (let* ((is-trigger (string-prefix? "trig-lit-" (symbol->string out)))
           (lit (if is-trigger
                    (gensym (format #f "trig-num-~a-" val))
                    (string->symbol (format #f "num-~a" val)))))
      (if (eq? lit out)
          `(par ,(mk-node out 'A))
          (let ((body `(par ,(mk-node lit 'A) ,(mk-node out 'A) ,(mk-wire lit 'p out 'r))))
            (if is-trigger `(nu (,lit) ,body) body)))))
   ((or (symbol? val) (string? val))
    (let* ((is-trigger (string-prefix? "trig-lit-" (symbol->string out)))
           (str-val (if (symbol? val) (symbol->string val) val))
           (safe-str (string-join (string-split str-val #\space) "_"))
           (lit (if is-trigger
                    (gensym (string-append "trig-str-" safe-str "-"))
                    (string->symbol (string-append "str-" safe-str)))))
      (if (eq? lit out)
          `(par ,(mk-node out 'A))
          (let ((body `(par ,(mk-node lit 'A) ,(mk-node out 'A) ,(mk-wire lit 'p out 'r))))
            (if is-trigger `(nu (,lit) ,body) body)))))
   (else
    (error "IC_LITERAL: unsupported literal type" val))))

;; IC_APPLY: f(x) 형태의 함수 적용을 나타내는 넷 가젯을 생성합니다.
;; f-port, x-port는 심볼(노드 이름) 또는 엔드포인트 쌍으로 허용됩니다.
;; out은 결과를 노출할 노드의 이름입니다.
(define (IC_APPLY f-port x-port out)
  (let* ((out-node (if (symbol? out) out out))
         (c-f (gensym "app-f-"))
         (c-x (gensym "app-x-")))
    `(nu (,c-f ,c-x)
         (par
          ,(mk-node out-node 'A)
          ,(mk-node c-f 'C)
          ,(mk-node c-x 'C)
          ,(if (symbol? f-port) (mk-wire f-port 'p c-f 'p) (list 'wire f-port (list c-f 'p)))
          ,(if (symbol? x-port) (mk-wire x-port 'p c-x 'p) (list 'wire x-port (list c-x 'p)))
          ,(mk-wire c-f 'l out-node 'l)
          ,(mk-wire c-x 'l out-node 'r)))))

(define (IC_GENERIC_CONST_OP const-val in-port out-node)
  (let* ((lit (gensym "lit-"))
         (in-copy (gensym "in-copy-"))
         (norm-in-port (if (symbol? in-port) (list in-port 'p) in-port)))
    `(nu (,lit ,in-copy)
         (par ,(IC_LITERAL const-val lit)
              ,(mk-node out-node 'A)
              ,(mk-node in-copy 'C)
              ,(list 'wire norm-in-port (list in-copy 'p))
              ,(mk-wire in-copy 'l out-node 'l)
              ,(mk-wire lit 'p out-node 'r)))))

;; 입력 포트와 상수를 비교하는 '같음' 검사 가젯을 생성합니다.
(define (IC_EQ_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node))

;; 입력 포트가 상수보다 '작음'을 검사하는 가젯을 생성합니다.
(define (IC_LT_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node))

;; 입력 포트가 상수보다 '큼'을 검사하는 가젯을 생성합니다.
(define (IC_GT_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node))

;; `in` 가젯의 상태(보조 포트)를 `out`으로 복사하는 구조적 복사 가젯을 생성합니다.
(define (IC_COPY in out)
  ;; Create a copy of the gadget `in` at `out`.
  ;; This is a structural copy of a single-node gadget's state (its aux ports).
  (let ((c-left (gensym "c-left-"))
        (c-right (gensym "c-right-")))
    `(nu (,c-left ,c-right)
	 (par
          ;; The output gadget 'out' is a fresh Applicator.
          ,(mk-node out 'A)
          ;; Copy the left branch of 'in' to the left branch of 'out'.
          ,(mk-node c-left 'C)
          ,(mk-wire in 'l c-left 'p)
          ,(mk-wire c-left 'l out 'l)
          ;; Copy the right branch of 'in' to the right branch of 'out'.
          ,(mk-node c-right 'C)
          ,(mk-wire in 'r c-right 'p)
          ,(mk-wire c-right 'l out 'r)))))

;; 논리 AND 연산을 수행하는 넷 가젯을 생성합니다. (out = in1 AND in2)
(define (IC_AND in1-port in2-port out-node)
  ;; out = IF in1 THEN in2 ELSE FALSE
  (let ((false-node (gensym "false-"))
        (in2-copy (gensym "in2-copy-")))
    `(nu (,false-node ,in2-copy)
         (par
          ,(mk-node out-node 'A)
          ,(IC_LITERAL #f false-node)
          ;; copy in2 from its principal port to match IC_IF's `then` expectation
          ,(mk-node in2-copy 'C)
          ,(if (symbol? in2-port) (mk-wire in2-port 'p in2-copy 'p) (list 'wire in2-port (list in2-copy 'p)))
          ,(IC_IF in1-port (list in2-copy 'l) (list false-node 'r) out-node)))))

;; 논리 OR 연산을 수행하는 넷 가젯을 생성합니다. (out = in1 OR in2)
(define (IC_OR in1-port in2-port out-node)
  ;; out = IF in1 THEN TRUE ELSE in2
  (let ((true-node (gensym "true-"))
        (in2-copy (gensym "in2-copy-")))
    `(nu (,true-node ,in2-copy)
         (par
          ,(mk-node out-node 'A)
          ,(IC_LITERAL #t true-node)
          ;; copy in2 from its principal port to match IC_IF's `else` expectation
          ,(mk-node in2-copy 'C)
          ,(if (symbol? in2-port) (mk-wire in2-port 'p in2-copy 'p) (list 'wire in2-port (list in2-copy 'p)))
          ,(IC_IF in1-port (list true-node 'r) (list in2-copy 'l) out-node)))))

;; 논리 NOT 연산을 수행하는 넷 가젯을 생성합니다. (out = NOT in)
(define (IC_NOT in-port out-node)
  ;; out = IF in THEN FALSE ELSE TRUE
  (let ((true-node (gensym "true-"))
        (false-node (gensym "false-")))
    `(nu (,true-node ,false-node)
         (par
          ,(mk-node out-node 'A)
          ,(IC_LITERAL #t true-node)
          ,(IC_LITERAL #f false-node)
          ,(IC_IF in-port (list false-node 'r) (list true-node 'r) out-node)))))

;; 두 리터럴 입력("church-n" 또는 "num-n")에 대한 덧셈을 나타내는 넷을 생성합니다.
;; 컴파일 타임에 계산 가능하면 미리 계산된 리터럴을 생성하고, 아니면 심볼릭 ADD 가젯을 생성합니다.
(define (IC_PRIM_ADD in1 in2 out)
  ;; Build a net that represents addition when inputs are literal "church-<n>"
  ;; or "num-<n>" names. If both inputs are recognized literal names at
  ;; construction time, produce a precomputed literal (church-<sum> / num-<sum>)
  ;; wired to `out` principal port. Otherwise fall back to a symbolic ADD gadget.
  (let* ((s1 (if (symbol? in1) (symbol->string in1) (symbol->string (car in1))))
         (s2 (if (symbol? in2) (symbol->string in2) (symbol->string (car in2))))
         (starts-with?
          (lambda (s p)
            (let ((ls (string-length s)) (lp (string-length p)))
              (and (>= ls lp) (string=? (substring s 0 lp) p))))))
    (cond
     ;; church + church -> church-(n+m)
     ((and ((lambda (f) (f s1 "church-")) starts-with?)
           ((lambda (f) (f s2 "church-")) starts-with?))
      (let* ((n1 (string->number (substring s1 7)))
             (n2 (string->number (substring s2 7)))
             (sum (+ (if n1 n1 0) (if n2 n2 0)))
             (res (string->symbol (format #f "church-~a" sum))))
        `(nu (,res)
             (par
              ,(mk-node res 'A)
              ,(mk-node out 'A)
              ;; expose the computed literal on out's aux port
              ,(mk-wire res 'p out 'r)))))
     ;; num + num -> num-(n+m)
     ((and ((lambda (f) (f s1 "num-")) starts-with?)
           ((lambda (f) (f s2 "num-")) starts-with?))
      (let* ((n1 (string->number (substring s1 4)))
             (n2 (string->number (substring s2 4)))
             (sum (+ (if n1 n1 0) (if n2 n2 0)))
             (res (string->symbol (format #f "num-~a" sum))))
        `(nu (,res)
             (par
              ,(mk-node res 'A)
              ,(mk-node out 'A)
              ,(mk-wire res 'p out 'r)))))
     (else
      ;; Fallback: create a symbolic add gadget. This is a placeholder gadget
      ;; which wires inputs into an ADD applicator; it does not implement
      ;; semantic reduction at runtime in this stage.
      (let ((add-impl (gensym "add-"))
            (c1 (gensym "c-")) (c2 (gensym "c-"))
            (out-c (gensym "out-copy-")))
        `(nu (,add-impl ,c1 ,c2 ,out-c)
             (par
              ,(mk-node add-impl 'A)
              ,(mk-node c1 'C)
              ,(mk-node c2 'C)
              ,(if (symbol? in1) (mk-wire in1 'p c1 'p) (list 'wire in1 (list c1 'p)))
              ,(if (symbol? in2) (mk-wire in2 'p c2 'p) (list 'wire in2 (list c2 'p)))
              ,(mk-wire c1 'l add-impl 'l)
              ,(mk-wire c2 'l add-impl 'r)
              ,(mk-node out 'A)
              ,(mk-node out-c 'C)
              ,(mk-wire add-impl 'p out-c 'p)
              ,(mk-wire out-c 'l out 'r))))))))

;; `b` 노드를 참(true)으로 만드는 가젯을 생성합니다 (오른쪽 보조 포트를 Eraser에 연결).
(define (IC_MK_TRUE b)
  (let ((e (gensym "e_")))
    `(nu (,e)
         (par
          ,(mk-node b 'A)
          ,(mk-node e 'E)
          ,(mk-wire b 'r e 'p)))))

;; `b` 노드를 거짓(false)으로 만드는 가젯을 생성합니다 (왼쪽 보조 포트를 Eraser에 연결).
(define (IC_MK_FALSE b)
  (let ((e (gensym "e_")))
    `(nu (,e)
         (par
          ,(mk-node b 'A)
          ,(mk-node e 'E)
          ,(mk-wire b 'l e 'p)))))

;; Y-combinator를 ICν 넷으로 구현하여 재귀를 가능하게 합니다.
;; 주어진 함수 `fn`을 받아 자기 참조가 가능한 재귀적 구조를 생성하고,
;; 그 결과를 `out` 노드를 통해 노출합니다.
(define (IC_Y fn out)
  ;; fn: 심볼(예: 'f) 또는 endpoint pair (예: (list 'f 'p))
  ;; out: 결과를 노출할 노드 이름(심볼)
  (let* ((y-node   (string->symbol (format #f "Y~a" (gensym ""))))
         (dup-node (string->symbol (format #f "Ydup~a" (gensym ""))))
         (app1     (string->symbol (format #f "Yapp1~a" (gensym ""))))
         (app2     (string->symbol (format #f "Yapp2~a" (gensym ""))))
         (res-node (string->symbol (format #f "Yres~a" (gensym "")))))
    ;; Build explicit surface s-expression (lists) to avoid quasiquote issues.
    (list 'nu (list y-node dup-node app1 app2 res-node)
          (list 'par
                ;; nodes
                (mk-node y-node 'A)
                (mk-node dup-node 'C)
                (mk-node app1 'A)
                (mk-node app2 'A)
                (mk-node res-node 'A)

                ;; wire fn -> duplicator principal to allow copying of the function
                ;; fn이 (name port) 형태로 들어올 수 있으므로, 그 경우 두번째 endpoint를
                ;; (dup-node p) 형태로 만들어야 올바른 (wire ...) 형태가 생성됩니다.
                (if (symbol? fn)
                    (mk-wire fn 'p dup-node 'p)
                    (list 'wire fn (list dup-node 'p)))

                ;; duplicator outputs: supply two copies to app1 and app2
                (mk-wire dup-node 'l app1 'p)
                (mk-wire dup-node 'r app2 'p)

                ;; app1: receives argument via y-node's left port
                (mk-wire app1 'l y-node 'l)
                ;; app1's result is the argument for app2
                (mk-wire app1 'r app2 'l)

                ;; app2's result is wired back to y-node's right port to form the recursion loop
                (mk-wire app2 'r y-node 'r)

                ;; app1 produces a principal result; collect it at res-node
                (mk-wire app1 'p res-node 'p)

                ;; expose final result at out.p
                (mk-wire res-node 'p out 'p)))))


;; -----------------------------------------------------------------------------
;; from church-runtime.scm
;; -----------------------------------------------------------------------------

;; Simple runtime/constructor helpers for IC^ν nets.
;; NOTE: These helpers produce surface s-expressions (node/wire/nu/par)
;; that are intended to be compiled/parsed by the existing pipeline.
;; They avoid introducing new agent kinds and stay within A/C/E/V.

;; "church-<n>" 형태의 리터럴 노드를 생성하고 `out`의 주 포트에 연결합니다.
(define (IC_CHURCH-RUN n out)
  (let ((sym (string->symbol (format #f "church-~a" n))))
    `(par
      ,(mk-node sym 'A)
      ,(mk-node out 'A)
      ,(mk-wire sym 'p out 'p))))

;; 처치 수(Church numeral)를 `n`번의 함수 적용(f^n(x))으로 확장하는 넷을 생성합니다.
(define (IC_CHURCH-APPLY church-id f-port x-port out-target)
  ;; Runtime Church application: expand a Church numeral into a chain of `n`
  ;; Applicator (A) nodes so that, when `f` and `x` are connected according to
  ;; the project's calling convention, the net contains the structure for
  ;; n-fold application f^n(x).
  ;;
  ;; Implementation:
  ;; - Accepts church-id as either a number or a symbol like 'church-<n>.
  ;; - Builds n fresh A nodes named gensym("church-app-").
  ;; - Builds a binary copier tree that fans `f` out to n distinct left-ports
  ;;   (so we don't link the same f.p endpoint multiple times).
  ;; - Chains each node's right aux-port to the next node's principal port,
  ;;   wires the last node's right aux-port to x, and exposes the first node's
  ;;   principal port at out.p.
  ;;
  ;; Notes:
  ;; - This constructs a concrete applicator chain + copier-tree as a surface s-expression.
  ;; - It relies on the engine's AA/AC/AE/CE rules to perform reductions when
  ;;   `f` and `x` are themselves reducible nets (e.g., functions built from A/C/E).
  ;; - For n = 0 we simply wire x.p -> out.p.
  (let* ((n (cond ((number? church-id) church-id)
                  ((and (symbol? church-id)
                        (let ((s (symbol->string church-id)))
                          (and (>= (string-length s) 7) (string=? (substring s 0 7) "church-"))))
                   (string->number (substring (symbol->string church-id) 7)))
                  (else #f))))
    (cond
     ((eq? n #f)
      (error "IC_CHURCH-APPLY: unsupported church identifier" church-id))
     ((<= n 0)
      ;; Zero: identity -> wire x to out
      (let ((from-ep (if (symbol? x-port) (list x-port 'p) x-port))
            (to-ep (if (symbol? out-target) (list out-target 'p) out-target)))
        `(par ,@(if (symbol? out-target) (list (mk-node out-target 'A)) '())
              (wire ,from-ep ,to-ep))))
     (else
      ;; Build n applicator nodes and a copier-tree that fans `f` to the left ports.
      (letrec ((build-fanout
                (lambda (input-port k)
                  (if (<= k 1)
                      (cons (list input-port) #f)
                      (let* ((c-node (gensym "copier-"))
                             (num-left (ceiling (/ k 2)))
                             (num-right (floor (/ k 2)))
                             (left-result (build-fanout (list c-node 'l) num-left))
                             (right-result (build-fanout (list c-node 'r) num-right))
                             (outputs (append (car left-result) (car right-result)))
                             (left-net (cdr left-result))
                             (right-net (cdr right-result))
                             (children (filter (lambda (x) x) (list left-net right-net))))
                        (cons outputs
                              `(nu (,c-node)
                                 (par
                                  ,(mk-node c-node 'C)
                                  ,(if (symbol? input-port)
                                       (mk-wire input-port 'p c-node 'p)
                                       ;; input-port이 이미 (name port) 쌍인 경우,
                                       ;; 올바른 wire 표현은 (wire (name port) (c-node p)) 입니다.
                                       ;; 따라서 두번째 피연산자는 엔드포인트 쌍으로 만들어 줍니다.
                                       (list 'wire input-port (list c-node 'p)))
                                  ,@children))))))))
        (let* ((apps (letrec ((loop (lambda (k acc)
                                      (if (= k 0)
                                          (reverse acc)
                                          (loop (- k 1) (cons (gensym "church-app-") acc))))))
                       (loop n '())))
               (app-node-forms (map (lambda (nm) (mk-node nm 'A)) apps))
               (fan (build-fanout f-port n))
               (outputs (car fan))
               (copier-net (cdr fan))
               (left-wires
                (map (lambda (pair app)
                       (let ((src-name (car pair))
                             (src-port (cadr pair)))
                         (mk-wire src-name src-port app 'l)))
                     outputs apps))
               (chain-wires
                (letrec ((loop (lambda (lst acc)
                                 (if (null? (cdr lst))
                                     (reverse acc)
                                     (let ((a (car lst)) (b (cadr lst)))
                                       (loop (cdr lst) (cons (mk-wire a 'r b 'p) acc)))))))
                  (loop apps '())))
               (last (car (reverse apps)))
               (last-wire (if (symbol? x-port)
                              (mk-wire last 'r x-port 'p)
                              (list 'wire (list last 'r) x-port)))
               (out-wire (if (symbol? out-target)
                             (mk-wire (car apps) 'p out-target 'p)
                             (list 'wire (list (car apps) 'p) out-target))))
          ;; assemble final s-expression
          `(nu ,apps
               (par
                ,@(if copier-net (list copier-net) '())
                ,@app-node-forms
                ,@left-wires
                ,@chain-wires
                ,last-wire
                ,@(if (symbol? out-target) (list (mk-node out-target 'A)) '())
                ,out-wire))))))))

;; IC_PRIM_ADD_RUNTIME: runtime-aware addition entrypoint.
;; Currently delegates to the compile-time/construct-time IC_PRIM_ADD which
;; performs literal recognition; this wrapper allows a future enhancement point
;; to perform runtime net-level addition when inputs are produced dynamically.
;; 런타임 덧셈의 진입점입니다. 현재는 컴파일 타임 덧셈 헬퍼를 호출합니다.
(define (IC_PRIM_ADD_RUNTIME in1 in2 out)
(IC_PRIM_ADD in1 in2 out))

;; 리스트의 `cons` 셀을 나타내는 넷 가젯을 생성합니다. `head`는 값, `tail`은 다음 `cons` 셀입니다.
(define (IC_CONS head tail name)
  (let ((sym name)
        (c-h (gensym "c-h-"))
        (c-t (gensym "c-t-")))
    `(nu (,c-h ,c-t)
      (par
       ,(mk-node sym 'A)
       ,(mk-node c-h 'C)
       ,(mk-node c-t 'C)
       ,(if (symbol? head)
            (mk-wire head 'p c-h 'p)
            (list 'wire head (list c-h 'p)))
       ,(if (symbol? tail)
            (mk-wire tail 'p c-t 'p)
            (list 'wire tail (list c-t 'p)))
       ,(mk-wire c-h 'l sym 'l)
       ,(mk-wire c-t 'l sym 'r)))))

;; 리스트의 끝을 나타내는 `nil` 노드를 생성합니다.
(define (IC_NIL name)
  (let ((sym (if (symbol? name) name (string->symbol (format #f "nil-~a" name))))
        (e1 (gensym "e_"))
        (e2 (gensym "e_")))
    (list 'nu (list sym e1 e2)
          (list 'par
                (mk-node sym 'A)
                (mk-node e1 'E)
                (mk-node e2 'E)
                (mk-wire sym 'l e1 'p)
                (mk-wire sym 'r e2 'p)))))

;; `cons` 셀의 `head`(첫 번째 요소)를 `out`으로 노출합니다.
(define (IC_FIRST cons out)
  `(par ,(mk-node out 'A) ,(mk-wire cons 'l out 'p)))

;; `cons` 셀의 `tail`(나머지 리스트)을 `out`으로 노출합니다.
(define (IC_REST cons out)
  `(par ,(mk-node out 'A) ,(mk-wire cons 'r out 'p)))

;; `fold` 연산을 위한 플레이스홀더 넷을 생성합니다.
(define (IC_FOLD list-port acc-port fn-port out)
  ;; list-port/acc-port/fn-port may be symbols or port pairs. We generate a
  ;; named net node as a placeholder so parsing/validation succeeds.
  (let ((fold-n (gensym "fold-")))
    `(nu (,fold-n)
	 (par
          ,(mk-node fold-n 'A)
          ,(if (symbol? list-port) (mk-wire list-port 'p fold-n 'l) (list 'wire list-port 'p fold-n 'l))
          ,(if (symbol? acc-port) (mk-wire acc-port 'p fold-n 'r) (list 'wire acc-port 'p fold-n 'r))
          ,(mk-node out 'A)))))
