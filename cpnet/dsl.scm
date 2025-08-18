;; Lightweight, robust refactor of cpnet/dsl to ensure the module loads cleanly.
;; This file provides the DSL surface macros (propagator, define-category, ...)
;; while delegating instance creation to cpnet/dsl-instance and runtime IC^ν
;; evaluation to cpnet/runtime. The goal of this patch is to fix the
;; "no code for module (cpnet dsl)" load-time failure by providing a
;; syntactically-correct, balanced module that preserves the important
;; exported symbols used across the codebase.
(define-module (cpnet dsl)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (cpnet core)
  #:use-module (cpnet log)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime state)
  #:use-module (cpnet category)
  #:use-module (cpnet functor)
  #:use-module (cpnet nt)
  #:use-module (cpnet visualize)
  #:use-module (icnu tools icnu-inject)
  #:use-module (icnu rewrite)
  ;; bring core IC^ν helpers (parse-net / mk-node / mk-wire / ...) into scope
  #:use-module (icnu icnu)
  #:use-module (icnu stdlib icnu-lib)
  #:use-module (icnu tools icnu-validate)
  ;; instance creation helpers implemented in a focused module
  #:use-module (cpnet dsl-instance)
  #:use-module (cpnet lowerer)
  #:use-module (cpnet artifact)
  #:use-module (cpnet scenario)
  #:use-module (srfi srfi-11)
  #:export (define-category define-connections define-scenario
             define-cpnet-system compose-systems
             visualize
             effect-scope
             propagator
             get-cell-value set-cell-effect
             get-cell
             define-functor define-nt
             apply-functor-as-connections
             make-system-functor
             wire
             make-icnu-propagator-fn
	     current-runtime-net
	     ))

(define current-system (make-parameter #f))
(define current-runtime-net (make-parameter #f))

;; 주어진 icnu 본문, 소스, 타겟에 대한 propagator 함수를 생성합니다 (현재는 스텁).
(define (make-icnu-propagator-fn body src tgt) (lambda (vals srcs) (cons '() '())))
;; 이펙트를 로깅합니다 (현재는 스텁).
(define (log-effects scope effects) #f)

;; icnu 본문 내의 짧은 cell 이름들을 완전한 ID로 치환합니다.
(define (substitute-cell-names body table)
  (let ((subst-map (make-hash-table)))
    (hash-for-each
     (lambda (short-name cell)
       (hash-set! subst-map short-name (cell-id cell)))
     table)
    (substitute-symbols-in-body body subst-map)))


;; Instance creation macros moved to cpnet/dsl-instance.scm and imported
;; at module top via #:use-module (cpnet dsl-instance). This keeps this file
;; smaller and delegates the macro implementations to a focused module.

;; ---------- helpers ----------

;; 현재 시스템 컨텍스트에서 특정 카테고리의 cell 값을 가져옵니다.
(define (get-cell-value category-name cell-name)
  (let* ((sys (current-system)))
    (let ((cell (system-find-cell sys category-name cell-name)))
      (if cell
          (cell-value cell)
          (begin
            (format (current-output-port) "Warning: get-cell-value could not find ~a.~a\n" category-name cell-name)
            #f)))))

;; 특정 cell의 값을 설정하는 이펙트(effect)를 생성합니다.
(define (set-cell-effect category-name cell-name value)
  (let* ((sys (current-system)))
    (let ((cell (system-find-cell sys category-name cell-name)))
      (if cell
          (make-effect 'set-value (cons cell value))
          (error "set-cell-effect: cannot find cell" (list category-name cell-name))))))

(define-syntax-rule (effect-scope scope-name fn)
  (lambda (vals srcs)
    (let* ((res (fn vals srcs))
           (effects (cdr res)))
      (when (and effects (not (null? effects)))
        (log-effects 'scope-name effects))
      res)))

;; 다양한 경로 표기법을 사용해 현재 시스템에서 cell 객체를 찾습니다.
(define (get-cell system-or-cat-name . path)
  (let ((sys (current-system)))
    (if (system? system-or-cat-name)
        (let* ((subsystem system-or-cat-name)
               (cell-name (car (last-pair path)))
               (cat-path-parts (reverse (cdr (reverse path))))
               (mangled-cat-name (string->symbol
                                  (string-join
                                   (cons (symbol->string (system-name subsystem))
                                         (map symbol->string cat-path-parts))
                                   "."))))
          (or (system-find-cell sys mangled-cat-name cell-name)
              (error "get-cell: cell not found in subsystem" (system-name sys) mangled-cat-name cell-name)))
        (let* ((all-parts (cons system-or-cat-name path))
               (cell-name (car (last-pair all-parts)))
               (raw-cat-parts (reverse (cdr (reverse all-parts)))))
          (if (null? raw-cat-parts)
              (error "get-cell: not enough arguments" all-parts)
              (let* ((cat-parts (if (and (> (length raw-cat-parts) 1)
                                         (eq? (car raw-cat-parts) (system-name sys)))
                                    (cdr raw-cat-parts)
                                    raw-cat-parts))
                     (full-cat-name (if (= 1 (length cat-parts))
                                        (car cat-parts)
                                        (string->symbol (string-join (map symbol->string cat-parts) "."))))
                     (direct-cell (system-find-cell sys full-cat-name cell-name)))
                (if direct-cell
                    direct-cell
                    (let* ((unqualified-cat-name (car (last-pair cat-parts)))
                           (tables (system-get-cell-tables sys))
                           (suffix (string-append "." (symbol->string unqualified-cat-name)))
                           (found-pairs (filter (lambda (pair)
                                                  (let ((key-string (symbol->string (car pair))))
                                                    (and (> (string-length key-string) (string-length suffix))
                                                         (string-suffix? suffix key-string))))
                                                (hash-map->list cons tables))))
                      (cond
                       ((null? found-pairs) (error "get-cell: could not find cell" all-parts))
                       ((= 1 (length found-pairs)) (let ((tbl (cdr (car found-pairs)))) (hash-ref tbl cell-name #f)))
                       (else (error "get-cell: ambiguous category" unqualified-cat-name)))))))))))

(define-syntax propagator
  (syntax-rules (-> icnu lambda)
    [(_ pid src -> tgt (lambda . rest))
     (syntax-error "propagator: inline Scheme lambda propagators are removed from the DSL; use (icnu ...) bodies instead.")]
    [(_ pid src -> tgt (lambda . rest) priority)
     (syntax-error "propagator: inline Scheme lambda propagators are removed from the DSL; use (icnu ...) bodies instead.")]
    ;; Case: explicit icnu body with priority
    [(_ pid src -> tgt (icnu body ...) priority)
     (system-add-morphisms
      (current-system)
      (list
       (make-propagator
        'pid
        src
        tgt
        (make-icnu-propagator-fn '(body ...) src tgt)
        priority
        '(body ...))))]
    ;; Case: explicit icnu body without priority
    [(_ pid src -> tgt (icnu body ...))
     (system-add-morphisms
      (current-system)
      (list
       (make-propagator
        'pid
        src
        tgt
        (make-icnu-propagator-fn '(body ...) src tgt)
        '(body ...))))]
    ;; Fallback: keep existing behavior for plain function references with priority
    [(_ pid src -> tgt fn priority)
     (system-add-morphisms
      (current-system)
      (list
       (make-propagator
        'pid
        src
        tgt
        fn
        priority)))]
    ;; Fallback: keep existing behavior for plain function references without priority
    [(_ pid src -> tgt fn)
     (system-add-morphisms
      (current-system)
      (list
       (make-propagator
        'pid
        src
        tgt
        fn)))]))

;; ------------------------------------------------------------
;; 공통: 아리티별 모르피즘 생성기 (IC^ν body 포함)
;;  - priority 인자 유무를 모두 지원
;;  - 기존 make-propagator / make-icnu-propagator-fn 사용 그대로 유지
;; ------------------------------------------------------------

(define-syntax create-morphism-NM
  (syntax-rules (morphism -> quote icnu)
    ;; with priority
    [(_ (quote name-sym)
        (morphism pid (src ...) -> (tgt ...))
        (icnu body ...) priority table)
     (let* ((name-str (symbol->string 'name-sym))
            (pid-sym  'pid)
            (src-syms '(src ...))
            (tgt-syms '(tgt ...))
            (pid-val  (string->symbol (format #f "~a.~a" name-str pid-sym)))
            (src-cells (map (lambda (s) (or (hash-ref table s)
                                            (error "DSL: cell not found in category" s)))
                            src-syms))
            (tgt-cells (map (lambda (t) (or (hash-ref table t)
                                            (error "DSL: cell not found in category" t)))
                            tgt-syms))
            (subst-body (substitute-cell-names '(body ...) table)))
       (make-propagator pid-val
                        src-cells
                        tgt-cells
                        (make-icnu-propagator-fn subst-body src-syms tgt-cells)
                        priority
                        subst-body))]
    ;; without priority
    [(_ (quote name-sym)
        (morphism pid (src ...) -> (tgt ...))
        (icnu body ...) table)
     (let* ((name-str (symbol->string 'name-sym))
            (pid-sym  'pid)
            (src-syms '(src ...))
            (tgt-syms '(tgt ...))
            (pid-val  (string->symbol (format #f "~a.~a" name-str pid-sym)))
            (src-cells (map (lambda (s) (or (hash-ref table s)
                                            (error "DSL: cell not found in category" s)))
                            src-syms))
            (tgt-cells (map (lambda (t) (or (hash-ref table t)
                                            (error "DSL: cell not found in category" t)))
                            tgt-syms))
            (subst-body (substitute-cell-names '(body ...) table)))
       (make-propagator pid-val
                        src-cells
                        tgt-cells
                        (make-icnu-propagator-fn subst-body src-syms tgt-cells)
                        subst-body))]))

(define-syntax create-morphism-N1
  (syntax-rules (morphism -> quote icnu)
    [(_ (quote name-sym)
        (morphism pid (src ...) -> tgt)
        (icnu body ...) priority table)
     (let* ((name-str (symbol->string 'name-sym))
            (pid-sym  'pid)
            (src-syms '(src ...))
            (tgt-sym  'tgt)
            (pid-val  (string->symbol (format #f "~a.~a" name-str pid-sym)))
            (src-cells (map (lambda (s) (or (hash-ref table s)
                                            (error "DSL: cell not found in category" s)))
                            src-syms))
            (tgt-cell (or (hash-ref table tgt-sym)
                          (error "DSL: cell not found in category" tgt-sym)))
            (subst-body (substitute-cell-names '(body ...) table)))
       (make-propagator pid-val
                        src-cells
                        tgt-cell
                        (make-icnu-propagator-fn subst-body src-syms tgt-cell)
                        priority
                        subst-body))]
    [(_ (quote name-sym)
        (morphism pid (src ...) -> tgt)
        (icnu body ...) table)
     (let* ((name-str (symbol->string 'name-sym))
            (pid-sym  'pid)
            (src-syms '(src ...))
            (tgt-sym  'tgt)
            (pid-val  (string->symbol (format #f "~a.~a" name-str pid-sym)))
            (src-cells (map (lambda (s) (or (hash-ref table s)
                                            (error "DSL: cell not found in category" s)))
                            src-syms))
            (tgt-cell (or (hash-ref table tgt-sym)
                          (error "DSL: cell not found in category" tgt-sym)))
            (subst-body (substitute-cell-names '(body ...) table)))
       (make-propagator pid-val
                        src-cells
                        tgt-cell
                        (make-icnu-propagator-fn subst-body src-syms tgt-cell)
                        subst-body))]))

(define-syntax create-morphism-1N
  (syntax-rules (morphism -> quote icnu)
    [(_ (quote name-sym)
        (morphism pid src -> (tgt ...))
        (icnu body ...) priority table)
     (let* ((name-str (symbol->string 'name-sym))
            (pid-sym  'pid)
            (src-sym  'src)
            (tgt-syms '(tgt ...))
            (pid-val  (string->symbol (format #f "~a.~a" name-str pid-sym)))
            (src-cell (or (hash-ref table src-sym)
                          (error "DSL: cell not found in category" src-sym)))
            (tgt-cells (map (lambda (t) (or (hash-ref table t)
                                            (error "DSL: cell not found in category" t)))
                            tgt-syms))
            (subst-body (substitute-cell-names '(body ...) table)))
       (make-propagator pid-val
                        src-cell
                        tgt-cells
                        (make-icnu-propagator-fn subst-body src-sym tgt-cells)
                        priority
                        subst-body))]
    [(_ (quote name-sym)
        (morphism pid src -> (tgt ...))
        (icnu body ...) table)
     (let* ((name-str (symbol->string 'name-sym))
            (pid-sym  'pid)
            (src-sym  'src)
            (tgt-syms '(tgt ...))
            (pid-val  (string->symbol (format #f "~a.~a" name-str pid-sym)))
            (src-cell (or (hash-ref table src-sym)
                          (error "DSL: cell not found in category" src-sym)))
            (tgt-cells (map (lambda (t) (or (hash-ref table t)
                                            (error "DSL: cell not found in category" t)))
                            tgt-syms))
            (subst-body (substitute-cell-names '(body ...) table)))
       (make-propagator pid-val
                        src-cell
                        tgt-cells
                        (make-icnu-propagator-fn subst-body src-sym tgt-cells)
                        subst-body))]))

(define-syntax create-morphism-11
  (syntax-rules (morphism -> quote icnu)
    [(_ (quote name-sym)
        (morphism pid src -> tgt)
        (icnu body ...) priority table)
     (let* ((name-str (symbol->string 'name-sym))
            (pid-sym  'pid)
            (src-sym  'src)
            (tgt-sym  'tgt)
            (pid-val  (string->symbol (format #f "~a.~a" name-str pid-sym)))
            (src-cell (or (hash-ref table src-sym)
                          (error "DSL: cell not found in category" src-sym)))
            (tgt-cell (or (hash-ref table tgt-sym)
                          (error "DSL: cell not found in category" tgt-sym)))
            (subst-body (substitute-cell-names '(body ...) table)))
       (make-propagator pid-val
                        src-cell
                        tgt-cell
                        (make-icnu-propagator-fn subst-body src-sym tgt-cell)
                        priority
                        subst-body))]
    [(_ (quote name-sym)
        (morphism pid src -> tgt)
        (icnu body ...) table)
     (let* ((name-str (symbol->string 'name-sym))
            (pid-sym  'pid)
            (src-sym  'src)
            (tgt-sym  'tgt)
            (pid-val  (string->symbol (format #f "~a.~a" name-str pid-sym)))
            (src-cell (or (hash-ref table src-sym)
                          (error "DSL: cell not found in category" src-sym)))
            (tgt-cell (or (hash-ref table tgt-sym)
                          (error "DSL: cell not found in category" tgt-sym)))
            (subst-body (substitute-cell-names '(body ...) table)))
       (make-propagator pid-val
                        src-cell
                        tgt-cell
                        (make-icnu-propagator-fn subst-body src-sym tgt-cell)
                        subst-body))]))

;; 통합 디스패처: 소스/타깃 아리티와 priority 유무에 따라 위 매크로로 위임
(define-syntax create-morphism-from-stx
  (syntax-rules (morphism -> quote icnu)
    ;; (src ...) -> (tgt ...)
    [(_ (quote name-sym) ((morphism pid (src ...) -> (tgt ...)) (icnu body ...) priority) table)
     (create-morphism-NM (quote name-sym) (morphism pid (src ...) -> (tgt ...)) (icnu body ...) priority table)]
    [(_ (quote name-sym) ((morphism pid (src ...) -> (tgt ...)) (icnu body ...)) table)
     (create-morphism-NM (quote name-sym) (morphism pid (src ...) -> (tgt ...)) (icnu body ...) table)]

    ;; (src ...) -> tgt
    [(_ (quote name-sym) ((morphism pid (src ...) -> tgt) (icnu body ...) priority) table)
     (create-morphism-N1 (quote name-sym) (morphism pid (src ...) -> tgt) (icnu body ...) priority table)]
    [(_ (quote name-sym) ((morphism pid (src ...) -> tgt) (icnu body ...)) table)
     (create-morphism-N1 (quote name-sym) (morphism pid (src ...) -> tgt) (icnu body ...) table)]

    ;; src -> (tgt ...)
    [(_ (quote name-sym) ((morphism pid src -> (tgt ...)) (icnu body ...) priority) table)
     (create-morphism-1N (quote name-sym) (morphism pid src -> (tgt ...)) (icnu body ...) priority table)]
    [(_ (quote name-sym) ((morphism pid src -> (tgt ...)) (icnu body ...)) table)
     (create-morphism-1N (quote name-sym) (morphism pid src -> (tgt ...)) (icnu body ...) table)]

    ;; src -> tgt
    [(_ (quote name-sym) ((morphism pid src -> tgt) (icnu body ...) priority) table)
     (create-morphism-11 (quote name-sym) (morphism pid src -> tgt) (icnu body ...) priority table)]
    [(_ (quote name-sym) ((morphism pid src -> tgt) (icnu body ...)) table)
     (create-morphism-11 (quote name-sym) (morphism pid src -> tgt) (icnu body ...) table)]))

;; ------------------------------------------------------------
;; define-category: 얇게. 객체만 / 객체+모르피즘 두 케이스 지원
;;  - 시스템 컨텍스트가 있으면 그 시스템에 추가, 없으면 새 시스템 생성
;;  - 테이블 생성/등록, 인스턴스/모르피즘 등록, 최종 validate
;; ------------------------------------------------------------

(define-syntax define-category
  (syntax-rules (objects morphisms)
    ;; objects + morphisms
    [(_ name (objects inst-def ...) (morphisms mor-def ...))
     (begin
       (define (name . maybe-name)
         (let ((%body
                (lambda (sys)
                  (let ((table (make-hash-table)))
                    (system-add-cell-table sys 'name table)
                    (system-add-objects   sys (list (create-instance-from-stx 'name inst-def table) ...))
                    (system-add-morphisms sys (list (create-morphism-from-stx 'name mor-def table) ...))
                    (category-validate (system-get-net sys))))))
           (if (null? maybe-name)
               (let ((sys (current-system)))
                 (when (not sys)
                   (error "define-category called outside of a system context" 'name))
                 (parameterize ((current-system sys)) (%body sys)))
               (let ((new-system (make-system (car maybe-name))))
                 (parameterize ((current-system new-system)) (%body new-system))
                 new-system)))))]
    ;; objects only
    [(_ name (objects inst-def ...))
     (begin
       (define (name . maybe-name)
         (let ((%body
                (lambda (sys)
                  (let ((table (make-hash-table)))
                    (system-add-cell-table sys 'name table)
                    (system-add-objects   sys (list (create-instance-from-stx 'name inst-def table) ...))
                    (category-validate (system-get-net sys))))))
           (if (null? maybe-name)
               (let ((sys (current-system)))
                 (when (not sys)
                   (error "define-category called outside of a system context" 'name))
                 (parameterize ((current-system sys)) (%body sys)))
               (let ((new-system (make-system (car maybe-name))))
                 (parameterize ((current-system new-system)) (%body new-system))
                 new-system)))))]))

(define-syntax define-connections
  (syntax-rules (propagator connector)
    [(_ name)
     (define (name)
       (values))]
    [(_ name clause ...)
     (define (name)
       (begin clause ...))]))

(define-syntax-rule (define-scenario name . body)
  (define (name) (begin . body)))

(define-syntax define-cpnet-system
  (syntax-rules ()
    [(_ name . body)
     (begin
       (define (name . maybe-name)
         (let ((sys-name (if (null? maybe-name) 'name (car maybe-name))))
           (let ((new-system (make-system sys-name)))
             (parameterize ((current-system new-system))
               (let () . body))
             new-system)))
       (register-builder (make-category-builder 'name name '())))]))

(define-syntax compose-systems
  (syntax-rules (systems connections execution)
    [(_ (systems sys ...)
	(connections conn-proc ...)
	(execution . exec-procs))
     (let ((new-system (make-system 'composed)))
       (for-each (lambda (s) (add-subsystem! new-system s))
		 (list sys ...))
       (parameterize ((current-system new-system))
	 (begin conn-proc ...)
         (let* ((s-surf (system->icnu-surface new-system)))
           (*initial-icnu-surface* s-surf)
           (let* ((expanded-surf (expand-icnu-helpers s-surf))
                  (net (parameterize ((*link-conflict-mode* 'overwrite-injection))
                         (parse-net (normalize-mk expanded-surf)))))
             (current-runtime-net net)))
	 (begin . exec-procs))
       (begin
         (for-each
          (lambda (s-proc)
            (let ((s (if (procedure? s-proc) (s-proc) s-proc)))
	      (let* ((name-val (system-name s))
                     (name-str (cond ((symbol? name-val) (symbol->string name-val))
                                     ((string? name-val) name-val)
                                     (else (format #f "~a" name-val))))
                     (fname (string-append "build/" (if (and name-str (not (string-null? name-str))) name-str "anon") ".icnu")))
                (write-system-icnu s fname)
                (let* ((s-surf (system->icnu-surface s))
                       (expanded-surf (expand-icnu-helpers s-surf)))
                  (let ((net (parameterize ((*link-conflict-mode* 'overwrite-injection))
                               (parse-net (normalize-mk expanded-surf)))))
                    (unless (validate-ir net)
		      (format (current-output-port) "Warning: IC^ν validation returned errors for subsystem ~a\n" name-str)))))))
          (list sys ...))
         (write-system-icnu new-system (string-append "build/" (if (system-name new-system) (symbol->string (system-name new-system)) "composed") ".icnu"))
         (let* ((s-surf (system->icnu-surface new-system))
                (expanded-surf (expand-icnu-helpers s-surf)))
           (let ((net (parameterize ((*link-conflict-mode* 'overwrite-injection))
                        (parse-net (normalize-mk expanded-surf)))))
             (unless (validate-ir net)
	       (format (current-output-port) "Warning: IC^ν validation returned errors for composed system\n"))))
         new-system))]))

(define-syntax-rule (visualize path)
  (system->dot (current-system) path))

(define-syntax-rule (wire from-cell to-cell)
  (system-add-morphisms (current-system)
			(list
			 (make-propagator
			  (gensym "wire-")
			  from-cell
			  to-cell
			  (lambda (vals _) (cons (if (null? vals) *nothing* (car vals)) '()))))))

(define-syntax apply-functor-as-connections
  (syntax-rules (mappings ->)
    [(_ F (mappings (src -> tgt fn) ...))
     (let ((functor F))
       (let ((src-cat (functor-src-cat functor))
	     (tgt-cat (functor-tgt-cat functor))
	     (sys (current-system)))
	 ;; Verify that the implementation mappings are consistent with the functor definition
	 (let* ((clauses (list (cons 'src 'tgt) ...))
		(src-cat-name (system-find-category-name-for-cat sys src-cat))
		(tgt-cat-name (system-find-category-name-for-cat sys tgt-cat)))
	   (for-each
	    (lambda (clause)
	      (let* ((src-name (car clause))
		     (tgt-name (cdr clause))
		     (src-cell (system-find-cell sys src-cat-name src-name))
		     (tgt-cell (system-find-cell sys tgt-cat-name tgt-name))
		     (mapped-tgt-cell ((functor-obj-map functor) src-cell)))
		(unless (eq? tgt-cell mapped-tgt-cell)
		  (error "Inconsistent mapping in apply-functor-as-connections"
			 (format #f "Functor maps ~a to ~a, but implementation connects to ~a"
				 src-name (cell-id mapped-tgt-cell) (cell-id tgt-cell))))))
	    clauses))
	 ;; If consistent, create the propagators
	 (for-each
	  (lambda (src-obj)
	    (let* ((src-id (cell-id src-obj))
		   (tgt-obj ((functor-obj-map functor) src-obj)))
	      (when tgt-obj
		(let* ((full-id-str (symbol->string src-id))
		       (parts (string-split full-id-str #\.))
		       (short-id-sym (string->symbol (car (last-pair parts))))
		       (functor-name (functor-name functor)))
		  (let ((proc (cond ((eq? short-id-sym 'src) (wrap-icnu fn)) ...
				    (else #f))))
		    (when proc
		      (system-add-morphisms
		       (current-system)
		       (list
			(make-propagator
			 (string->symbol (format #f "functor-conn-~a-~a->~a"
						 (if functor-name functor-name "anon")
						 (cell-id src-obj)
						 (cell-id tgt-obj)))
			 src-obj
			 tgt-obj
			 proc)))))))))
	  (category-objects src-cat))))]))

(define-syntax define-functor
  (syntax-rules ()
    [(_ name from-cat to-cat
	(on-objects obj-fn)
	(on-morphisms mor-fn))
     ;; name: 심볼, from-cat/to- 카테고리 생성 함수
     (define name
       (let* ((C   (from-cat)) ; 실제 카테고리 인스턴스
	      (D   (to-cat))
	      (F   (make-functor-record C D obj-fn mor-fn)))
	 ;; 공리 검증
	 (for-each
	  (lambda (a)
	    ;; 단위 보존: F(id_x) = id_{F(x)}
	    (let ((id-a ((category-id-fn C) a))
		  (mapped-id-a (mor-fn id-a))
		  (id-fa ((category-id-fn D) (obj-fn a))))
	      (unless ((category-equal-fn D) mapped-id-a id-fa)
		(error 'functor-validate
		       (format #f "Functor ~a fails unit law at object ~a" 'name a)))))
	  (category-objects C))
	 (let ((all-morphisms (category-morphisms C)))
	   (for-each
	    (lambda (f)
	      (for-each
	       (lambda (g)
		 (when (equal? (arrow-cod f) (arrow-dom g))
		   (let* ((comp-gf (category-compose C g f))
			  (mapped-comp (mor-fn comp-gf))
			  (mapped-g (mor-fn g))
			  (mapped-f (mor-fn f))
			  (comp-mapped (category-compose D mapped-g mapped-f)))
		     (unless ((category-equal-fn D) mapped-comp comp-mapped)
		       (error 'functor-validate
			      (format #f "Functor ~a fails composition law on f=~a, g=~a"
				      'name f g))))))
	       all-morphisms))
	    all-morphisms))
	 F))]))

(define-syntax make-system-functor
  (syntax-rules (name from to mappings ->)
    [(_ (name functor-name) (from src-cat-name) (to tgt-cat-name) (mappings (src-cell-name -> tgt-cell-name) ...))
     (let ((functor (let* ((sys (current-system))
			   (tables (system-get-cell-tables sys))
			   (src-cat-table (hash-ref tables 'src-cat-name))
			   (tgt-cat-table (hash-ref tables 'tgt-cat-name))
			   (src-objs (if src-cat-table (hash-map->list (lambda (k v) v) src-cat-table) '()))
			   (tgt-objs (if tgt-cat-table (hash-map->list (lambda (k v) v) tgt-cat-table) '()))
			   (all-mors (category-morphisms (system-get-net sys)))
			   (src-mors (filter (lambda (m)
					       (let ((dom (arrow-dom m)) (cod (arrow-cod m)))
						 (and (if (list? dom) (every (lambda (c) (member c src-objs)) dom) (member dom src-objs))
						      (if (list? cod) (every (lambda (c) (member c src-objs)) cod) (member cod src-objs)))))
					     all-mors))
			   (tgt-mors (filter (lambda (m)
					       (let ((dom (arrow-dom m)) (cod (arrow-cod m)))
						 (and (if (list? dom) (every (lambda (c) (member c tgt-objs)) dom) (member dom tgt-objs))
						      (if (list? cod) (every (lambda (c) (member c tgt-objs)) cod) (member cod tgt-objs)))))
					     all-mors))
			   (src-cat (make-cpnet-category src-objs src-mors))
			   (tgt-cat (make-cpnet-category tgt-objs tgt-mors))
			   (cell-map (list
				      (cons (system-find-cell sys 'src-cat-name 'src-cell-name)
					    (system-find-cell sys 'tgt-cat-name 'tgt-cell-name))
				      ...)))
		      (make-cpnet-functor 'functor-name src-cat tgt-cat cell-map))))
       (system-add-functor! (current-system) functor)
       functor)]
    [(_ (from src-cat-name) (to tgt-cat-name) (mappings (src-cell-name -> tgt-cell-name) ...))
     (let ((functor (let* ((sys (current-system))
			   (tables (system-get-cell-tables sys))
			   (src-cat-table (hash-ref tables 'src-cat-name))
			   (tgt-cat-table (hash-ref tables 'tgt-cat-name))
			   (src-objs (if src-cat-table (hash-map->list (lambda (k v) v) src-cat-table) '()))
			   (tgt-objs (if tgt-cat-table (hash-map->list (lambda (k v) v) tgt-cat-table) '()))
			   (all-mors (category-morphisms (system-get-net sys)))
			   (src-mors (filter (lambda (m)
					       (let ((dom (arrow-dom m)) (cod (arrow-cod m)))
						 (and (if (list? dom) (every (lambda (c) (member c src-objs)) dom) (member dom src-objs))
						      (if (list? cod) (every (lambda (c) (member c src-objs)) cod) (member cod src-objs)))))
					     all-mors))
			   (tgt-mors (filter (lambda (m)
					       (let ((dom (arrow-dom m)) (cod (arrow-cod m)))
						 (and (if (list? dom) (every (lambda (c) (member c tgt-objs)) dom) (member dom tgt-objs))
						      (if (list? cod) (every (lambda (c) (member c tgt-objs)) cod) (member cod tgt-objs)))))
					     all-mors))
			   (src-cat (make-cpnet-category src-objs src-mors))
			   (tgt-cat (make-cpnet-category tgt-objs tgt-mors))
			   (cell-map (list
				      (cons (system-find-cell sys 'src-cat-name 'src-cell-name)
					    (system-find-cell sys 'tgt-cat-name 'tgt-cell-name))
				      ...)))
		      (make-cpnet-functor #f src-cat tgt-cat cell-map))))
       (system-add-functor! (current-system) functor)
       functor)]))

(define-syntax define-nt
  (syntax-rules (component)
    [(_ name F G (component obj-name mor-name) ...)
     (define name
       (let ((nt-F F) (nt-G G))
	 (let* ((C (functor-src-cat nt-F))
		(D (functor-tgt-cat nt-F))
		(components-alist
		 (let ((obj-map (make-hash-table)))
		   (for-each
		    (lambda (o)
		      (let* ((full-id-str (symbol->string (cell-id o)))
			     (parts (string-split full-id-str #\.))
			     (short-id-sym (string->symbol (car (last-pair parts)))))
			(hash-set! obj-map short-id-sym o)))
		    (category-objects C))
		   (list
		    (let* ((obj-name 'obj-name)
			   (mor-name 'mor-name)
			   (obj (hash-ref obj-map obj-name))
			   (mor (category-find-morphism-by-suffix D mor-name)))
		      (if (and obj mor)
			  (cons obj mor)
			  (error 'define-nt "Cannot find object or morphism for component" obj-name mor-name))) ...)))
		(η (make-nt-record 'name nt-F nt-G components-alist)))
	   ;; Naturality square validation / 자연성 사각형 검증
	   (for-each
	    (lambda (f)
	      (let* ((x (arrow-dom f))
		     (y (arrow-cod f))
		     (ηx-pair (assoc x (nt-components η)))
		     (ηy-pair (assoc y (nt-components η))))
		(when (and ηx-pair ηy-pair)
		  (let ((ηx (cdr ηx-pair))
			(ηy (cdr ηy-pair))
			(Gf ((functor-mor-map nt-G) f))
			(Ff ((functor-mor-map nt-F) f)))
		    (when (and Gf Ff)
		      (let ((lhs (category-compose D Gf ηx))
			    (rhs (category-compose D ηy Ff)))
			(unless ((category-equal-fn D) lhs rhs)
			  (error 'nt-validate
				 (format #f "NT ~a fails naturality at morphism ~a" 'name f)))))))))
	    (category-morphisms C))
	   (system-add-nt! (current-system) η)
	   η)))]))
