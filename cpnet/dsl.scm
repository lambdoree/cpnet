(define-module (cpnet dsl)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:use-module (cpnet category)
  #:use-module (cpnet functor)
  #:use-module (cpnet nt)
  #:use-module (cpnet visualize)
  #:use-module (srfi srfi-11)
  #:export (define-category define-connections define-scenario
             define-cpnet-system compose-systems
             visualize
             define-object
             effect-scope
	     propagator
	     trigger run show-state
             current-system
	     get-cell-value set-cell-effect
             get-cell
	     define-functor define-nt
             apply-functor-as-connections
             make-system-functor
             wire
             add-subsystem!))

(define (syntax->list stx)
  (syntax-case stx ()
    (() '())
    ((x . xs) (cons #'x (syntax->list #'xs)))
    (_ (syntax-violation 'syntax->list "not a proper list" stx))))

(define current-system (make-parameter #f))

(define (get-cell-value category-name cell-name)
  (let* ((sys (current-system)))
    (let ((cell (system-find-cell sys category-name cell-name)))
      (if cell
          (cell-value cell)
          (begin
            (format (current-error-port) "Warning: get-cell-value could not find ~a.~a\n" category-name cell-name)
            #f)))))

(define (set-cell-effect category-name cell-name value)
  (let* ((sys (current-system)))
    (let ((cell (system-find-cell sys category-name cell-name)))
      (if cell
          (make-effect 'set-value (cons cell value))
          (error "set-cell-effect: cannot find cell" (list category-name cell-name))))))

(define-syntax-rule (define-object name) (begin))

(define-syntax-rule (effect-scope cat-name-sym fn) fn)

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
               (cat-parts (reverse (cdr (reverse all-parts)))))
          (if (null? cat-parts)
              (error "get-cell: not enough arguments" all-parts)
              (let* ((full-cat-name (if (= 1 (length cat-parts))
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
  (lambda (stx)
    (syntax-case stx (->)
      [(_ pid src -> tgt fn priority)
       #`(system-add-morphisms
          (current-system)
          (list
           (make-propagator
            'pid
            src
            tgt
            fn
            priority)))]
      [(_ pid src -> tgt fn)
       #`(system-add-morphisms
          (current-system)
          (list
           (make-propagator
            'pid
            src
            tgt
            fn)))])))


(define-syntax create-instance-from-stx
  (lambda (stx)
    (syntax-case stx (instance quote)
      ;; with merge function
      [(_ (quote name-sym) (instance id type-name init merge-fn) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(id-sym 'id)
		(type-sym 'type-name)
		(init-val init)
		(merge-fn merge-fn)
		(cid-val (string->symbol (format #f "~a.~a" name-str id-sym)))
		(c (make-cell cid-val type-sym init-val merge-fn)))
           (hash-set! table id-sym c)
           c)]
      ;; without merge function
      [(_ (quote name-sym) (instance id type-name init) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(id-sym 'id)
		(type-sym 'type-name)
		(init-val init)
		(merge-fn default-merge-fn)
		(cid-val (string->symbol (format #f "~a.~a" name-str id-sym)))
		(c (make-cell cid-val type-sym init-val merge-fn)))
           (hash-set! table id-sym c)
           c)])))

(define-syntax create-morphism-from-stx
  (lambda (stx)
    (syntax-case stx (morphism -> effect-scope quote)
      ;; N:M with priority
      [(_ (quote name-sym) ((morphism pid (src ...) -> (tgt ...)) (effect-scope fn) priority) table)
       #`(let* ((name-str (symbol->string 'name-sym)) (pid-sym 'pid) (src-syms '(src ...)) (tgt-syms '(tgt ...))
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cells (map (lambda (s) (or (hash-ref table s) (error "DSL: cell not found in category" s))) src-syms))
		(tgt-cells (map (lambda (t) (or (hash-ref table t) (error "DSL: cell not found in category" t))) tgt-syms)))
	   (make-propagator pid-val src-cells tgt-cells (effect-scope 'name-sym fn) priority))]
      [(_ (quote name-sym) ((morphism pid (src ...) -> (tgt ...)) fn priority) table)
       #`(let* ((name-str (symbol->string 'name-sym)) (pid-sym 'pid) (src-syms '(src ...)) (tgt-syms '(tgt ...))
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cells (map (lambda (s) (or (hash-ref table s) (error "DSL: cell not found in category" s))) src-syms))
		(tgt-cells (map (lambda (t) (or (hash-ref table t) (error "DSL: cell not found in category" t))) tgt-syms)))
	   (make-propagator pid-val src-cells tgt-cells fn priority))]
      ;; N:M
      [(_ (quote name-sym) ((morphism pid (src ...) -> (tgt ...)) (effect-scope fn)) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(pid-sym 'pid)
		(src-syms '(src ...))
		(tgt-syms '(tgt ...))
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cells (map (lambda (s) (or (hash-ref table s) (error "DSL: cell not found in category" s))) src-syms))
		(tgt-cells (map (lambda (t) (or (hash-ref table t) (error "DSL: cell not found in category" t))) tgt-syms)))
	   (make-propagator pid-val src-cells tgt-cells (effect-scope 'name-sym fn)))]
      [(_ (quote name-sym) ((morphism pid (src ...) -> (tgt ...)) fn) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(pid-sym 'pid)
		(src-syms '(src ...))
		(tgt-syms '(tgt ...))
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cells (map (lambda (s)
                                  (or (hash-ref table s)
                                      (error "DSL: cell not found in category" s)))
				src-syms))
		(tgt-cells (map (lambda (t)
                                  (or (hash-ref table t)
                                      (error "DSL: cell not found in category" t)))
				tgt-syms)))
	   (make-propagator pid-val src-cells tgt-cells fn))]
      ;; N:1 with priority
      [(_ (quote name-sym) ((morphism pid (src ...) -> tgt) (effect-scope fn) priority) table)
       #`(let* ((name-str (symbol->string 'name-sym)) (pid-sym 'pid) (src-syms '(src ...)) (tgt-sym 'tgt)
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cells (map (lambda (s) (or (hash-ref table s) (error "DSL: cell not found in category" s))) src-syms))
		(tgt-cell (or (hash-ref table tgt-sym) (error "DSL: cell not found in category" tgt-sym))))
           (make-propagator pid-val src-cells tgt-cell (effect-scope 'name-sym fn) priority))]
      [(_ (quote name-sym) ((morphism pid (src ...) -> tgt) fn priority) table)
       #`(let* ((name-str (symbol->string 'name-sym)) (pid-sym 'pid) (src-syms '(src ...)) (tgt-sym 'tgt)
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cells (map (lambda (s) (or (hash-ref table s) (error "DSL: cell not found in category" s))) src-syms))
		(tgt-cell (or (hash-ref table tgt-sym) (error "DSL: cell not found in category" tgt-sym))))
           (make-propagator pid-val src-cells tgt-cell fn priority))]
      ;; N:1
      [(_ (quote name-sym) ((morphism pid (src ...) -> tgt) (effect-scope fn)) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(pid-sym 'pid)
		(src-syms '(src ...))
		(tgt-sym 'tgt)
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cells (map (lambda (s) (or (hash-ref table s) (error "DSL: cell not found in category" s))) src-syms))
		(tgt-cell (or (hash-ref table tgt-sym) (error "DSL: cell not found in category" tgt-sym))))
           (make-propagator pid-val src-cells tgt-cell (effect-scope 'name-sym fn)))]
      [(_ (quote name-sym) ((morphism pid (src ...) -> tgt) fn) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(pid-sym 'pid)
		(src-syms '(src ...))
		(tgt-sym 'tgt)
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cells (map (lambda (s)
                                  (or (hash-ref table s)
                                      (error "DSL: cell not found in category" s)))
				src-syms))
		(tgt-cell (or (hash-ref table tgt-sym)
                              (error "DSL: cell not found in category" tgt-sym))))
           (make-propagator pid-val src-cells tgt-cell fn))]
      ;; 1:N with priority
      [(_ (quote name-sym) ((morphism pid src -> (tgt ...)) (effect-scope fn) priority) table)
       #`(let* ((name-str (symbol->string 'name-sym)) (pid-sym 'pid) (src-sym 'src) (tgt-syms '(tgt ...))
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cell (or (hash-ref table src-sym) (error "DSL: cell not found in category" src-sym)))
		(tgt-cells (map (lambda (t) (or (hash-ref table t) (error "DSL: cell not found in category" t))) tgt-syms)))
           (make-propagator pid-val src-cell tgt-cells (effect-scope 'name-sym fn) priority))]
      [(_ (quote name-sym) ((morphism pid src -> (tgt ...)) fn priority) table)
       #`(let* ((name-str (symbol->string 'name-sym)) (pid-sym 'pid) (src-sym 'src) (tgt-syms '(tgt ...))
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cell (or (hash-ref table src-sym) (error "DSL: cell not found in category" src-sym)))
		(tgt-cells (map (lambda (t) (or (hash-ref table t) (error "DSL: cell not found in category" t))) tgt-syms)))
           (make-propagator pid-val src-cell tgt-cells fn priority))]
      ;; 1:N
      [(_ (quote name-sym) ((morphism pid src -> (tgt ...)) (effect-scope fn)) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(pid-sym 'pid)
		(src-sym 'src)
		(tgt-syms '(tgt ...))
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cell (or (hash-ref table src-sym) (error "DSL: cell not found in category" src-sym)))
		(tgt-cells (map (lambda (t) (or (hash-ref table t) (error "DSL: cell not found in category" t))) tgt-syms)))
           (make-propagator pid-val src-cell tgt-cells (effect-scope 'name-sym fn)))]
      [(_ (quote name-sym) ((morphism pid src -> (tgt ...)) fn) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(pid-sym 'pid)
		(src-sym 'src)
		(tgt-syms '(tgt ...))
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cell (or (hash-ref table src-sym) (error "DSL: cell not found in category" src-sym)))
		(tgt-cells (map (lambda (t) (or (hash-ref table t) (error "DSL: cell not found in category" t))) tgt-syms)))
           (make-propagator pid-val src-cell tgt-cells fn))]
      ;; 1:1 with priority
      [(_ (quote name-sym) ((morphism pid src -> tgt) (effect-scope fn) priority) table)
       #`(let* ((name-str (symbol->string 'name-sym)) (pid-sym 'pid) (src-sym 'src) (tgt-sym 'tgt)
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cell (or (hash-ref table src-sym) (error "DSL: cell not found in category" src-sym)))
		(tgt-cell (or (hash-ref table tgt-sym) (error "DSL: cell not found in category" tgt-sym))))
           (make-propagator pid-val src-cell tgt-cell (effect-scope 'name-sym fn) priority))]
      [(_ (quote name-sym) ((morphism pid src -> tgt) fn priority) table)
       #`(let* ((name-str (symbol->string 'name-sym)) (pid-sym 'pid) (src-sym 'src) (tgt-sym 'tgt)
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cell (or (hash-ref table src-sym) (error "DSL: cell not found in category" src-sym)))
		(tgt-cell (or (hash-ref table tgt-sym) (error "DSL: cell not found in category" tgt-sym))))
           (make-propagator pid-val src-cell tgt-cell fn priority))]
      ;; 1:1
      [(_ (quote name-sym) ((morphism pid src -> tgt) (effect-scope fn)) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(pid-sym 'pid)
		(src-sym 'src)
		(tgt-sym 'tgt)
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cell (or (hash-ref table src-sym) (error "DSL: cell not found in category" src-sym)))
		(tgt-cell (or (hash-ref table tgt-sym) (error "DSL: cell not found in category" tgt-sym))))
           (make-propagator pid-val src-cell tgt-cell (effect-scope 'name-sym fn)))]
      [(_ (quote name-sym) ((morphism pid src -> tgt) fn) table)
       #`(let* ((name-str (symbol->string 'name-sym))
		(pid-sym 'pid)
		(src-sym 'src)
		(tgt-sym 'tgt)
		(pid-val (string->symbol (format #f "~a.~a" name-str pid-sym)))
		(src-cell (or (hash-ref table src-sym) (error "DSL: cell not found in category" src-sym)))
		(tgt-cell (or (hash-ref table tgt-sym) (error "DSL: cell not found in category" tgt-sym))))
           (make-propagator pid-val src-cell tgt-cell fn))])))

(define-syntax define-category
  (lambda (stx)
    (syntax-case stx (objects morphisms)
      ;; objects + morphisms
      [(_ name (objects inst-def ...) (morphisms mor-def ...))
       (with-syntax ([the-name #'name]
                     [(tmp-obj ...) (generate-temporaries #'(inst-def ...))]
                     [(tmp-mor ...) (generate-temporaries #'(mor-def ...))])
         #'(begin
             (define (name . maybe-name)
               (let ((body (lambda (sys)
                             (let ((table (make-hash-table)))
                               (system-add-cell-table sys 'the-name table)
                               (let* ((tmp-obj (create-instance-from-stx 'the-name inst-def table)) ...)
                                 (system-add-objects sys (list tmp-obj ...)))
                               (let* ((tmp-mor (create-morphism-from-stx 'the-name mor-def table)) ...)
                                 (system-add-morphisms sys (list tmp-mor ...)))
                               (category-validate (system-get-net sys))))))
                 (if (null? maybe-name)
                     (let ((sys (current-system)))
                       (when (not sys) (error "define-category called outside of a system context" 'the-name))
                       (parameterize ((current-system sys)) (body sys)))
                     (let ((new-system (make-system (car maybe-name))))
                       (parameterize ((current-system new-system)) (body new-system))
                       new-system))))
             (register-builder (make-category-builder 'the-name name))))]
      ;; objects-only
      [(_ name (objects inst-def ...))
       (with-syntax ([the-name #'name]
                     [(tmp-obj ...) (generate-temporaries #'(inst-def ...))])
         #'(begin
             (define (name . maybe-name)
               (let ((body (lambda (sys)
                             (let ((table (make-hash-table)))
                               (system-add-cell-table sys 'the-name table)
                               (let* ((tmp-obj (create-instance-from-stx 'the-name inst-def table)) ...)
                                 (system-add-objects sys (list tmp-obj ...)))
                               (category-validate (system-get-net sys))))))
                 (if (null? maybe-name)
                     (let ((sys (current-system)))
                       (when (not sys) (error "define-category called outside of a system context" 'the-name))
                       (parameterize ((current-system sys)) (body sys)))
                     (let ((new-system (make-system (car maybe-name))))
                       (parameterize ((current-system new-system)) (body new-system))
                       new-system))))
             (register-builder (make-category-builder 'the-name name))))])))

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
  (lambda (stx)
    (syntax-case stx ()
      [(_ name . body)
       (with-syntax ([the-name #'name])
         #'(begin
             (define (name . maybe-name)
               (let ((sys-name (if (null? maybe-name) 'the-name (car maybe-name))))
                 (let ((new-system (make-system sys-name)))
                   (parameterize ((current-system new-system))
                     (let () . body))
                   new-system)))
             (register-builder (make-category-builder 'the-name name))))])))

(define (add-subsystem! target-system source-system-or-proc)
  (let* ((source-system (if (procedure? source-system-or-proc)
                            (source-system-or-proc)
                            source-system-or-proc))
         (source-prefix (system-name source-system))
         (old->new-cell-map (make-hash-table)))
    (when (not source-prefix)
      (error "Cannot compose an unnamed system" source-system))
    (let ((source-net (system-get-net source-system)))
      (for-each
       (lambda (old-cell)
         (let* ((new-id (string->symbol (format #f "~a.~a" source-prefix (cell-id old-cell))))
                (new-cell (make-cell new-id (cell-type old-cell) (cell-value old-cell) (cell-merge-fn old-cell))))
           (hash-set! old->new-cell-map old-cell new-cell)))
       (category-objects source-net))
      (system-add-objects target-system (hash-map->list (lambda (k v) v) old->new-cell-map)))
    (let ((source-net (system-get-net source-system)))
      (for-each
       (lambda (old-mor)
         (let* ((map-one (lambda (c) (hash-ref old->new-cell-map c #f)))
                (old-dom (arrow-dom old-mor))
                (old-cod (arrow-cod old-mor))
                (new-dom (map-maybe map-one old-dom))
                (new-cod (map-maybe map-one old-cod)))
           (if (and new-dom (if (list? new-dom) (not (member #f new-dom)) #t)
                    new-cod (if (list? new-cod) (not (member #f new-cod)) #t))
               (let* ((old-fn (arrow-fn old-mor))
                      (new-mor-id (string->symbol (format #f "~a.~a" source-prefix (arrow-id old-mor)))))
                 (system-add-morphisms target-system (list (make-propagator new-mor-id new-dom new-cod old-fn (arrow-priority old-mor)))))
               (format (current-error-port) "Warning: could not map morphism ~a during subsystem merge.\n" (arrow-id old-mor)))))
       (category-morphisms source-net)))
    (hash-for-each
     (lambda (cat-name table)
       (let* ((mangled-cat-name (string->symbol (format #f "~a.~a" source-prefix cat-name)))
              (new-table (make-hash-table)))
         (hash-for-each
          (lambda (cell-name old-cell)
            (let ((new-cell (hash-ref old->new-cell-map old-cell)))
              (hash-set! new-table cell-name new-cell)))
          table)
         (system-add-cell-table target-system mangled-cat-name new-table)))
     (system-get-cell-tables source-system))))

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
	 (begin . exec-procs))
       new-system)]))


(define-syntax-rule (trigger cell-expr val)
  (cell-set-value! cell-expr val))

(define-syntax run
  (lambda (stx)
    #'(runtime-settle! (current-system))))

(define-syntax-rule (show-state msg)
  (runtime-show-state (current-system) msg))

(define-syntax-rule (visualize path)
  (system->dot (current-system) path))

(define-syntax-rule (wire from-cell to-cell)
  (system-add-morphisms (current-system)
			(list
			 (make-propagator
			  (gensym "wire-")
			  from-cell
			  to-cell
			  (lambda (v _) (if (list? v) v (cons v '())))))))

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
		(let* ((clauses (list (cons 'src fn) ...))
		       (full-id-str (symbol->string src-id))
		       (parts (string-split full-id-str #\.))
		       (short-id-sym (string->symbol (car (last-pair parts))))
		       (found (assoc short-id-sym clauses))
		       (functor-name (functor-name functor)))
                  (when found
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
		       (cdr found)))))))))
          (category-objects src-cat))))]))

(define-syntax define-functor
  (syntax-rules ()
    [(_ name from-cat to-cat
	(on-objects obj-fn)
	(on-morphisms mor-fn))
     ;; name: 심볼, from-cat/to-cat: 카테고리 생성 함수
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
           ;; 자연성 사각형 검증
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
