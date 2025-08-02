(define-module (cpnet dsl)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:use-module (cpnet category)
  #:use-module (cpnet functor)
  #:use-module (cpnet nt)
  #:export (define-category define-connections define-execution
             define-cpnet-system compose-systems
	     propagator connector fan-out
	     trigger run show-state
             current-system
             get-cell-value set-cell-effect
	     define-functor define-nt
             apply-functor-as-connections
             define-system-functor))

(define current-system (make-parameter #f))

(define (get-cell-value category-name cell-name)
  (cell-value (system-find-cell (current-system) category-name cell-name)))

(define (set-cell-effect category-name cell-name value)
  (make-effect 'set-value (cons (system-find-cell (current-system) category-name cell-name) value)))

(define (split-sym stx)
  (let* ((sym   (syntax->datum stx))
         (parts (string-split (symbol->string sym) #\.)))
    (map string->symbol parts)))

(define (get-cell-access-syntax-helper cell-stx)
  (define (split-sym stx)
    (let* ((sym   (syntax->datum stx))
           (parts (string-split (symbol->string sym) #\.)))
      (map string->symbol parts)))
  (syntax-case cell-stx ()
    [_
     (let ((parts (split-sym cell-stx)))
       (cond
        ((= (length parts) 3) ; System.category.cell
         (let ((sys-name (list-ref parts 0)) (cat-name (list-ref parts 1)) (cell-name (list-ref parts 2)))
           (with-syntax ([cat (datum->syntax cell-stx cat-name)]
                         [cell (datum->syntax cell-stx cell-name)])
             #'(system-find-cell (current-system) 'cat 'cell))))
        ((= (length parts) 2) ; category.cell
         (let ((cat-name (list-ref parts 0)) (cell-name (list-ref parts 1)))
           (with-syntax ([cat (datum->syntax cell-stx cat-name)]
                         [cell (datum->syntax cell-stx cell-name)])
             #'(system-find-cell (current-system) 'cat 'cell))))
        (else (syntax-violation 'get-cell-access-syntax-helper "invalid cell specifier" cell-stx))))]))

(define-syntax propagator
  (lambda (stx)
    (syntax-case stx (->)
      [(_ pid src -> tgt fn)
       (with-syntax ([src-cell (get-cell-access-syntax-helper #'src)]
                     [tgt-cell (get-cell-access-syntax-helper #'tgt)])
         #'(system-add-morphisms
            (current-system)
            (list
             (let ((propagator-fn fn)
                   (target-cell-obj tgt-cell))
               (make-propagator
                'pid
                src-cell
                target-cell-obj
                (lambda (v source-cell)
                  (let* ((result (propagator-fn v source-cell))
                         (new-val (car result))
                         (effects (cdr result)))
                    (if (and (null? effects) (equal? new-val (cell-value target-cell-obj)))
                        (cons #f '()) ; value hasn't changed, and no effects. no-op.
                        result))))))))])))

(define-syntax connector
  (lambda (stx)
    (syntax-case stx ()
      [(_ pid src tgt)
       (with-syntax ([src-cell (get-cell-access-syntax-helper #'src)]
                     [tgt-cell (get-cell-access-syntax-helper #'tgt)])
         #'(system-add-morphisms
            (current-system)
            (list
             (make-propagator
              'pid
              src-cell
              tgt-cell
              (lambda (v source-cell)
                ;; Propagate the value directly and reset the source cell to
                ;; avoid infinite propagation loops.
                (if v
                    (cons v (list (make-effect 'set-value (cons source-cell #f))))
                    (cons #f '())))))))])))

(define-syntax fan-out
  (lambda (stx)
    (syntax-case stx (->)
      [(_ pid src tgts)
       (let ((tgt-list (syntax->datum #'tgts)))
         (if (or (null? tgt-list) (not (list? tgt-list)))
             (syntax-violation 'fan-out "expected a non-empty list of target cells" stx #'tgts)
             (let ((effects (map (lambda (tgt-sym)
                                   (let* ((tgt-cell-stx (datum->syntax #'tgts tgt-sym))
                                          (tgt-cell-access (get-cell-access-syntax-helper tgt-cell-stx)))
                                     #`(make-effect 'set-value (cons #,tgt-cell-access v))))
                                 tgt-list)))
               (with-syntax ([pid #'pid]
                             [src #'src]
                             [(effect ...) effects])
                 #'(propagator pid src -> src
                               (lambda (v src-cell)
                                 (if v
                                     (cons #f (list effect ...))
                                     (cons #f '()))))))))])))

(define-syntax define-category
  (syntax-rules (cells propagators)
    ;; cells-only
    [(_ name (cells (Cell id init) ...))
     (define (name)
       (let ((table (make-hash-table)))
         (system-add-cell-table (current-system) 'name table)
         (system-add-objects (current-system)
			     (list
			      (let ((c# (make-cell 'id init)))
				(hash-set! table 'id c#)
				c#)
			      ...))
         table))]
    ;; cells + propagators
    [(_ name (cells (Cell id init) ...) (propagators ((prop pid src -> tgt) fn) ...))
     (define (name)
       (let ((table (make-hash-table)))
         (system-add-cell-table (current-system) 'name table)
         (system-add-objects (current-system)
			     (list
			      (let ((c# (make-cell 'id init)))
				(hash-set! table 'id c#)
				c#)
			      ...))
         (system-add-morphisms (current-system)
			       (list
				(let ((target-cell-obj (hash-ref table 'tgt)))
				  (make-propagator
				   'pid
				   (hash-ref table 'src)
				   target-cell-obj
				   (lambda (value source-cell)
				     (let* ((user-fn-result (fn value source-cell))
					    (new-val (car user-fn-result))
					    (effects (cdr user-fn-result)))
				       (if (and (null? effects) (equal? new-val (cell-value target-cell-obj)))
					   (cons #f '()) ; no change
					   user-fn-result)))))
				...))
         table))]
    
    ))

(define-syntax define-connections
  (syntax-rules (propagator connector)
    [(_ name)
     (define (name)
       (begin))]
    [(_ name clause ...)
     (define (name)
       (begin clause ...))]))

(define-syntax-rule (define-execution name . body)
  (define (name) (begin . body)))

(define-syntax-rule (define-cpnet-system name . body)
  (define name
    (let ((new-system (make-system)))
      (parameterize ((current-system new-system))
        (begin . body))
      new-system)))

(define (merge-system! target-system source-system)
  (let ((net (system-get-net source-system)))
    (system-add-objects target-system (category-objects net))
    (system-add-morphisms target-system (category-morphisms net))
    (hash-for-each
     (lambda (cat-name table)
       (system-add-cell-table target-system cat-name table))
     (system-get-cell-tables source-system))))


(define-syntax compose-systems
  (syntax-rules (systems connections execution)
    ;; systems 병합 후 연결 정의 및 실행 시나리오 호출
    [(_ (systems sys ...)
        (connections conn-proc ...)
        (execution . exec-procs))
     (let ((new-system (make-system)))
       ;; 서브시스템 병합
       (for-each (lambda (s) (merge-system! new-system s))
                 (list sys ...))
       ;; 현재 시스템 컨텍스트에서 연결 정의 및 시나리오 실행
       (parameterize ((current-system new-system))
         ;; 연결 정의
         (begin conn-proc ...)
         ;; 시나리오 프로시저 호출
         (for-each (lambda (proc) (proc)) (list (lambda () (begin . exec-procs)))))
       new-system)]))


(define-syntax trigger
  (lambda (stx)
    (syntax-case stx ()
      [(_ cell-name-stx val)
       (let ((parts (split-sym #'cell-name-stx)))
         (cond
          ((= (length parts) 3) ; System.category.cell
           (let ((sys-name (list-ref parts 0)) (cat-name (list-ref parts 1)) (cell-name (list-ref parts 2)))
             (with-syntax ([cat (datum->syntax #'cell-name-stx cat-name)]
                           [cell (datum->syntax #'cell-name-stx cell-name)])
               #'(let ((c (system-find-cell (current-system) 'cat 'cell)))
                   (cell-set-value! c val)))))
          ((= (length parts) 2) ; category.cell
           (let ((cat-name (list-ref parts 0)) (cell-name (list-ref parts 1)))
             (with-syntax ([cat (datum->syntax #'cell-name-stx cat-name)]
                           [cell (datum->syntax #'cell-name-stx cell-name)])
               #'(let ((c (system-find-cell (current-system) 'cat 'cell)))
                   (cell-set-value! c val)))))
          (else (syntax-violation 'trigger "invalid cell specifier" #'cell-name-stx))))])))

(define-syntax-rule (run)
  (runtime-settle! (current-system)))

(define-syntax-rule (show-state msg)
  (runtime-show-state (current-system) msg))

(define-syntax-rule (apply-functor-as-connections F)
  (let ((functor F))
    (let ((src-cat (functor-src-cat functor)))
      (for-each
       (lambda (src-obj)
         (let ((tgt-obj ((functor-obj-map functor) src-obj)))
           (when tgt-obj ;; if there is a mapping
             (system-add-morphisms
              (current-system)
              (list
               (make-propagator
                (string->symbol (format #f "functor-conn-~a->~a"
                                        (cell-id src-obj)
                                        (cell-id tgt-obj)))
                src-obj
                tgt-obj
                (lambda (v _) (cons v '()))))))))
       (category-objects src-cat)))))

(define-syntax define-functor
  (syntax-rules ()
    [(_ name from-cat to-cat
        (on-objects obj-fn)
        (on-morphisms mor-fn))
     ;; name: 심볼, from-cat/to-cat: 카테고리 생성 함수
     (define name
       (let* ((C   (from-cat)) ; 실제 카테고리 인스턴스
              (D   (to-cat))
              (F   (make-functor C D obj-fn mor-fn)))
         ;; 공리 검증
         (for-each
          (lambda (a)
            ;; 단위 보존: F(id_x) = id_{F x}
            (unless (equal? (functor-map-morphism F (identity-morphism C a))
                            (identity-morphism D (obj-fn a)))
              (error 'functor-validate
                     (format #f "Functor ~a fails unit law at object ~a" 'name a))))
          (category-objects C))
         (for-each
          (lambda (f)
            (let* ((g   (identity-morphism C (codomain-of f)))
                   (lhs (functor-map-morphism F (compose-morphisms C g f)))
                   (rhs (compose-morphisms D
					   (functor-map-morphism F g)
					   (functor-map-morphism F f))))
              ;; 합성 보존
              (unless (equal? lhs rhs)
                (error 'functor-validate
                       (format #f "Functor ~a fails comp law at morphism ~a" 'name f)))))
          (category-morphisms C))
         F))]))

(define-syntax define-system-functor
  (syntax-rules (from to mappings ->)
    [(_ name (from src-cat-name) (to tgt-cat-name) (mappings (src-cell-name -> tgt-cell-name) ...))
     (define name
       (let* ((sys (current-system))
              (tables (system-get-cell-tables sys))
              (src-cat-table (hash-ref tables 'src-cat-name))
              (tgt-cat-table (hash-ref tables 'tgt-cat-name))

              (src-objs (hash-map->list (lambda (k v) v) src-cat-table))
              (tgt-objs (hash-map->list (lambda (k v) v) tgt-cat-table))
              (all-mors (category-morphisms (system-get-net sys)))

              (src-mors (filter (lambda (m)
                                  (and (member (arrow-dom m) src-objs)
                                       (member (arrow-cod m) src-objs)))
                                all-mors))
              (tgt-mors (filter (lambda (m)
                                  (and (member (arrow-dom m) tgt-objs)
                                       (member (arrow-cod m) tgt-objs)))
                                all-mors))

              (src-cat (make-cpnet-category src-objs src-mors))
              (tgt-cat (make-cpnet-category tgt-objs tgt-mors))

              (cell-map (list
                         (cons (system-find-cell sys 'src-cat-name 'src-cell-name)
                               (system-find-cell sys 'tgt-cat-name 'tgt-cell-name))
                         ...))
              )
         (make-cpnet-functor src-cat tgt-cat cell-map)))]))

(define-syntax define-nt
  (syntax-rules ()
    [(_ name F G (component obj fn) ...)
     (define name
       (let ((η (make-nt-record F G (list (cons 'obj fn) ...))))
         ;; 자연성 사각형 검증
         (for-each
          (lambda (f)
            (let* ((x   (domain-of f))
                   (y   (codomain-of f))
                   (ηx  (nt-component η x))
                   (ηy  (nt-component η y))
                   (lhs (compose-morphisms (functor-target G)
					   (functor-map-morphism G f)
					   ηx))
                   (rhs (compose-morphisms (functor-target F)
					   ηy
					   (functor-map-morphism F f))))
              (unless (equal? lhs rhs)
                (error 'nt-validate
                       (format #f "NT ~a fails naturality at morphism ~a" 'name f)))))
          (category-morphisms (functor-source F)))
         η))]))
