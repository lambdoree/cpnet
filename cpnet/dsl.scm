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

(define (syntax->list stx)
  (syntax-case stx ()
    (() '())
    ((x . xs) (cons #'x (syntax->list #'xs)))
    (_ (syntax-violation 'syntax->list "not a proper list" stx))))

(define current-system (make-parameter #f))
(define current-namespace (make-parameter #f))

(define (find-cell-with-ns category-name cell-name)
  (let* ((sys (current-system))
         (ns (current-namespace))
         (mangled-cat-name (if ns
                               (string->symbol (format #f "~a.~a" ns category-name))
                               #f)))
    (or (and mangled-cat-name (system-find-cell sys mangled-cat-name cell-name))
        (system-find-cell sys category-name cell-name))))

(define (get-cell-value category-name cell-name)
  (let ((cell (find-cell-with-ns category-name cell-name)))
    (if cell
        (cell-value cell)
        (begin
          (format (current-error-port) "Warning: get-cell-value could not find ~a.~a in namespace ~a\n" category-name cell-name (current-namespace))
          #f))))

(define (set-cell-effect category-name cell-name value)
  (let ((cell (find-cell-with-ns category-name cell-name)))
    (if cell
        (make-effect 'set-value (cons cell value))
        (error "set-cell-effect: cannot find cell" (if (current-namespace) (list (current-namespace) category-name) category-name) cell-name))))

(define (split-sym stx)
  (let* ((sym   (syntax->datum stx))
         (parts (string-split (symbol->string sym) #\.)))
    (map string->symbol parts)))

(define (get-cell-access-syntax-helper cell-stx)
  (syntax-case cell-stx ()
    [_
     (let ((parts (split-sym cell-stx)))
       (cond
        ((= (length parts) 3) ; System.category.cell
         (let ((sys-name (list-ref parts 0)) (cat-name (list-ref parts 1)) (cell-name (list-ref parts 2)))
           (let ((mangled-cat-name (string->symbol (format #f "~a.~a" sys-name cat-name))))
             (with-syntax ([cat (datum->syntax cell-stx mangled-cat-name)]
                           [cell (datum->syntax cell-stx cell-name)])
               #'(or (system-find-cell (current-system) 'cat 'cell)
                     (error "Cell not found:" 'cat 'cell))))))
        ((= (length parts) 2) ; category.cell
         (let ((cat-name (list-ref parts 0)) (cell-name (list-ref parts 1)))
           (with-syntax ([cat (datum->syntax cell-stx cat-name)]
                         [cell (datum->syntax cell-stx cell-name)])
             #'(or (system-find-cell (current-system) 'cat 'cell)
                   (error "Cell not found:" 'cat 'cell)))))
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
             (make-propagator
              'pid
              src-cell
              tgt-cell
              fn))))])))

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

(define (make-cell-expr name-sym def-stx table-stx)
  (syntax-case def-stx (Cell)
    [(Cell id init)
     (let ((cid-val (string->symbol (format #f "~a.~a" (syntax->datum name-sym) (syntax->datum #'id)))))
       (with-syntax ([cid (datum->syntax #'id cid-val)])
         #`(let ((c# (make-cell 'cid init)))
             (hash-set! #,table-stx (quote id) c#)
             c#)))]
    [(Cell id init merge-fn)
     (let ((cid-val (string->symbol (format #f "~a.~a" (syntax->datum name-sym) (syntax->datum #'id)))))
       (with-syntax ([cid (datum->syntax #'id cid-val)])
         #`(let ((c# (make-cell 'cid init merge-fn)))
             (hash-set! #,table-stx (quote id) c#)
             c#)))]
    [_ (syntax-violation 'define-category "invalid cell definition" def-stx)]))

(define (make-prop-expr name-sym prop-stx table-stx)
  (syntax-case prop-stx (prop ->)
    [((prop pid src -> tgt) fn)
     (let ((pid-val (string->symbol (format #f "~a.~a" (syntax->datum name-sym) (syntax->datum #'pid)))))
       (with-syntax ([pid (datum->syntax #'pid pid-val)])
         #`(make-propagator 'pid
                            (hash-ref #,table-stx (quote src))
                            (hash-ref #,table-stx (quote tgt))
                            fn)))]
    [_ (syntax-violation 'define-category "invalid propagator definition" prop-stx)]))

(define-syntax define-category
  (lambda (stx)
    (syntax-case stx (cells propagators)
      ;; cells-only
      [(_ name (cells cell-def ...))
       (with-syntax
           ([(cell-expr ...)
             (map (lambda (def-stx) (make-cell-expr #'name def-stx #'table))
                  (syntax->list #'(cell-def ...)))])
         #'(define (name)
             (let ((table (make-hash-table)))
               (system-add-cell-table (current-system) (quote name) table)
               (system-add-objects (current-system)
                 (list cell-expr ...))
               table)))]
      ;; cells + propagators
      [(_ name (cells cell-def ...) (propagators prop-def ...))
       (with-syntax
           ([(cell-expr ...)
             (map (lambda (def-stx) (make-cell-expr #'name def-stx #'table))
                  (syntax->list #'(cell-def ...)))]
            [(prop-expr ...)
             (map (lambda (prop-stx) (make-prop-expr #'name prop-stx #'table))
                  (syntax->list #'(prop-def ...)))])
         #'(define (name)
             (let ((table (make-hash-table)))
               (system-add-cell-table (current-system) (quote name) table)
               (system-add-objects (current-system)
                 (list cell-expr ...))
               (system-add-morphisms (current-system)
                 (list prop-expr ...))
               table)))])))

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

(define-syntax define-cpnet-system
  (lambda (stx)
    (syntax-case stx ()
      [(_ name . body)
       (with-syntax ([q-name (datum->syntax #'name (list 'quote (syntax->datum #'name)))])
         #'(define name
             (let ((new-system (make-system q-name)))
               (parameterize ((current-system new-system))
                 (begin . body))
               new-system)))])))

(define (add-subsystem! target-system source-system)
  (let ((source-prefix (system-name source-system))
        (old->new-cell-map (make-hash-table)))
    (when (not source-prefix)
      (error "Cannot compose an unnamed system" source-system))
    
    (let ((source-net (system-get-net source-system)))
      (for-each
       (lambda (old-cell)
         (let* ((new-id (string->symbol (format #f "~a.~a" source-prefix (cell-id old-cell))))
                (new-cell (make-cell new-id (cell-value old-cell) (cell-merge-fn old-cell))))
           (hash-set! old->new-cell-map old-cell new-cell)))
       (category-objects source-net))
      (system-add-objects target-system (hash-map->list (lambda (k v) v) old->new-cell-map)))

    (let ((source-net (system-get-net source-system)))
      (for-each
       (lambda (old-mor)
         (let* ((new-dom (hash-ref old->new-cell-map (arrow-dom old-mor) #f))
                (new-cod (hash-ref old->new-cell-map (arrow-cod old-mor) #f))
                (old-fn (arrow-fn old-mor))
                (new-fn (lambda (v new-source-cell)
                          (parameterize ((current-namespace source-prefix))
                            (old-fn v new-source-cell))))
                (new-mor-id (string->symbol (format #f "~a.~a" source-prefix (arrow-id old-mor)))))
           (if (and new-dom new-cod)
               (let ((new-mor (make-propagator new-mor-id new-dom new-cod new-fn)))
                 (system-add-morphisms target-system new-mor))
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
             (let ((mangled-cat-name (string->symbol (format #f "~a.~a" sys-name cat-name))))
               (with-syntax ([cat (datum->syntax #'cell-name-stx mangled-cat-name)]
                             [cell (datum->syntax #'cell-name-stx cell-name)])
                 #'(let ((c (system-find-cell (current-system) 'cat 'cell)))
                     (if c
                         (cell-set-value! c val)
                         (error "trigger: cell not found" 'cat 'cell)))))))
          ((= (length parts) 2) ; category.cell
           (let ((cat-name (list-ref parts 0)) (cell-name (list-ref parts 1)))
             (with-syntax ([cat (datum->syntax #'cell-name-stx cat-name)]
                           [cell (datum->syntax #'cell-name-stx cell-name)])
               #'(let ((c (system-find-cell (current-system) 'cat 'cell)))
                   (if c
                       (cell-set-value! c val)
                       (error "trigger: cell not found" 'cat 'cell))))))
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
