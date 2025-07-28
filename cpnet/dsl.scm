;;; cpnet/dsl.scm
(define-module (cpnet dsl)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:export (category define-cpnet-system compose-systems
           propagator connector fan-out
           trigger run show-state))

(define current-system (make-parameter #f))

(define (split-sym stx)
  (let* ((sym   (syntax->datum stx))
         (parts (string-split (symbol->string sym) #\.)))
    (map string->symbol parts)))

(define-syntax propagator
  (lambda (stx)
    (syntax-case stx ()
      [(_ pid src tgt fn)
       (let* ((src-parts (split-sym #'src))
              (src-cat-sym (car src-parts))
              (src-cell-sym (cadr src-parts))
              (src-table-sym (string->symbol (string-append (symbol->string src-cat-sym) "-cells")))
              (tgt-parts (split-sym #'tgt))
              (tgt-cat-sym (car tgt-parts))
              (tgt-cell-sym (cadr tgt-parts))
              (tgt-table-sym (string->symbol (string-append (symbol->string tgt-cat-sym) "-cells"))))
         (with-syntax ([src-table (datum->syntax #'src src-table-sym)]
                       [src-cell-name (datum->syntax #'src src-cell-sym)]
                       [tgt-table (datum->syntax #'tgt tgt-table-sym)]
                       [tgt-cell-name (datum->syntax #'tgt tgt-cell-sym)])
           #'(system-add-morphisms
              (current-system)
              (make-propagator
               'pid
               (hash-ref src-table 'src-cell-name)
               (hash-ref tgt-table 'tgt-cell-name)
               fn))))])))

(define-syntax connector
  (lambda (stx)
    (syntax-case stx ()
      [(_ pid src tgt)
       (let* ((src-parts (split-sym #'src))
              (src-cat-sym (car src-parts))
              (src-cell-sym (cadr src-parts))
              (src-table-sym (string->symbol (string-append (symbol->string src-cat-sym) "-cells")))
              (tgt-parts (split-sym #'tgt))
              (tgt-cat-sym (car tgt-parts))
              (tgt-cell-sym (cadr tgt-parts))
              (tgt-table-sym (string->symbol (string-append (symbol->string tgt-cat-sym) "-cells"))))
         (with-syntax ([src-table (datum->syntax #'src src-table-sym)]
                       [src-cell-name (datum->syntax #'src src-cell-sym)]
                       [tgt-table (datum->syntax #'tgt tgt-table-sym)]
                       [tgt-cell-name (datum->syntax #'tgt tgt-cell-sym)])
           #'(system-add-morphisms
              (current-system)
              (make-propagator
               'pid
               (hash-ref src-table 'src-cell-name)
               (hash-ref tgt-table 'tgt-cell-name)
               (lambda (v src-cell)
                 (if v
                     (cons v (list (make-effect 'set-value (cons src-cell #f))))
                     (cons #f '())))))))])))

(define-syntax fan-out
  (lambda (stx)
    (syntax-case stx ()
      [(_ pid src tgts)
       (let ((tgt-list (syntax->datum #'tgts)))
         (if (not (list? tgt-list))
             (syntax-violation 'fan-out "expected a list of target cells" stx #'tgts)
             (let* ((pid-base (symbol->string (syntax->datum #'pid)))
                    (new-forms
                     (map (lambda (tgt-sym)
                            (let ((new-pid-sym (string->symbol (string-append pid-base "-" (symbol->string tgt-sym)))))
                              (with-syntax ([new-pid (datum->syntax #'pid new-pid-sym)]
                                            [src-cell #'src]
                                            [tgt-cell (datum->syntax #'tgts tgt-sym)])
                                #'(connector new-pid src-cell tgt-cell))))
                          tgt-list)))
               (with-syntax ([(form ...) new-forms])
                 #'(begin form ...)))))])))

(define-syntax category
  (lambda (stx)
    (syntax-case stx (cells propagators Cell prop ->)
      [(_ name
          (cells (Cell id init) ...)
          (propagators ((prop pid src -> tgt) fn) ...))
       (with-syntax ([name-cells (datum->syntax #'name
                                                (string->symbol
                                                 (string-append (symbol->string (syntax->datum #'name))
                                                                "-cells")))])
         #'(begin
             (for-each
              (lambda (c) (system-add-objects (current-system) c))
              (list (let ((c# (make-cell 'id init)))
                      (hash-set! name-cells 'id c#)
                      c#) ...))
             (for-each
              (lambda (m) (system-add-morphisms (current-system) m))
              (list (make-propagator
                     'pid
                     (hash-ref name-cells 'src)
                     (hash-ref name-cells 'tgt)
                     fn) ...))
             #t))])))

(define-syntax define-cpnet-system
  (lambda (stx)
    (syntax-case stx (category connections execution)
      [(_ name
          (category cat-name . cat-body) ...
          (connections con ...)
          (execution step ...))
       (let* ((cat-name-symbols (syntax->datum #'(cat-name ...)))
              (cell-var-symbols (map (lambda (s)
                                       (string->symbol (string-append (symbol->string s) "-cells")))
                                     cat-name-symbols))
              (cell-vars (map (lambda (s) (datum->syntax #'name s))
                              cell-var-symbols)))
         (with-syntax (((cat-cell-var ...) cell-vars))
           #'(define name
               (let ((new-system (make-system)))
                 (letrec* ((cat-cell-var (make-hash-table)) ...)
                   (parameterize ((current-system new-system))
                     (category cat-name . cat-body) ...
                     con ...
                     (begin step ...)))
                 new-system))))])))

(define-syntax compose-systems
  (syntax-rules (connections execution)
    [(_ name
        sys ...
        (connections con ...)
        (execution step ...))
     (define name
       (let ((new-system (make-system)))
         (parameterize ((current-system new-system))
           (for-each (lambda (x) #f) (list sys ...))
           con ...
           (begin step ...))
         new-system))]))

(define-syntax trigger
  (lambda (stx)
    (syntax-case stx ()
      [(_ cell val)
       (let* ((parts (split-sym #'cell))
              (cat-name-sym (car parts))
              (cell-name-sym (cadr parts))
              (table-name-sym (string->symbol
                               (string-append (symbol->string cat-name-sym)
                                              "-cells"))))
         (with-syntax ([table-name (datum->syntax #'cell table-name-sym)]
                       [cell-name  (datum->syntax #'cell cell-name-sym)])
           #'(let ((c (hash-ref table-name 'cell-name)))
               (cell-set-value! c val))))])))

(define-syntax-rule (run)
  (runtime-settle! (current-system)))

(define-syntax-rule (show-state msg)
  (runtime-show-state (current-system) msg))
