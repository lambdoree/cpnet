(define-module (cpnet dsl)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:use-module (cpnet category)
  #:export (define-category define-connections define-execution
             define-cpnet-system compose-systems
	     propagator connector fan-out
	     trigger run show-state
             current-system))

(define current-system (make-parameter #f))

(define (split-sym stx)
  (let* ((sym   (syntax->datum stx))
         (parts (string-split (symbol->string sym) #\.)))
    (map string->symbol parts)))

(define-syntax propagator
  (lambda (stx)
    (define (get-cell-access-syntax cell-stx)
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
            (else (syntax-violation 'propagator "invalid cell specifier" cell-stx))))]))
    (syntax-case stx (->)
      [(_ pid src -> tgt fn)
       (with-syntax ([src-cell (get-cell-access-syntax #'src)]
                     [tgt-cell (get-cell-access-syntax #'tgt)])
         #'(system-add-morphisms
            (current-system)
            (make-propagator
             'pid
             src-cell
             tgt-cell
             fn)))])))

(define-syntax connector
  (lambda (stx)
    (define (get-cell-access-syntax cell-stx)
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
            (else (syntax-violation 'connector "invalid cell specifier" cell-stx))))]))
    (syntax-case stx ()
      [(_ pid src tgt)
       (with-syntax ([src-cell (get-cell-access-syntax #'src)]
                     [tgt-cell (get-cell-access-syntax #'tgt)])
         #'(system-add-morphisms
            (current-system)
            (make-propagator
             'pid
             src-cell
             tgt-cell
             (lambda (v source-cell)
               (if v
                   (cons v (list (make-effect 'set-value (cons source-cell #f))))
                   (cons #f '()))))))])))

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

(define-syntax define-category
  (lambda (stx)
    (syntax-case stx (cells propagators Cell prop ->)
      [(_ name (cells (Cell id init) ...))
       (with-syntax ([quoted-name (datum->syntax #'name (list 'quote (syntax->datum #'name)))])
         #'(define (name)
             (let ((table (make-hash-table)))
               (system-add-cell-table (current-system) quoted-name table)
               (for-each
                (lambda (c) (system-add-objects (current-system) c))
                (list (let ((c# (make-cell 'id init)))
                        (hash-set! table 'id c#)
                        c#) ...)))))]
      [(_ name (cells (Cell id init) ...) (propagators ((prop pid src -> tgt) fn) ...))
       (with-syntax ([quoted-name (datum->syntax #'name (list 'quote (syntax->datum #'name)))])
         #'(define (name)
             (let ((table (make-hash-table)))
               (system-add-cell-table (current-system) quoted-name table)
               (for-each
                (lambda (c) (system-add-objects (current-system) c))
                (list (let ((c# (make-cell 'id init)))
                        (hash-set! table 'id c#)
                        c#) ...)))
             (for-each
              (lambda (m) (system-add-morphisms (current-system) m))
              (list (make-propagator 'pid
                                     (hash-ref table 'src)
                                     (hash-ref table 'tgt)
                                     fn) ...))))])))

(define-syntax-rule (define-connections name . body)
  (define (name) (begin . body)))

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
    [(_ name
        (systems sys ...)
        (connections conn-proc ...)
        (execution exec-proc ...))
     (define name
       (let ((new-system (make-system)))
         (for-each
          (lambda (s) (merge-system! new-system s))
          (list sys ...))
         (parameterize ((current-system new-system))
           (conn-proc) ...
           (exec-proc) ...)
         new-system))]))

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
