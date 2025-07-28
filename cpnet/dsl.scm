;;; cpnet/dsl.scm
(define-module (cpnet dsl)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:use-module (cpnet category)
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
    (define (get-cell-access-syntax cell-stx)
      (syntax-case cell-stx ()
        [_
         (let ((parts (split-sym cell-stx)))
           (if (= (length parts) 3)
               (let ((sys-name (list-ref parts 0))
                     (cat-name (list-ref parts 1))
                     (cell-name (list-ref parts 2)))
                 (with-syntax ([sys (datum->syntax cell-stx sys-name)]
                               [cat (datum->syntax cell-stx cat-name)]
                               [cell (datum->syntax cell-stx cell-name)])
                   #'(system-find-cell sys 'cat 'cell)))
               (let* ((cat-name-sym (car parts))
                      (cell-name-sym (cadr parts))
                      (table-name-sym (string->symbol
                                       (string-append (symbol->string cat-name-sym) "-cells"))))
                 (with-syntax ([table-name (datum->syntax cell-stx table-name-sym)]
                               [cell-name (datum->syntax cell-stx cell-name-sym)])
                   #'(hash-ref table-name 'cell-name)))))]))
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
           (if (= (length parts) 3)
               (let ((sys-name (list-ref parts 0))
                     (cat-name (list-ref parts 1))
                     (cell-name (list-ref parts 2)))
                 (with-syntax ([sys (datum->syntax cell-stx sys-name)]
                               [cat (datum->syntax cell-stx cat-name)]
                               [cell (datum->syntax cell-stx cell-name)])
                   #'(system-find-cell sys 'cat 'cell)))
               (let* ((cat-name-sym (car parts))
                      (cell-name-sym (cadr parts))
                      (table-name-sym (string->symbol
                                       (string-append (symbol->string cat-name-sym) "-cells"))))
                 (with-syntax ([table-name (datum->syntax cell-stx table-name-sym)]
                               [cell-name (datum->syntax cell-stx cell-name-sym)])
                   #'(hash-ref table-name 'cell-name)))))]))
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

(define-syntax category
  (lambda (stx)
    (syntax-case stx (cells propagators Cell prop ->)
      [(_ name
          (cells (Cell id init) ...))
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
             #t))]
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
      [(_ name (category cat-name . cat-body) ...)
       #'(define-cpnet-system name
           (category cat-name . cat-body) ...
           (connections) (execution))]
      [(_ name (category cat-name . cat-body) ...
          (connections con ...))
       #'(define-cpnet-system name
           (category cat-name . cat-body) ...
           (connections con ...) (execution))]
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
                     (for-each
                      (lambda (pair) (system-add-cell-table new-system (car pair) (cdr pair)))
                      (list (cons 'cat-name cat-cell-var) ...))
                     (category cat-name . cat-body) ...
                     con ...
                     step ...))
                 new-system))))])))

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
        (connections con ...)
        (execution step ...))
     (define name
       (let ((new-system (make-system)))
         (for-each
          (lambda (s) (merge-system! new-system s))
          (list sys ...))
         (parameterize ((current-system new-system))
           con ...
           step ...)
         new-system))]))

(define-syntax trigger
  (lambda (stx)
    (syntax-case stx ()
      [(_ cell-name-stx val)
       (let ((parts (split-sym #'cell-name-stx)))
         (if (= (length parts) 3)
             (let ((sys-name (list-ref parts 0))
                   (cat-name (list-ref parts 1))
                   (cell-name (list-ref parts 2)))
               (with-syntax ([sys (datum->syntax #'cell-name-stx sys-name)]
                             [cat (datum->syntax #'cell-name-stx cat-name)]
                             [cell (datum->syntax #'cell-name-stx cell-name)])
                 #'(let ((c (system-find-cell sys 'cat 'cell)))
                     (cell-set-value! c val))))
             (let* ((cat-name-sym (car parts))
                    (cell-name-sym (cadr parts))
                    (table-name-sym (string->symbol
                                     (string-append (symbol->string cat-name-sym)
                                                    "-cells"))))
               (with-syntax ([table-name (datum->syntax #'cell-name-stx table-name-sym)]
                             [cell-name  (datum->syntax #'cell-name-stx cell-name-sym)])
                 #'(let ((c (hash-ref table-name 'cell-name)))
                     (cell-set-value! c val))))))])))

(define-syntax-rule (run)
  (runtime-settle! (current-system)))

(define-syntax-rule (show-state msg)
  (runtime-show-state (current-system) msg))
