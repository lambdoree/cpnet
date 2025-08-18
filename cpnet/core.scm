(define-module (cpnet core)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:use-module (cpnet log)
  #:export (
	    cell?
	    make-cell
	    cell-id
	    cell-type
	    cell-value
	    cell-set-value!
            cell-lattice-id
	    cell-merge-fn
	    cell-system
	    cell-set-system!
	    effect?
	    make-effect
	    effect-type
	    effect-payload
            *nothing*
            *unresolved*
            map-maybe
            category-builder?
            make-category-builder
            builder-name
            builder-function
            *builder-registry*
            register-builder
	    register-lattice
	    builder-signature
            get-builder
            ))

(define-record-type <category-builder>
  (make-category-builder-internal name builder-proc signature)
  category-builder?
  (name builder-name)
  (builder-proc builder-function)
  (signature builder-signature))

;; category-builder 레코드를 생성합니다.
(define (make-category-builder name builder-proc signature)
  (make-category-builder-internal name builder-proc signature))

(define *builder-registry* (make-hash-table))
;; category-builder를 전역 레지스트리에 등록합니다.
(define (register-builder cb)
  (hash-set! *builder-registry* (builder-name cb) cb))
;; 이름으로 category-builder를 레지스트리에서 찾습니다.
(define (get-builder name)
  (hash-ref *builder-registry* name
            (lambda () (error "Unknown category:" name))))

;; 단일 아이템 또는 리스트의 모든 아이템에 함수를 적용합니다.
(define (map-maybe f x)
  (if (list? x) (map f x) (f x)))

(define-record-type <lattice>
  (make-lattice bottom join commutative? associative? idempotent? top)
  lattice?
  (bottom lattice-bottom)
  (join lattice-join)
  (commutative? lattice-commutative?)
  (associative? lattice-associative?)
  (idempotent? lattice-idempotent?)
  (top lattice-top))

(define *lattice-registry* (make-hash-table))

;;; `('key1 val1 'key2 val2)`와 같은 리스트에서 키워드 인자를 파싱합니다.
(define (get-keyword-from-list key lst default)
  (let ((tail (memq key lst)))
    (if (and tail (pair? (cdr tail)))
        (cadr tail)
        default)))

;; 주어진 속성으로 lattice를 생성하여 전역 레지스트리에 등록합니다.
(define (register-lattice id . kwargs)
  (let ((bottom (get-keyword-from-list 'bottom kwargs #f))
        (join-fn (get-keyword-from-list 'join kwargs #f))
        (comm (get-keyword-from-list 'commutative? kwargs #t))
        (assoc (get-keyword-from-list 'associative? kwargs #t))
        (idem (get-keyword-from-list 'idempotent? kwargs #t))
        (top (get-keyword-from-list 'top-element kwargs #f)))
    (if join-fn
        (hash-set! *lattice-registry* id (make-lattice bottom join-fn comm assoc idem top))
        (error "register-lattice requires a 'join function for" id))))

(define-record-type <cell>
  (make-cell-record id type value lattice-id system)
  cell?
  (id cell-id)
  (type cell-type)
  (value cell-value set-cell-value!)
  (lattice-id cell-lattice-id)
  (system cell-system set-cell-system!))

;; cell의 lattice ID에 해당하는 병합(join) 함수를 반환합니다.
(define (cell-merge-fn cell)
  (let ((l (hash-ref *lattice-registry* (cell-lattice-id cell))))
    (if l (lattice-join l) (error "Lattice not found" (cell-lattice-id cell)))))

(define-record-type <effect>
  (make-effect type payload)
  effect?
  (type effect-type)
  (payload effect-payload))

;; 새로운 cell 레코드를 생성합니다. lattice ID가 제공되지 않으면 'Default'를 사용합니다.
(define (make-cell id type init-val . maybe-lattice-id)
  (let ((lattice-id (if (null? maybe-lattice-id) 'Default (car maybe-lattice-id))))
    (make-cell-record id type init-val lattice-id #f)))

;; cell의 값을 설정하고 cell 자체를 반환합니다.
(define (cell-set-value! c new-val)
  (set-cell-value! c new-val)
  c)

;; cell이 속한 시스템을 설정하고 cell 자체를 반환합니다.
(define (cell-set-system! c new-sys)
  (set-cell-system! c new-sys)
  c)

(define *nothing* (gensym "nothing"))

(define *unresolved* (gensym "unresolved"))

(register-lattice 'Default 'bottom #f
		  'join (lambda (cell new-vals)
		    (let* ((old (cell-value cell))
			   (unique-new-vals (delete-duplicates new-vals equal?)))
		      (cond ((null? unique-new-vals) (cons old '()))
			    ((and (= 1 (length unique-new-vals)) (equal? (car unique-new-vals) old))
			     (cons old '()))
			    ((= 1 (length unique-new-vals))
			     (cons (car unique-new-vals) '()))
			    (else
			     (begin
			       (warnf "CONFLICT on cell ~a. Values: ~s. Reverting to bottom (#f).\n"
				      (cell-id cell) new-vals)
			       (cons #f '())))))))

(register-lattice 'Replace 'bottom #f
		  'join (lambda (cell new-vals)
		    (if (null? new-vals)
			(cons (cell-value cell) '())
			(cons (car new-vals) '())))
		  'commutative? #f
		  'associative? #f)

(register-lattice 'Set 'bottom '()
		  'join (lambda (cell new-vals)
		    (let ((current (let ((val (cell-value cell)))
				     (if (list? val) val (if (not (eq? val #f)) (list val) '()))))
			  (news (map (lambda (v) (if (list? v) v (if (not (eq? v #f)) (list v) '()))) new-vals)))
		      (cons (delete-duplicates (apply append (cons current news)) equal?) '()))))

(register-lattice 'Max 'bottom *nothing*
  'join (lambda (cell new-vals)
    (if (null? new-vals)
        (cons (cell-value cell) '())
        (let ((numbers (filter number? new-vals)))
          (if (null? numbers)
              (cons (cell-value cell) '())
              (cons (apply max numbers) '()))))))

(register-lattice 'Min 'bottom *nothing*
  'join (lambda (cell new-vals)
    (if (null? new-vals)
        (cons (cell-value cell) '())
        (let ((numbers (filter number? new-vals)))
          (if (null? numbers)
              (cons (cell-value cell) '())
              (cons (apply min numbers) '()))))))

(register-lattice 'Bool 'bottom #f
  'join (lambda (cell new-vals)
    (if (null? new-vals)
        (cons (cell-value cell) '())
        (let ((bool-vals (filter boolean? new-vals)))
          (if (null? bool-vals)
              (cons (cell-value cell) '())
              (cons (any identity bool-vals) '()))))))

(register-lattice 'Maybe 'bottom *nothing*
  'join (lambda (cell new-vals)
    (let ((defined-vals (filter (lambda (v) (not (eq? v *nothing*))) new-vals)))
      (cond
       ((null? defined-vals) (cons *nothing* '()))
       ((= 1 (length defined-vals)) (cons (car defined-vals) '()))
       (else (cons (car defined-vals)
                   (list (make-effect 'display
                                      (format #f "CONFLICT on Maybe cell ~a. Values: ~a. Taking first value.\n"
                                              (cell-id cell) defined-vals))))))))
  'commutative? #f
  'associative? #f)


