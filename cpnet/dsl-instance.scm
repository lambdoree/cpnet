(define-module (cpnet dsl-instance)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet core)
  #:export (create-instance-with-lattice
            create-instance-without-lattice
            create-instance-from-stx))

;; Runtime helper used by the macros below to centralize the instance creation logic.
;; Arguments:
;; - name-str : string (category name) for mangling
;; - id-sym   : symbol (cell identifier in category)
;; - type-sym : symbol (type name)
;; - init-val : initial value
;; - lattice  : lattice id or #f
;; - table    : hash table to install the created cell
;; 매크로에서 호출하는 런타임 헬퍼 함수로, cell 인스턴스 생성을 중앙에서 처리합니다.
(define (create-instance-runtime name-str id-sym type-sym init-val lattice table)
  (let* ((cid-val (string->symbol (format #f "~a.~a" name-str id-sym)))
         (c (if lattice
                (make-cell cid-val type-sym init-val lattice)
                (make-cell cid-val type-sym init-val))))
    (hash-set! table id-sym c)
    c))

;; Macro: with explicit lattice id -> delegate to runtime helper.
(define-syntax create-instance-with-lattice
  (syntax-rules ()
    [(_ name-sym id type-name init lattice-id table)
     (create-instance-runtime (symbol->string name-sym) (quote id) (quote type-name) init lattice-id table)]))

;; Macro: without lattice id -> delegate to runtime helper.
(define-syntax create-instance-without-lattice
  (syntax-rules ()
    [(_ name-sym id type-name init table)
     (create-instance-runtime (symbol->string name-sym) (quote id) (quote type-name) init #f table)]))

;; Dispatcher macro kept for backward compatibility; it selects the appropriate
;; helper macro (which in turn calls the small runtime helper above).
(define-syntax create-instance-from-stx
  (syntax-rules ()
    ;; with lattice-id -> delegate to helper
    [(_ name-sym (instance id type-name init lattice-id) table)
     (create-instance-with-lattice name-sym id type-name init lattice-id table)]
    ;; without lattice-id -> delegate to helper
    [(_ name-sym (instance id type-name init) table)
     (create-instance-without-lattice name-sym id type-name init table)]))
