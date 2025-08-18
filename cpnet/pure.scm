(define-module (cpnet pure)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:use-module (cpnet apply)
  #:export (true-gate
	    false-gate
            if-system
            loop-system
            is-f-system
            is-f-interface
            branch-on-f-system
            branch-on-f-interface
            switch-system
            switch-interface
            try-catch-system
            try-catch-interface))

;; propagator 함수: 소스 값들을 그대로 전달합니다.
(define (pass-as-is vals srcs) (cons srcs '()))
;; propagator 함수: 소스 값들을 하나의 리스트로 묶어 전달합니다.
(define (pass-as-list vals srcs) (cons (list srcs) '()))

(register-lattice 'Select-Non-F 'bottom #f
  'join (lambda (cell new-vals)
    (let ((filtered-vals (filter (lambda (v) (not (eq? v #f))) new-vals)))
      (if (null? filtered-vals)
          (cons #f '())
          ((cell-merge-fn (make-cell 'dummy 'dummy #f 'Default)) cell filtered-vals)))))

(register-lattice 'Ignore-F 'bottom #f
  'join (lambda (cell new-vals)
    (let ((filtered-vals (filter (lambda (v) (not (eq? v #f))) new-vals)))
      (if (null? filtered-vals)
          (cons (cell-value cell) '())
          ((cell-merge-fn (make-cell 'dummy 'dummy #f 'Default)) cell filtered-vals)))))

(define-category branch-on-f-interface
  (objects
   (instance in Data #f)
   (instance then-val Data #f)
   (instance else-val Data #f)
   (instance out Data #f)))

(define-cpnet-system branch-on-f-system
  (branch-on-f-interface)
  (propagator p-branch
    (list (get-cell 'branch-on-f-interface 'in)
          (get-cell 'branch-on-f-interface 'then-val)
          (get-cell 'branch-on-f-interface 'else-val))
    -> (get-cell 'branch-on-f-interface 'out)
    (icnu
     (par
      ;; out = IF in THEN then-val ELSE else-val
      (IC_IF branch-on-f-interface.in (list branch-on-f-interface.then-val 'p) (list branch-on-f-interface.else-val 'p) out))))

(define-category const-bool-provider
  (objects
   (instance true category-builder (get-builder 'true-gate))
   (instance false category-builder (get-builder 'false-gate))))

(define-category const-f-provider
  (objects
   (instance f Data #f)))

(define-category true-gate
  (objects
   (instance X Data #f 'Replace)
   (instance Y Data #f 'Replace)
   (instance Z Data #f 'Replace))
  (morphisms
   ((morphism select-left (X Y) -> Z)
    (icnu
     (par
      (mk-node out 'A)
      (mk-wire true-gate.X 'p out 'p)))))

(register-builder (make-category-builder
                   'true-gate
                   true-gate
                   '((inputs (X Y)) (outputs (Z)))))

(define-category false-gate
  (objects
   (instance X Data #f 'Replace)
   (instance Y Data #f 'Replace)
   (instance Z Data #f 'Replace))
  (morphisms
   ((morphism select-right (X Y) -> Z)
    (icnu
     (par
      (mk-node out 'A)
      (mk-wire false-gate.Y 'p out 'p)))))

(register-builder (make-category-builder
                   'false-gate
                   false-gate
                   '((inputs (X Y)) (outputs (Z)))))

(define-category is-f-interface
  (objects
   (instance in Data #f)
   (instance out category-builder #f)))

(define-cpnet-system is-f-system
  (is-f-interface)
  (add-subsystem! (current-system) (branch-on-f-system 'brancher))
  (add-subsystem! (current-system) (const-bool-provider 'consts))

  (wire (get-cell 'is-f-interface 'in) (get-cell 'brancher 'branch-on-f-interface 'in))

  (wire (get-cell 'consts 'const-bool-provider 'true) (get-cell 'brancher 'branch-on-f-interface 'then-val))
  (wire (get-cell 'consts 'const-bool-provider 'false) (get-cell 'brancher 'branch-on-f-interface 'else-val))
  
  (wire (get-cell 'brancher 'branch-on-f-interface 'out) (get-cell 'is-f-interface 'out)))

(define-category if-interface
  (objects
   (instance condition Bool #f 'Replace)
   (instance then-val Data #f 'Replace)
   (instance else-val Data #f 'Replace)
   (instance result Data #f 'Replace)))

(define-cpnet-system if-system
  (if-interface)
  (system-add-propagator! (current-system)
    (make-branch-propagator
     'p-if
     (get-cell 'if-interface 'condition)
     (get-cell 'if-interface 'then-val)
     (get-cell 'if-interface 'else-val)
     (get-cell 'if-interface 'result))))

(define-category gated-channel-interface
  (objects
   (instance control category-builder #f 'Replace)
   (instance input Data #f)
   (instance output Data #f 'Replace)))

(define-cpnet-system gated-channel-system
  (gated-channel-interface)
  (add-subsystem! (current-system) (apply-gate 'gate-impl))
  (add-subsystem! (current-system) (const-f-provider 'const-f))

  (wire (get-cell 'gated-channel-interface 'control) (get-cell 'gate-impl 'apply-interface 'code))

  (propagator gated-channel-arg-setup
              (list (get-cell 'gated-channel-interface 'input)
                    (get-cell 'const-f 'const-f-provider 'f))
              -> (get-cell 'gate-impl 'apply-interface 'args)
              pass-as-is)
  
  (propagator gated-channel-result-setup
              (get-cell 'gated-channel-interface 'output)
              -> (get-cell 'gate-impl 'apply-interface 'results)
              pass-as-list))

(define-category is-not-f-interface
  (objects
   (instance in Data #f)
   (instance out category-builder #f)))

(define-cpnet-system is-not-f-system
  (is-not-f-interface)
  (add-subsystem! (current-system) (is-f-system 'is-f-check))
  (add-subsystem! (current-system) (if-system 'if-impl))
  (add-subsystem! (current-system) (const-bool-provider 'const-church-bools))

  (wire (get-cell 'is-not-f-interface 'in) (get-cell 'is-f-check 'is-f-interface 'in))
  (wire (get-cell 'is-f-check 'is-f-interface 'out) (get-cell 'if-impl 'if-interface 'condition))

  (wire (get-cell 'const-church-bools 'const-bool-provider 'false) (get-cell 'if-impl 'if-interface 'then-val))
  (wire (get-cell 'const-church-bools 'const-bool-provider 'true) (get-cell 'if-impl 'if-interface 'else-val))

  (wire (get-cell 'if-impl 'if-interface 'result) (get-cell 'is-not-f-interface 'out)))

(define-category loop-interface
  (objects
   (instance body category-builder #f)
   (instance input Data #f)
   (instance output Data #f 'Ignore-F)))

(define-category loop-internal-interface
  (objects
   (instance temp-result Data #f 'Select-Non-F)))

(define-cpnet-system loop-system
  (loop-interface)
  (add-subsystem! (current-system) (apply-gate 'apply-body))
  (add-subsystem! (current-system) (gated-channel-system 'feedback-gate))
  (add-subsystem! (current-system) (is-not-f-system 'gate-control))
  (add-subsystem! (current-system) (loop-internal-interface 'internal))

  (wire (get-cell 'loop-interface 'body) (get-cell 'apply-body 'apply-interface 'code))

  (wire (get-cell 'internal 'loop-internal-interface 'temp-result) (get-cell 'gate-control 'is-not-f-interface 'in))
  (wire (get-cell 'gate-control 'is-not-f-interface 'out) (get-cell 'feedback-gate 'gated-channel-interface 'control))

  (wire (get-cell 'internal 'loop-internal-interface 'temp-result) (get-cell 'feedback-gate 'gated-channel-interface 'input))
  (wire (get-cell 'feedback-gate 'gated-channel-interface 'output) (get-cell 'loop-interface 'output))

  (propagator loop-system-arg-setup
              (list (get-cell 'loop-interface 'input) (get-cell 'loop-interface 'output))
              -> (get-cell 'apply-body 'apply-interface 'args)
              pass-as-is)
  
  (propagator loop-system-result-setup
              (get-cell 'internal 'loop-internal-interface 'temp-result)
              -> (get-cell 'apply-body 'apply-interface 'results)
              pass-as-list))

(define-category switch-interface
  (objects
   (instance key Data #f)
   (instance cases Data '() 'Replace)
   (instance default Data #f)
   (instance result Data #f)))

;; `switch-system`을 위한 핸들러 함수.
;; `key`에 해당하는 `cases`를 찾아 값을 반환하고, 없으면 `default` 값을 반환합니다.
(define (switch-handler vals _)
  (let* ((key     (car vals))
         (cases   (cadr vals))
         (default (caddr vals))
         (found   (and (list? cases) (assoc key cases))))
    (if found
        (cons (cdr found) '())
        (cons default '()))))

(define-cpnet-system switch-system
  (switch-interface)
  (propagator p-switch
              (list (get-cell 'switch-interface 'key)
                    (get-cell 'switch-interface 'cases)
                    (get-cell 'switch-interface 'default))
              -> (get-cell 'switch-interface 'result)
              switch-handler))

;; 값이 `(error . payload)` 형태의 에러 값인지 확인합니다.
(define (is-error? val)
  (and (pair? val) (eq? (car val) 'error)))
;; 에러 값에서 실제 payload를 추출합니다.
(define (error-payload val)
  (cdr val))

(define-category try-catch-interface
  (objects
   (instance try-body category-builder #f 'Replace)
   (instance catch-body category-builder #f 'Replace)
   (instance body-in Data #f)
   (instance result Data #f 'Select-Non-F)))

(define-category internal-cells
  (objects
   (instance try-result Data #f 'Select-Non-F)
   (instance is-error Bool #f)
   (instance error-payload Data #f)))

(define-cpnet-system try-catch-system
  (try-catch-interface)
  (add-subsystem! (current-system) (apply-gate 'try-impl))
  (add-subsystem! (current-system) (apply-gate 'catch-impl))
  (internal-cells)

  ;; Wire up try-body
  (wire (get-cell 'try-catch-interface 'try-body) (get-cell 'try-impl 'apply-interface 'code))
;; Named helpers to avoid inline lambda propagators in try-catch wiring.
;; `try-catch-system`의 최종 결과를 라우팅하는 핸들러.
;; 에러가 발생하면 `catch-impl`이 결과를 쓰도록 하고, 아니면 `try-impl`의 결과를 전달합니다.
(define (route-final-handler vals _)
  (let ((try-res (car vals))
        (is-err? (cadr vals)))
    (if is-err?
        (cons *nothing* '()) ; if error, let catch-impl write to result
        (cons try-res '()))))

;; Handler to check for error payloads and emit the is-error / error-payload pair.
;; 에러 페이로드를 확인하고, `is-error` 플래그와 `error-payload` 쌍을 방출하는 핸들러.
(define (check-error-handler vals _)
  (let ((res (car vals)))
    (if (is-error? res)
        (cons (list #t (error-payload res)) '())
        (cons (list #f #f) '()))))

  (propagator setup-try-args
    (get-cell 'try-catch-interface 'body-in)
    -> (get-cell 'try-impl 'apply-interface 'args)
    pass-as-list)
  (propagator setup-try-results
    (get-cell 'internal-cells 'try-result)
    -> (get-cell 'try-impl 'apply-interface 'results)
    pass-as-list)

  ;; Check for error (named handler)
  (propagator check-error
    (get-cell 'internal-cells 'try-result)
    -> (list (get-cell 'internal-cells 'is-error)
             (get-cell 'internal-cells 'error-payload))
    check-error-handler)

  ;; Wire up catch-body
  (wire (get-cell 'try-catch-interface 'catch-body) (get-cell 'catch-impl 'apply-interface 'code))
  (propagator setup-catch-args
    (get-cell 'internal-cells 'error-payload)
    -> (get-cell 'catch-impl 'apply-interface 'args)
    pass-as-list)
  (propagator setup-catch-results
    (get-cell 'try-catch-interface 'result)
    -> (get-cell 'catch-impl 'apply-interface 'results)
    pass-as-list)

  ;; Route final result
  (propagator route-final
    (list (get-cell 'internal-cells 'try-result)
          (get-cell 'internal-cells 'is-error))
    -> (get-cell 'try-catch-interface 'result)
    route-final-handler)

