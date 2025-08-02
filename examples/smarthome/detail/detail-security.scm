(define-module (examples smarthome detail detail-security)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:export (door-sensor security-system))

;; Sub-System: Door Sensor
(define-category door-sensor
  (cells (Cell state 'closed))) ;; open, closed

;; Sub-System: Security System
(define-category security-system
  (cells (Cell status 'disarmed) ; disarmed, armed_away, armed_home
         (Cell door_input 'closed)
         (Cell alarm_state 'off)) ; off, on
  (propagators
   ((prop p-sec-on-door-change door_input -> alarm_state)
    (lambda (door-state src-cell)
      (let* ((parts (string-split (symbol->string (cell-id src-cell)) #\.))
             (cat-name (string->symbol (if (= (length parts) 3) (list-ref parts 1) (car parts))))
             (sec-status (get-cell-value cat-name 'status)))
        (if (and (or (eq? sec-status 'armed_away) (eq? sec-status 'armed_home))
                 (eq? door-state 'open))
            (cons 'on '())
            (cons #f '())))))
   ((prop p-sec-on-status-change status -> alarm_state)
    (lambda (sec-status src-cell)
      (if (eq? sec-status 'disarmed)
          (cons 'off '())
          (cons #f '()))))))
