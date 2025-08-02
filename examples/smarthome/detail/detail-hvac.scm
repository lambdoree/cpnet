(define-module (examples smarthome detail detail-hvac)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:export (thermostat air-conditioner heater))

(define (update-thermostat-commands)
  (let* ((mode (get-cell-value 'thermostat 'mode))
         (current (get-cell-value 'thermostat 'current_temp))
         (desired (get-cell-value 'thermostat 'desired_temp))
         (ac-cmd (if (and (eq? mode 'cool) (> current desired)) 'on 'off))
         (heater-cmd (if (and (eq? mode 'heat) (< current desired)) 'on 'off)))
    (cons ac-cmd (list (set-cell-effect 'thermostat 'heater_command heater-cmd)))))

;; Sub-System: Thermostat
(define-category thermostat
  (cells (Cell current_temp 72)
         (Cell desired_temp 70)
         (Cell mode 'cool) ; off, cool, heat
         (Cell ac_command 'off) ; on, off
         (Cell heater_command 'off) ; on, off
         (Cell outside_temp_f 78))
  (propagators
   ((prop p-therm-on-mode-change mode -> ac_command)
    (lambda (v s) (update-thermostat-commands)))
   ((prop p-therm-on-temp-change current_temp -> ac_command)
    (lambda (v s) (update-thermostat-commands)))
   ((prop p-therm-on-desired-change desired_temp -> ac_command)
    (lambda (v s) (update-thermostat-commands)))))

;; Sub-System: Air Conditioner
(define-category air-conditioner
  (cells (Cell state 'off) ; on, off
         (Cell power_draw 0))
  (propagators
   ((prop p-update-ac-power state -> power_draw)
    (lambda (state src-cell)
      (if (eq? state 'on) (cons 1500 '()) (cons 0 '()))))))

;; Sub-System: Heater
(define-category heater
  (cells (Cell state 'off) ; on, off
         (Cell power_draw 0))
  (propagators
   ((prop p-update-heater-power state -> power_draw)
    (lambda (state src-cell)
      (if (eq? state 'on) (cons 2000 '()) (cons 0 '()))))))
