(define-module (examples N-M-prop)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  )

(define-category weather-station
  (cells
   (Cell outside_temp 78 default-merge-fn)
   (Cell precipitation 'none default-merge-fn)
   (Cell get-umb #f default-merge-fn)
   (Cell jacket_needed #f default-merge-fn))
  (propagators
   ;; N:N propagator example
   ((prop clothing_advice (outside_temp precipitation) -> (get-umb jacket_needed))
    (lambda (vals src)
      (let ((temp (car vals)) (prec (cadr vals)))
        (cons (list (eq? prec 'rain) (< temp 65)) '()))))))

(define-category home-automation
  (cells
   (Cell time_of_day 'day default-merge-fn)
   (Cell is_home #f default-merge-fn)
   (Cell lights_on #f default-merge-fn)
   (Cell thermostat_setting 'eco default-merge-fn)
   (Cell security_armed #t default-merge-fn))
  (propagators
   ;; N:1 propagator example
   ((prop light_control (time_of_day is_home) -> lights_on)
    (lambda (vals src)
      (let ((time (car vals)) (home? (cadr vals)))
        (cons (and home? (or (eq? time 'evening) (eq? time 'night)))
              '()))))
   ;; 1:N propagator example
   ((prop presence_actions is_home -> (thermostat_setting security_armed))
    (lambda (home? src)
      (if home?
          (cons (list 'comfort #f) '())
          (cons (list 'eco #t) '()))))))

(define-execution run-weather-scenario
  (show-state "--- [Weather] Initial State ---")
  (trigger weather-station.outside_temp   95) (run) (show-state "[Weather] Hot, no rain")
  (trigger weather-station.outside_temp   60) (run) (show-state "[Weather] Cold, no rain")
  (trigger weather-station.precipitation 'rain) (run) (show-state "[Weather] Cold and rainy")
  (trigger weather-station.outside_temp   80) (run) (show-state "[Weather] Warm and rainy")
  (trigger weather-station.precipitation 'none) (run) (show-state "[Weather] Warm, no rain"))

(define-execution run-home-automation-scenario
  (show-state "--- [Home] Initial State (away, day) ---")
  (trigger home-automation.is_home #t) (run) (show-state "[Home] Arrived home during day")
  (trigger home-automation.time_of_day 'evening) (run) (show-state "[Home] Evening at home")
  (trigger home-automation.is_home #f) (run) (show-state "[Home] Left home during evening")
  (trigger home-automation.time_of_day 'day) (run) (show-state "[Home] Away during day"))

(define-cpnet-system CpnetExampleSystem
  (weather-station)
  (home-automation)
  (run-weather-scenario)
  (run-home-automation-scenario))

