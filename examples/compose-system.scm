(define-module (examples compose-system)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet category)
  #:use-module (cpnet system)
  #:use-module (examples smarthome wall-switch)
  #:use-module (examples weather weather))

(define-connections misc-connections
  (propagator p-adjust-desired-temp-from-outside thermostat.outside_temp_f -> thermostat.desired_temp
    (lambda (outside-temp src-cell)
      (cond ((> outside-temp 90) (cons 68 '()))   ;; It's hot, cool down more
            ((< outside-temp 32) (cons 72 '()))   ;; It's freezing, heat up more
            (else (cons 70 '())))))) ;; default desired temp

(define-execution run-weather-interaction-scenario
  (show-state "--- Weather System Influences Smarthome ---")
  (trigger weather-station.outside_temp 95)
  (run)
  (show-state "Outside temp is hot (95F), desired temp should adjust to 68F")
  (trigger thermostat.mode 'cool)
  (trigger thermostat.current_temp 72)
  (run)
  (show-state "AC should turn on due to adjusted desired temp"))

(define ComposedSys
  (compose-systems
   (systems WallSwitchSystemDef WeatherSystemDef)
   (connections
    (misc-connections)
    (define-system-functor weather->thermostat-functor
      (from weather-station)
      (to thermostat)
      (mappings (outside_temp -> outside_temp_f)))
    (apply-functor-as-connections weather->thermostat-functor))
   (execution
    (run-smarthome-scenario)
    (run-weather-interaction-scenario))))
