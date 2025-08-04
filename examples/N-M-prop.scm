(define-module (examples N-M-prop)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  )

(define-object Temperature)
(define-object Precipitation)
(define-object Flag)

(define-category weather-station
  (objects
   (instance outside_temp Temperature 78 default-merge-fn)
   (instance precipitation Precipitation 'none default-merge-fn)
   (instance get-umb Flag #f default-merge-fn)
   (instance jacket_needed Flag #f default-merge-fn))
  (morphisms
   ;; N:N propagator example
   ((morphism clothing_advice (outside_temp precipitation) -> (get-umb jacket_needed))
    (lambda (vals src)
      (let ((temp (car vals)) (prec (cadr vals)))
        (cons (list (eq? prec 'rain) (< temp 65)) '()))))))

(define-object TimeOfDay)
(define-object HomeStatus)
(define-object LightStatus)
(define-object ThermostatSetting)
(define-object SecurityStatus)

(define-category home-automation
  (objects
   (instance time_of_day TimeOfDay 'day default-merge-fn)
   (instance is_home HomeStatus #f default-merge-fn)
   (instance lights_on LightStatus #f default-merge-fn)
   (instance thermostat_setting ThermostatSetting 'eco default-merge-fn)
   (instance security_armed SecurityStatus #t default-merge-fn))
  (morphisms
   ;; N:1 propagator example
   ((morphism light_control (time_of_day is_home) -> lights_on)
    (lambda (vals src)
      (let ((time (car vals)) (home? (cadr vals)))
        (cons (and home? (or (eq? time 'evening) (eq? time 'night)))
              '()))))
   ;; 1:1 propagator example for security system based on presence
   ((morphism presence_actions is_home -> security_armed)
    (lambda (home? src)
      (if home?
          (cons #f '())    ;; not armed
          (cons #t '())))))) ;; armed

(define-connections internal-smarthome-connections
  (propagator thermostat-controller
              (list (get-cell 'home-automation 'is_home)
                    (get-cell 'weather-station 'outside_temp))
              ->
              (get-cell 'home-automation 'thermostat_setting)
              (lambda (vals _)
                (let ((is-home (car vals))
                      (temp (cadr vals)))
                  (cond ((< temp 40) (cons 'comfort '())) ;; Safety rule has priority
                        (is-home (cons 'comfort '()))
                        (else (cons 'eco '())))))))

(define-object DisplayString)
(define-object Power)

(define-category alert_panel
  (objects
   (instance alert_message DisplayString "System Nominal" replace-merge-fn)
   (instance siren_on Flag #f replace-merge-fn)))

(define-connections display-to-alert-connections
  (propagator status->alert
              (list (get-cell 'display_panel 'home_status_verbose)
                    (get-cell 'display_panel 'weather_status))
              ->
              (get-cell 'alert_panel 'alert_message)
              (lambda (vals _)
                (let ((home (car vals))
                      (weather (cadr vals)))
                  (cond ((and (equal? home "User at Home") (equal? weather "WARNING"))
                         (cons "ALERT: User at home during weather hazard" '()))
                        ((equal? weather "WARNING")
                         (cons "ALERT: Weather hazard detected" '()))
                        (else (cons "System Nominal" '()))))))
  (propagator alert->siren
              (get-cell 'alert_panel 'alert_message)
              ->
              (get-cell 'alert_panel 'siren_on)
              (lambda (msg _)
                (if (string-contains msg "ALERT")
                    (cons #t '())
                    (cons #f '())))))

(define-category display_panel
  (objects
   (instance home_status_simple DisplayString "EMPTY" replace-merge-fn)
   (instance home_status_verbose DisplayString "House is Vacant" replace-merge-fn)
   (instance weather_status DisplayString "CLEAR" replace-merge-fn)
   (instance power_consumption Power 0 max-merge-fn))
  (morphisms
   ((morphism simple->verbose_translator home_status_simple -> home_status_verbose)
    (lambda (v _)
      (cond ((equal? v "OCCUPIED") (cons "User at Home" '()))
            ((equal? v "EMPTY") (cons "House is Vacant" '()))
            (else (cons *nothing* '())))))
   ((morphism update_power_consumption (home_status_verbose weather_status) -> power_consumption)
    (lambda (vals _)
      (let ((home-status (car vals))
            (weather-status (cadr vals)))
        (let ((power (+ (if (equal? home-status "User at Home") 5 1)
                        (if (equal? weather-status "WARNING") 10 1))))
          (cons power '())))))))

(define-scenario run-weather-scenario
  (show-state "--- [Weather] Initial State ---")
  (trigger (get-cell 'weather-station 'outside_temp)   95) (run) (show-state "[Weather] Hot, no rain")
  (trigger (get-cell 'weather-station 'outside_temp)   60) (run) (show-state "[Weather] Cold, no rain")
  (trigger (get-cell 'weather-station 'precipitation) 'rain) (run) (show-state "[Weather] Cold and rainy")
  (trigger (get-cell 'weather-station 'outside_temp)   80) (run) (show-state "[Weather] Warm and rainy")
  (trigger (get-cell 'weather-station 'precipitation) 'none) (run) (show-state "[Weather] Warm, no rain")
  (trigger (get-cell 'weather-station 'outside_temp)   35) (run) (show-state "[Weather] Very cold, thermostat set to comfort"))

(define-scenario run-home-automation-scenario
  (show-state "--- [Home] Initial State (away, day) ---")
  (trigger (get-cell 'home-automation 'is_home) #t) (run) (show-state "[Home] Arrived home during day")
  (trigger (get-cell 'home-automation 'time_of_day) 'evening) (run) (show-state "[Home] Evening at home")
  (trigger (get-cell 'home-automation 'is_home) #f) (run) (show-state "[Home] Left home during evening")
  (trigger (get-cell 'home-automation 'time_of_day) 'day) (run) (show-state "[Home] Away during day"))

(define-scenario run-display-panel-scenario
  (show-state "--- [Display Panel] Initial State ---")
  (trigger (get-cell 'home-automation 'is_home) #t) (run)
  (show-state "[Display Panel] User arrived, power consumption should increase. Alert nominal.")
  (trigger (get-cell 'weather-station 'precipitation) 'rain) (run)
  (show-state "[Display Panel] Rain started, power/alert increase.")
  (trigger (get-cell 'home-automation 'is_home) #f) (run)
  (show-state "[Display Panel] User left, power decreases, alert remains for weather.")
  (trigger (get-cell 'weather-station 'precipitation) 'none) (run)
  (show-state "[Display Panel] Rain stopped, power/alert minimal."))

(define-cpnet-system SmartHomeSystem
  (weather-station)
  (home-automation)
  (internal-smarthome-connections))

(define-cpnet-system display-panel-system
  (display_panel)
  (alert_panel)
  (display-to-alert-connections))

(compose-systems
 (systems SmartHomeSystem display-panel-system)
 (connections)
 (execution
  (define F-simple-view
    (make-system-functor
     (from SmartHomeSystem.home-automation)
     (to display-panel-system.display_panel)
     (mappings (is_home -> home_status_simple))))

  (define G-verbose-view
    (make-system-functor
     (from SmartHomeSystem.home-automation)
     (to display-panel-system.display_panel)
     (mappings (is_home -> home_status_verbose))))

  (define-nt nt-view-changer F-simple-view G-verbose-view
    (component is_home display_panel.simple->verbose_translator))

  (apply-functor-as-connections F-simple-view
   (mappings
    (is_home -> home_status_simple (lambda (v _) (cons (if v "OCCUPIED" "EMPTY") '())))))
  
  (apply-functor-as-connections G-verbose-view
   (mappings
    (is_home -> home_status_verbose (lambda (v _) (cons (if v "User at Home" "House is Vacant") '())))))

  ;; Connect the weather system as before.
  (apply-functor-as-connections
   (make-system-functor
    (from SmartHomeSystem.weather-station)
    (to display-panel-system.display_panel)
    (mappings (precipitation -> weather_status)))
   (mappings
    (precipitation -> weather_status (lambda (v _) (cons (if (eq? v 'rain) "WARNING" "CLEAR") '())))))
  
  (visualize "composed-system.dot")
  (run-weather-scenario)
  (run-home-automation-scenario)
  (run-display-panel-scenario)))

