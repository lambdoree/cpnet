(define-module (examples N-M-prop)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet category)
  #:use-module (cpnet functor)
  #:use-module (cpnet nt)
  #:use-module (cpnet system)
  #:use-module (srfi srfi-1)
  )

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
      (let ((temp (car vals))
	    (prec (cadr vals)))
        (cons (list (eq? prec 'rain) (< temp 65)) '()))))))

(define-category home-automation
  (objects
   (instance time_of_day TimeOfDay 'day default-merge-fn)
   (instance is_home HomeStatus #f default-merge-fn)
   (instance lights_on LightStatus #f default-merge-fn)
   (instance thermostat_setting ThermostatSetting 'eco default-merge-fn)
   (instance security_armed SecurityStatus #t default-merge-fn)
   (instance power_save_mode Flag #f replace-merge-fn))
  (morphisms
   ((morphism light_control (time_of_day is_home) -> lights_on)
    (lambda (vals src)
      (let ((time (car vals)) 
	    (home? (cadr vals)))
        (cons (and home? (or (eq? time 'evening) (eq? time 'night)))
              '()))))))

(define-connections internal-smarthome-connections
  (propagator thermostat-controller
              (list (get-cell 'home-automation 'is_home)
                    (get-cell 'weather-station 'outside_temp)
                    (get-cell 'home-automation 'power_save_mode)
                    (get-cell 'guest_mode 'active))
              ->
              (get-cell 'home-automation 'thermostat_setting)
              (lambda (vals _)
                (let ((is-home (car vals))
                      (temp (cadr vals))
                      (power-save? (caddr vals))
                      (guests? (cadddr vals)))
                  (cond ((< temp 40) (cons 'comfort '()))
                        (guests? (cons 'comfort '()))
                        (power-save? (cons 'eco '()))
                        (is-home (cons 'comfort '()))
                        (else (cons 'eco '()))))))
  (propagator presence_actions
              (list (get-cell 'home-automation 'is_home)
                    (get-cell 'guest_mode 'active))
              ->
              (get-cell 'home-automation 'security_armed)
              (lambda (vals _)
                (let ((home? (car vals))
                      (guests? (cadr vals)))
                  (if (or home? guests?)
                      (cons #f '())
                      (cons #t '()))))))

(define-category guest_mode
  (objects
   (instance active Flag #f default-merge-fn)))

(define-category alert_panel
  (objects
   (instance alert_message DisplayString "System Nominal" replace-merge-fn)
   (instance siren_on Flag #f replace-merge-fn))
  (morphisms
   ((morphism alert->siren alert_message -> siren_on)
    (lambda (vals _)
      (let ((msg (car vals)))
        (if (and (string? msg) (string-contains msg "ALERT"))
            (cons #t '())
            (cons #f '())))))))

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
                        (else (cons "System Nominal" '())))))))

(define-category display_panel
  (objects
   (instance home_status_simple DisplayString "EMPTY" replace-merge-fn)
   (instance home_status_verbose DisplayString "House is Vacant" replace-merge-fn)
   (instance executive_summary DisplayString "UNKNOWN" replace-merge-fn)
   (instance weather_status DisplayString "CLEAR" replace-merge-fn)
   (instance power_consumption Power 0 max-merge-fn))
  (morphisms
   ((morphism simple->verbose_translator home_status_simple -> home_status_verbose)
    (lambda (vals _)
      (let ((v (car vals)))
        (cond ((equal? v "OCCUPIED") (cons "User at Home" '()))
              ((equal? v "EMPTY") (cons "House is Vacant" '()))
              (else (cons *nothing* '()))))))
   ((morphism summarize_verbose_status home_status_verbose -> executive_summary)
    (lambda (vals _)
      (let ((v (car vals)))
        (cond ((and (string? v) (string-contains v "User at Home")) (cons "ALL OK" '()))
              ((and (string? v) (string-contains v "House is Vacant")) (cons "SYSTEM NOMINAL" '()))
              (else (cons "UNKNOWN" '()))))))))

(define-connections display-compute-connections
  (propagator update_power_consumption
	      (list (get-cell 'display_panel 'home_status_verbose)
                    (get-cell 'display_panel 'weather_status))
	      ->
	      (get-cell 'display_panel 'power_consumption)
	      (lambda (vals _)
                (let ((home-status (car vals))
		      (weather-status (cadr vals)))
                  (let ((power (+ (if (equal? home-status "User at Home") 5 1)
                                  (if (equal? weather-status "WARNING") 10 1))))
                    (cons power '()))))))

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

(define-scenario run-power-management-scenario
  (show-state "--- [Power Management] Initial State: Away, cold, no rain ---")
  (trigger (get-cell 'home-automation 'is_home) #f)
  (trigger (get-cell 'weather-station 'outside_temp) 60)
  (trigger (get-cell 'weather-station 'precipitation) 'none) (run)
  (show-state "[Power Management] Thermostat is ECO.")
  (trigger (get-cell 'weather-station 'precipitation) 'rain) (run)
  (show-state "[Power Management] Power overload, power-save enabled, thermostat remains ECO.")
  (trigger (get-cell 'weather-station 'outside_temp) 35) (run)
  (show-state "[Power Management] Very cold, safety override engages, thermostat is COMFORT.")
  (trigger (get-cell 'home-automation 'is_home) #t) (run)
  (show-state "[Power Management] User is home, power-save disabled, thermostat remains COMFORT."))

(define-scenario run-guest-mode-scenario
  (show-state "--- [Guest Mode] Initializing: User home, no guests ---")
  (trigger (get-cell 'home-automation 'is_home) #t)
  (trigger (get-cell 'guest_mode 'active) #f)
  (run) (show-state "[Guest Mode] Initial state")
  
  (trigger (get-cell 'guest_mode 'active) #t)
  (run) (show-state "[Guest Mode] Guest mode is now ACTIVE")
  
  (trigger (get-cell 'home-automation 'is_home) #f)
  (run) (show-state "[Guest Mode] User is AWAY, guests present. Security OFF, Thermostat COMFORT")

  (trigger (get-cell 'guest_mode 'active) #f)
  (run) (show-state "[Guest Mode] Guests have left. Security ON, Thermostat depends on temp"))

(define-scenario run-summary-scenario
  (show-state "--- [Summary View] Initializing system state ---")
  (trigger (get-cell 'home-automation 'is_home) #f)
  (trigger (get-cell 'guest_mode 'active) #f)
  (run) (show-state "[Summary View] User is away, all views should be consistent")
  (trigger (get-cell 'home-automation 'is_home) #t)
  (run) (show-state "[Summary View] User is home, all views should update consistently"))

(define-cpnet-system SmartHomeSystem
  (weather-station)
  (home-automation)
  (guest_mode)
  (internal-smarthome-connections))

(define-cpnet-system display-panel-system
  (display_panel)
  (alert_panel)
  (display-to-alert-connections))

(compose-systems
 (systems SmartHomeSystem display-panel-system)
 (connections
  (display-compute-connections)
  (propagator power_overload_manager
	      (list (get-cell 'display-panel-system.display_panel 'power_consumption)
                    (get-cell 'SmartHomeSystem.home-automation 'is_home)
                    (get-cell 'SmartHomeSystem.weather-station 'outside_temp))
	      ->
	      (get-cell 'SmartHomeSystem.home-automation 'power_save_mode)
	      (lambda (vals _)
                (let ((power (car vals))
		      (is-home (cadr vals))
                      (temp (caddr vals)))
                  (if (and (> power 10) (not is-home) (>= temp 40))
		      (cons #t '())
		      (cons #f '()))))))
 (execution
  (define F-simple-view
    (make-system-functor
     (name F-simple-view)
     (from SmartHomeSystem.home-automation)
     (to display-panel-system.display_panel)
     (mappings (is_home -> home_status_simple))))

  (define G-verbose-view
    (make-system-functor
     (name G-verbose-view)
     (from SmartHomeSystem.home-automation)
     (to display-panel-system.display_panel)
     (mappings (is_home -> home_status_verbose))))

  (define-nt nt-view-changer F-simple-view G-verbose-view
    (component is_home display_panel.simple->verbose_translator))

  (define H-ExecutiveSummary
    (make-system-functor
     (name H-ExecutiveSummary)
     (from SmartHomeSystem.home-automation)
     (to display-panel-system.display_panel)
     (mappings (is_home -> executive_summary))))

  (define-nt nt-summarize-view G-verbose-view H-ExecutiveSummary
    (component is_home display_panel.summarize_verbose_status))

  (apply-functor-as-connections F-simple-view
				(mappings
				 (is_home -> home_status_simple (lambda (vals _) (cons (if (car vals) "OCCUPIED" "EMPTY") '())))))
  
  (apply-functor-as-connections G-verbose-view
				(mappings
				 (is_home -> home_status_verbose (lambda (vals _) (cons (if (car vals) "User at Home" "House is Vacant") '())))))

  (apply-functor-as-connections H-ExecutiveSummary
				(mappings
				 (is_home -> executive_summary (lambda (vals _) (cons (if (car vals) "ALL OK" "SYSTEM NOMINAL") '())))))

  ;; Connect the weather system as before.
  (apply-functor-as-connections
   (make-system-functor
    (from SmartHomeSystem.weather-station)
    (to display-panel-system.display_panel)
    (mappings (precipitation -> weather_status)))
   (mappings
    (precipitation -> weather_status (lambda (vals _) (cons (if (eq? (car vals) 'rain) "WARNING" "CLEAR") '())))))
  
  (visualize "composed-system.dot")
  (run-weather-scenario)
  (run-home-automation-scenario)
  (run-display-panel-scenario)
  (run-power-management-scenario)
  (run-guest-mode-scenario)
  (run-summary-scenario)))
