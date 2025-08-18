(define-module (examples N-M-prop)
  #:use-module (cpnet dsl)
  #:use-module (cpnet scenario)
  #:use-module (cpnet core)
  #:use-module (cpnet category)
  #:use-module (cpnet functor)
  #:use-module (cpnet nt)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime)
  #:use-module (cpnet runtime stepper)
  #:use-module (cpnet runtime state)
  #:use-module (ice-9 match)
  #:use-module (icnu icnu)
  #:use-module (icnu stdlib icnu-lib)
  #:use-module (srfi srfi-1))

;; `show-state` 매크로 출력에서 모니터링할 cell들을 정의합니다.
(*cells-to-show*
 '(SmartHomeSystem.weather-station.outside_temp
   SmartHomeSystem.weather-station.precipitation
   SmartHomeSystem.weather-station.get-umb
   SmartHomeSystem.weather-station.jacket_needed
   SmartHomeSystem.home-automation.is_home
   SmartHomeSystem.home-automation.lights_on
   SmartHomeSystem.home-automation.thermostat_setting
   display-panel-system.display_panel.power_consumption
   display-panel-system.alert_panel.alert_message
   display-panel-system.alert_panel.siren_on))

;; 날씨 정보를 처리하는 `weather-station` 카테고리를 정의합니다.
(define-category weather-station
  (objects
   (instance outside_temp Temperature 78)
   (instance precipitation Precipitation 'none)
   (instance get-umb Flag *nothing*)
   (instance jacket_needed Flag *nothing*))
  (morphisms
   ;; N:N morphism converted to IC^ν: produce a 2-tuple/list [ (=prec 'rain) (< temp 65) ] on 'out'.
   ((morphism clothing_advice (outside_temp precipitation) -> (get-umb jacket_needed))
    (icnu
     (nu (ws-prec-eq ws-temp-lt ws-clothing-nil ws-clothing-cons1 ws-clothing-cons2)
	 (par
          ;; Compare precipitation == 'rain -> ws-prec-eq
          (IC_EQ_CONST 'rain weather-station.precipitation ws-prec-eq)
          ;; Compare temp < 65 -> ws-temp-lt
          (IC_LT_CONST 65 weather-station.outside_temp ws-temp-lt)

          ;; Build the result list: '(#t #f)
          (IC_NIL ws-clothing-nil)
          (IC_CONS ws-temp-lt ws-clothing-nil ws-clothing-cons1)
          (IC_CONS ws-prec-eq ws-clothing-cons1 ws-clothing-cons2)
          
          ;; NOTE: The 'out' node wiring was removed. For N:M propagators where each
          ;; output is computed independently, wire each result to its target's
          ;; aux port directly. The runtime's fan-out from a single 'out' node
          ;; is for cases where all targets should receive the same value.
          
          ;; >>> 핵심: 실제 목적지로 직결 전파 <<<
          ;; 비 == 'rain 이면 get-umb = #t
          (mk-wire ws-prec-eq 'p weather-station.get-umb 'r)
          ;; temp < 65 이면 jacket_needed = #t
          (mk-wire ws-temp-lt 'p weather-station.jacket_needed 'r)))))))

;; 시간, 재실 여부 등 집의 상태를 관리하는 `home-automation` 카테고리를 정의합니다.
(define-category home-automation
  (objects
   (instance time_of_day TimeOfDay 'day)
   (instance is_home HomeStatus #f)
   (instance lights_on LightStatus *nothing*)
   (instance thermostat_setting ThermostatSetting *nothing*)
   (instance security_armed SecurityStatus *nothing*)
   (instance power_save_mode Flag *nothing*))
  (morphisms
   ((morphism light_control (time_of_day is_home) -> lights_on)
    (icnu
     (nu (ha-lc-tod-copy ha-lc-lit-true ha-lc-lit-false ha-lc-eq-evening ha-lc-eq-night ha-lc-time-is-even-or-night ha-lc-should-lights-on)
         (par
          ;; Hoist literals to the top level of the par block
          (IC_LITERAL #t ha-lc-lit-true)
          (IC_LITERAL #f ha-lc-lit-false)
          ;; Fan-out the time_of_day input because it is used by two gadgets below.
          (mk-node ha-lc-tod-copy 'C)
          (mk-wire home-automation.time_of_day 'p ha-lc-tod-copy 'p)
          ;; Compare time == evening or time == night, using the fanned-out value
          (IC_EQ_CONST 'evening (list ha-lc-tod-copy 'l) ha-lc-eq-evening)
          (IC_EQ_CONST 'night   (list ha-lc-tod-copy 'r) ha-lc-eq-night)
          (IC_OR ha-lc-eq-evening ha-lc-eq-night ha-lc-time-is-even-or-night)
          ;; AND with is_home
          (IC_AND is_home ha-lc-time-is-even-or-night ha-lc-should-lights-on)
          ;; expose as out
          (IC_IF ha-lc-should-lights-on (list ha-lc-lit-true 'r) (list ha-lc-lit-false 'r) out)))))))

;; 스마트홈 시스템 내부의 여러 카테고리 간의 연결(propagator)을 정의합니다.
(define-connections internal-smarthome-connections
  (propagator thermostat-controller
	      (list (get-cell 'home-automation 'is_home)
		    (get-cell 'weather-station 'outside_temp)
		    (get-cell 'home-automation 'power_save_mode)
		    (get-cell 'guest_mode 'active))
	      ->
	      (get-cell 'home-automation 'thermostat_setting)
	      (icnu
	       (nu (ha-tc-comfort-lit ha-tc-eco-lit ha-tc-temp-lt ha-tc-inner2 ha-tc-inner1 ha-tc-guest-out)
		   (par
                    ;; Literals for outputs
                    (IC_LITERAL 'comfort ha-tc-comfort-lit)
                    (IC_LITERAL 'eco ha-tc-eco-lit)
                    ;; temp < 40?
                    (IC_LT_CONST 40 weather-station.outside_temp ha-tc-temp-lt)
                    ;; inner-most: if is_home then comfort else eco -> ha-tc-inner2
                    (IC_IF home-automation.is_home (list ha-tc-comfort-lit 'r) (list ha-tc-eco-lit 'r) ha-tc-inner2)
                    ;; if power_save_mode then eco else ha-tc-inner2 -> ha-tc-inner1
                    (IC_IF home-automation.power_save_mode (list ha-tc-eco-lit 'r) (list ha-tc-inner2 'r) ha-tc-inner1)
                    ;; if guest_mode.active then comfort else ha-tc-inner1 -> ha-tc-guest-out
                    (IC_IF guest_mode.active (list ha-tc-comfort-lit 'r) (list ha-tc-inner1 'r) ha-tc-guest-out)
                    ;; final: if ha-tc-temp-lt then comfort else ha-tc-guest-out -> out
                    (IC_IF (list ha-tc-temp-lt 'p) (list ha-tc-comfort-lit 'r) (list ha-tc-guest-out 'r) out)))))
  (propagator presence_actions
	      (list (get-cell 'home-automation 'is_home)
		    (get-cell 'guest_mode 'active))
	      ->
	      (get-cell 'home-automation 'security_armed)
	      (icnu
	       (nu (ha-pa-false-lit ha-pa-true-lit ha-pa-or-flag)
		   (par
                    (IC_LITERAL #f ha-pa-false-lit)
                    (IC_LITERAL #t ha-pa-true-lit)
                    ;; or = OR(is_home, guest_mode.active)
                    (IC_OR home-automation.is_home guest_mode.active ha-pa-or-flag)
                    ;; if or then false else true -> out
                    (IC_IF ha-pa-or-flag (list ha-pa-false-lit 'r) (list ha-pa-true-lit 'r) out))))))

;; 손님 모드 활성화 여부를 관리하는 `guest_mode` 카테고리를 정의합니다.
(define-category guest_mode
  (objects
   (instance active Flag #f)))

;; 경고 메시지와 사이렌을 관리하는 `alert_panel` 카테고리를 정의합니다.
(define-category alert_panel
  (objects
   (instance alert_message DisplayString *nothing*)
   (instance siren_on Flag *nothing*))
  (morphisms
   ((morphism alert->siren alert_message -> siren_on)
    (icnu
     (nu (ap-as-lit-true ap-as-lit-false ap-as-aeq1 ap-as-aeq2 ap-as-alert-or)
	 (par
	  (IC_LITERAL #t ap-as-lit-true)
	  (IC_LITERAL #f ap-as-lit-false)
	  ;; Two equality checks for the known alert messages (conservative)
	  (IC_EQ_CONST "ALERT: User at home during weather hazard" alert_panel.alert_message ap-as-aeq1)
	  (IC_EQ_CONST "ALERT: Weather hazard detected" alert_panel.alert_message ap-as-aeq2)
	  (IC_OR ap-as-aeq1 ap-as-aeq2 ap-as-alert-or)
	  (IC_IF ap-as-alert-or (list ap-as-lit-true 'r) (list ap-as-lit-false 'r) out)))))))

;; 디스플레이 패널의 상태 정보를 경고 패널로 전달하는 연결을 정의합니다.
(define-connections display-to-alert-connections
  (propagator status->alert
	      (list (get-cell 'display_panel 'home_status_verbose)
		    (get-cell 'display_panel 'weather_status))
	      ->
	      (get-cell 'alert_panel 'alert_message)
	      (icnu
	       (nu (dp-sa-alert-1 dp-sa-alert-2 dp-sa-alert-3 dp-sa-home-eq dp-sa-weather-eq dp-sa-both-flag dp-sa-temp-branch)
		   (par
		    ;; Alert string literals
		    (IC_LITERAL "ALERT: User at home during weather hazard" dp-sa-alert-1)
		    (IC_LITERAL "ALERT: Weather hazard detected" dp-sa-alert-2)
		    (IC_LITERAL "System Nominal" dp-sa-alert-3)
		    ;; Comparisons
		    (IC_EQ_CONST "User at Home" display_panel.home_status_verbose dp-sa-home-eq)
		    (IC_EQ_CONST "WARNING" display_panel.weather_status dp-sa-weather-eq)
		    ;; dp-sa-home-eq AND dp-sa-weather-eq -> both
		    (IC_AND dp-sa-home-eq dp-sa-weather-eq dp-sa-both-flag)
		    ;; prepare intermediate holder for the two-stage choice
		    (mk-node dp-sa-temp-branch 'A)
		    ;; dp-sa-temp-branch = if dp-sa-weather-eq then dp-sa-alert-2 else dp-sa-alert-3
		    (IC_IF dp-sa-weather-eq (list dp-sa-alert-2 'r) (list dp-sa-alert-3 'r) dp-sa-temp-branch)
		    ;; out = if both then dp-sa-alert-1 else dp-sa-temp-branch
		    (IC_IF dp-sa-both-flag (list dp-sa-alert-1 'r) (list dp-sa-temp-branch 'r) out))))))

;; 다양한 상태 정보를 표시하는 `display_panel` 카테고리를 정의합니다.
(define-category display_panel
  (objects
   (instance home_status_simple DisplayString *nothing*)
   (instance home_status_verbose DisplayString *nothing*)
   (instance executive_summary DisplayString *nothing*)
   (instance weather_status DisplayString "CLEAR")
   (instance power_consumption Power *nothing*))
  (morphisms
   ((morphism simple->verbose_translator home_status_simple -> home_status_verbose)
    (icnu
     (nu (dp-sv-if-empty-branch dp-sv-occ-flag dp-sv-empty-flag dp-sv-lit-occ dp-sv-lit-empty dp-sv-lit-unknown)
	 (par
          ;; exact-match checks for simple->verbose translation
          (IC_EQ_CONST "OCCUPIED" display_panel.home_status_simple dp-sv-occ-flag)
          (IC_EQ_CONST "EMPTY"    display_panel.home_status_simple dp-sv-empty-flag)
          (IC_LITERAL "User at Home" dp-sv-lit-occ)
          (IC_LITERAL "House is Vacant" dp-sv-lit-empty)
          (IC_LITERAL "Unknown Status" dp-sv-lit-unknown)
          ;; if empty -> lit-empty else unknown
          (IC_IF dp-sv-empty-flag (list dp-sv-lit-empty 'r) (list dp-sv-lit-unknown 'r) dp-sv-if-empty-branch)
          ;; if occ -> lit-occ else (result of inner if)
          (IC_IF dp-sv-occ-flag (list dp-sv-lit-occ 'r) (list dp-sv-if-empty-branch 'r) out)))))
   ((morphism summarize_verbose_status home_status_verbose -> executive_summary)
    (icnu
     (nu (dp-su-if-vacant-branch dp-su-ue-flag dp-su-uv-flag dp-su-lit-all dp-su-lit-sys dp-su-lit-unknown)
	 (par
          ;; conservative exact-match based summarizer
          (IC_EQ_CONST "User at Home" display_panel.home_status_verbose dp-su-ue-flag)
          (IC_EQ_CONST "House is Vacant" display_panel.home_status_verbose dp-su-uv-flag)
          (IC_LITERAL "ALL OK" dp-su-lit-all)
          (IC_LITERAL "SYSTEM NOMINAL" dp-su-lit-sys)
          (IC_LITERAL "UNKNOWN" dp-su-lit-unknown)
          (IC_IF dp-su-uv-flag (list dp-su-lit-sys 'r) (list dp-su-lit-unknown 'r) dp-su-if-vacant-branch)
          (IC_IF dp-su-ue-flag (list dp-su-lit-all 'r) (list dp-su-if-vacant-branch 'r) out)))))))

;; 디스플레이 패널의 전력 소비량을 계산하는 로직을 정의합니다.
(define-connections display-compute-connections
  (propagator update_power_consumption
	      (list (get-cell 'display_panel 'home_status_verbose)
		    (get-cell 'display_panel 'weather_status))
	      ->
	      (get-cell 'display_panel 'power_consumption)
	      (icnu
	       (nu (dp-upc-num-5 dp-upc-num-1 dp-upc-num-10 dp-upc-home-eq dp-upc-weather-eq dp-upc-home-num dp-upc-weather-num)
		   (par
		    ;; numeric literals
		    (IC_LITERAL 5 dp-upc-num-5)
		    (IC_LITERAL 1 dp-upc-num-1)
		    (IC_LITERAL 10 dp-upc-num-10)
		    ;; comparisons
		    (IC_EQ_CONST "User at Home" display_panel.home_status_verbose dp-upc-home-eq)
		    (IC_EQ_CONST "WARNING" display_panel.weather_status dp-upc-weather-eq)
		    ;; pick home number: if dp-upc-home-eq then 5 else 1 -> dp-upc-home-num
		    (IC_IF dp-upc-home-eq (list dp-upc-num-5 'r) (list dp-upc-num-1 'r) dp-upc-home-num)
		    ;; pick weather number: if dp-upc-weather-eq then 10 else 1 -> dp-upc-weather-num
		    (IC_IF dp-upc-weather-eq (list dp-upc-num-10 'r) (list dp-upc-num-1 'r) dp-upc-weather-num)
		    ;; add dp-upc-home-num + dp-upc-weather-num -> out (use IC_PRIM_ADD)
		    (IC_PRIM_ADD dp-upc-home-num dp-upc-weather-num out))))))


;; 날씨, 자동화, 손님 모드 카테고리를 포함하는 `SmartHomeSystem`을 정의합니다.
(define-cpnet-system SmartHomeSystem
  (weather-station)
  (home-automation)
  (guest_mode)
  (internal-smarthome-connections))

;; 디스플레이와 경고 패널을 포함하는 `display-panel-system`을 정의합니다.
(define-cpnet-system display-panel-system
  (display_panel)
  (alert_panel)
  (display-to-alert-connections))

;; `SmartHomeSystem`과 `display-panel-system`을 하나의 `composed` 시스템으로 통합하고,
;; 두 시스템 간의 상호작용과 전체 실행 시나리오를 정의합니다.
(compose-systems
 (systems SmartHomeSystem display-panel-system)
 (connections
  (display-compute-connections)
  (propagator weather-status-prop
    (get-cell 'SmartHomeSystem.weather-station 'precipitation)
    ->
    (get-cell 'display-panel-system.display_panel 'weather_status)
    (icnu
     (nu (is-rain lit-warn lit-clear)
         (par
          (IC_EQ_CONST 'rain SmartHomeSystem.weather-station.precipitation is-rain)
          (IC_LITERAL "WARNING" lit-warn)
          (IC_LITERAL "CLEAR" lit-clear)
          (IC_IF is-rain (list lit-warn 'r) (list lit-clear 'r) out)))))
  (propagator home-status-prop
    (get-cell 'SmartHomeSystem.home-automation 'is_home)
    ->
    (get-cell 'display-panel-system.display_panel 'home_status_simple)
    (icnu
     (nu (lit-occ lit-empty)
         (par
          (IC_LITERAL "OCCUPIED" lit-occ)
          (IC_LITERAL "EMPTY" lit-empty)
          (IC_IF SmartHomeSystem.home-automation.is_home (list lit-occ 'r) (list lit-empty 'r) out)))))
  (propagator power_overload_manager
	      (list (get-cell 'display-panel-system.display_panel 'power_consumption)
		    (get-cell 'SmartHomeSystem.home-automation 'is_home)
		    (get-cell 'SmartHomeSystem.weather-station 'outside_temp))
	      ->
	      (get-cell 'SmartHomeSystem.home-automation 'power_save_mode)
	      (icnu
	       (nu (co-pom-lit-true co-pom-lit-false co-pom-gt-power co-pom-temp-ge-40 co-pom-not-is-home co-pom-and1 co-pom-combined)
		   (par
		    (IC_LITERAL #t co-pom-lit-true)
		    (IC_LITERAL #f co-pom-lit-false)
		    ;; power > 10 ?
		    (IC_GT_CONST 10 display-panel-system.display_panel.power_consumption co-pom-gt-power)
		    ;; temp >= 40  -> use IC_GT_CONST 39 on outside_temp to approximate >=40 as (> temp 39)
		    (IC_GT_CONST 39 SmartHomeSystem.weather-station.outside_temp co-pom-temp-ge-40)
		    ;; not is_home -> need to invert is_home
		    (IC_NOT SmartHomeSystem.home-automation.is_home co-pom-not-is-home)
		    ;; combined condition: co-pom-gt-power AND co-pom-not-is-home AND co-pom-temp-ge-40 -> co-pom-combined
		    (IC_AND co-pom-gt-power co-pom-not-is-home co-pom-and1)
		    (IC_AND co-pom-and1 co-pom-temp-ge-40 co-pom-combined)
		    ;; if co-pom-combined then true else false -> out
		    (IC_IF co-pom-combined (list co-pom-lit-true 'r) (list co-pom-lit-false 'r) out))))))
 (execution
  (begin
    (visualize "composed-system.dot")
    (newline)
    (run "--- [Weather] Initial State ---")
    (trigger (get-cell 'weather-station 'outside_temp) 95)
    (trigger (get-cell 'weather-station 'precipitation) 'none)
    (run "[Weather] Hot, no rain")
    (trigger (get-cell 'weather-station 'outside_temp) 60)
    (trigger (get-cell 'weather-station 'precipitation) 'none)
    (run "[Weather] Cold, no rain")
    (trigger (get-cell 'weather-station 'outside_temp) 60)
    (trigger (get-cell 'weather-station 'precipitation) 'rain)
    (run "[Weather] Cold and rainy")
    (trigger (get-cell 'weather-station 'outside_temp) 80)
    (trigger (get-cell 'weather-station 'precipitation) 'rain)
    (run "[Weather] Warm and rainy")
    (trigger (get-cell 'weather-station 'outside_temp) 80)
    (trigger (get-cell 'weather-station 'precipitation) 'none)
    (run "[Weather] Warm, no rain")
    (trigger (get-cell 'weather-station 'outside_temp) 35)
    (trigger (get-cell 'weather-station 'precipitation) 'none)
    (run "[Weather] Very cold, thermostat set to comfort")
    (newline)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'time_of_day) 'day)
    (run "--- [Home] Initial State (away, day) ---")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'time_of_day) 'day)
    (run "[Home] Arrived home during day")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'time_of_day) 'evening)
    (run "[Home] Evening at home")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'time_of_day) 'evening)
    (run "[Home] Left home during evening")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'time_of_day) 'day)
    (run "[Home] Away during day")
    (newline)
    (run "--- [Display Panel] Initial State ---")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (run "[Display Panel] User arrived, power consumption should increase. Alert nominal.")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'rain)
    (run "[Display Panel] Rain started, power/alert increase.")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'rain)
    (run "[Display Panel] User left, power decreases, alert remains for weather.")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'none)
    (run "[Display Panel] Rain stopped, power/alert minimal.")
    (newline)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'outside_temp) 35)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'none)
    (run "--- [Power Management] Initial State: Away, cold, no rain ---")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'outside_temp) 60)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'none)
    (run "[Power Management] Thermostat is ECO.")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'outside_temp) 60)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'rain)
    (run "[Power Management] Power overload, power-save enabled, thermostat remains ECO.")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'outside_temp) 35)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'rain)
    (run "[Power Management] Very cold, safety override engages, thermostat is COMFORT.")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'outside_temp) 35)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'rain)
    (run "[Power Management] User is home, power-save disabled, thermostat remains COMFORT.")
    (newline)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #f)
    (run "--- [Guest Mode] Initializing: User home, no guests ---")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #f)
    (run "[Guest Mode] Initial state")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #t)
    (run "[Guest Mode] Guest mode is now ACTIVE")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #t)
    (run "[Guest Mode] User is AWAY, guests present. Security OFF, Thermostat COMFORT")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #f)
    (run "[Guest Mode] Guests have left. Security ON, Thermostat depends on temp")
    (newline)
    (run "--- [Summary View] Initializing system state ---")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #f)
    (run "[Summary View] User is away, all views should be consistent")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #f)
    (run "[Summary View] User is home, all views should update consistently")
    (newline)
    (run "--- [Complex Scenario 1] Cold, rainy, user home with guests ---")
    (trigger (get-cell 'SmartHomeSystem.weather-station 'outside_temp) 35)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'rain)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #t)
    (run "[Complex 1] All systems interacting")
    (newline)
    (run "--- [Complex Scenario 2] Power management edge case ---")
    (trigger (get-cell 'SmartHomeSystem.weather-station 'outside_temp) 40)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'rain)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #f)
    (run "[Complex 2] Power-save mode should activate")
    (newline)
    (run "--- [Edge Case 1] Conflicting overrides: Cold safety vs. Power save ---")
    (trigger (get-cell 'SmartHomeSystem.weather-station 'outside_temp) 35)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'rain)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #f)
    (run "[Edge 1] Safety override should set thermostat to COMFORT despite power-save mode")
    (newline)
    (run "--- [Edge Case 2] Night time lights ---")
    (trigger (get-cell 'SmartHomeSystem.home-automation 'time_of_day) 'night)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #t)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #f)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'none)
    (run "[Edge 2] Lights should be ON at night when user is home")
    (newline)
    (run "--- [Edge Case 3] Guest mode vs. Power save ---")
    (trigger (get-cell 'SmartHomeSystem.weather-station 'outside_temp) 60)
    (trigger (get-cell 'SmartHomeSystem.weather-station 'precipitation) 'rain)
    (trigger (get-cell 'SmartHomeSystem.home-automation 'is_home) #f)
    (trigger (get-cell 'SmartHomeSystem.guest_mode 'active) #t)
    (run "[Edge 3] Guest mode should set thermostat to COMFORT, overriding power-save"))))
