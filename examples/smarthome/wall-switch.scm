(define-module (examples smarthome wall-switch)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (examples smarthome detail detail-hvac)
  #:use-module (examples smarthome detail detail-power)
  #:use-module (examples smarthome detail detail-security)
  #:use-module (examples smarthome detail detail-wall-switch)
  #:export (WallSwitchSystemDef run-smarthome-scenario))

(define-connections wall-switch-to-light-connections
  (propagator p-switch-to-light wall-switch.switch_state -> light-bulb.state
    (lambda (v src-cell)
      (let ((light-state (if (eq? v 'on) 'on 'off)))
        (cons light-state '())))))

(define-connections light-to-power-meter-connections
  (propagator p-light-to-power light-bulb.state -> power-meter.power_usage
    (lambda (v src-cell)
      (let ((usage (if (eq? v 'on) 60 0)))
        (cons usage '())))))

(define-connections power-meter-to-aggregator-connections
  (propagator p-meter-to-aggregator power-meter.power_usage -> power-aggregator.power_in_1
    (lambda (v src-cell)
      (cons v '()))))

(define-connections thermostat-to-ac-connections
  (propagator p-thermostat-to-ac thermostat.ac_command -> air-conditioner.state
    (lambda (v src-cell) (cons v '()))))

(define-connections thermostat-to-heater-connections
  (propagator p-thermostat-to-heater thermostat.heater_command -> heater.state
    (lambda (v src-cell) (cons v '()))))

(define-connections ac-to-power-aggregator-connections
  (propagator p-ac-to-aggregator air-conditioner.power_draw -> power-aggregator.power_in_2
    (lambda (v src-cell) (cons v '()))))

(define-connections heater-to-power-aggregator-connections
  (propagator p-heater-to-aggregator heater.power_draw -> power-aggregator.power_in_3
    (lambda (v src-cell) (cons v '()))))

(define-connections door-to-security-connections
  (propagator p-door-to-security door-sensor.state -> security-system.door_input
    (lambda (v src-cell) (cons v '()))))

(define-execution run-smarthome-scenario
  (show-state "Initial State")
  (trigger wall-switch.button_press #t)
  (run)
  (show-state "After 1st press (auto -> on), light is on, drawing power")
  (trigger wall-switch.button_press #t)
  (run)
  (show-state "After 2nd press (on -> off), light is off, no power")
  (trigger wall-switch.button_press #t)
  (run)
  (show-state "After 3rd press (off -> on), light is on, drawing power")

  (show-state "--- Thermostat Scenario (Cooling) ---")
  (trigger thermostat.current_temp 75)
  (run)
  (show-state "Temp rises, AC turns on, total power increases")
  (trigger thermostat.current_temp 68)
  (run)
  (show-state "Temp drops, AC turns off")

  (show-state "--- Thermostat Scenario (Heating) ---")
  (trigger thermostat.mode 'heat)
  (run)
  (show-state "Mode set to heat, nothing happens yet")
  (trigger thermostat.current_temp 65)
  (run)
  (show-state "Temp drops, Heater turns on")
  (trigger thermostat.current_temp 71)
  (run)
  (show-state "Temp rises, Heater turns off")

  (show-state "--- Security System Scenario ---")
  (trigger security-system.status 'armed_away)
  (run)
  (show-state "Security system is armed")
  (trigger door-sensor.state 'open)
  (run)
  (show-state "Door opens, alarm sounds")
  (trigger door-sensor.state 'closed)
  (run)
  (show-state "Door closes, alarm remains on")
  (trigger security-system.status 'disarmed)
  (run)
  (show-state "System disarmed, alarm turns off"))

(define-cpnet-system WallSwitchSystemDef
  (wall-switch)
  (light-bulb)
  (power-meter)
  (power-aggregator)
  (thermostat)
  (air-conditioner)
  (heater)
  (door-sensor)
  (security-system)
  (wall-switch-to-light-connections)
  (light-to-power-meter-connections)
  (power-meter-to-aggregator-connections)
  (thermostat-to-ac-connections)
  (thermostat-to-heater-connections)
  (ac-to-power-aggregator-connections)
  (heater-to-power-aggregator-connections)
  (door-to-security-connections))

