(define-module (examples smarthome weather weather)
  #:use-module (cpnet dsl)
  #:use-module (examples smarthome weather detail detail-weather)
  #:export (WeatherSystemDef run-weather-scenario))

(define-execution run-weather-scenario
  (show-state "--- Initial Weather State ---")
  (trigger weather-station.outside_temp 95)
  (run)
  (show-state "Weather: Temp is hot (95F)")
  (trigger weather-station.outside_temp 20)
  (run)
  (show-state "Weather: Temp is freezing (20F)")
  (trigger weather-station.precipitation 'rain)
  (run)
  (show-state "Weather: It is raining"))

(define-cpnet-system WeatherSystemDef
  (weather-station))
