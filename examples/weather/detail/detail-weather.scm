(define-module (examples weather detail detail-weather)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:export (weather-station))

(define-category weather-station
  (cells (Cell outside_temp 78) ;; in Fahrenheit
         (Cell precipitation 'none))) ;; none, rain, snow
