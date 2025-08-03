(define-module (examples smarthome weather detail detail-weather)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:export (weather-station))

(define-category weather-station
  (cells (Cell outside_temp 78 max-merge-fn) ;; in Fahrenheit
         (Cell precipitation 'none append-merge-fn))) ;; none, rain, snow
