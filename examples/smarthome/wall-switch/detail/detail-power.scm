(define-module (examples smarthome wall-switch detail detail-power)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:export (power-meter power-aggregator))

(define (update-total-power cat-name)
  (let ((p1 (get-cell-value cat-name 'power_in_1))
        (p2 (get-cell-value cat-name 'power_in_2))
        (p3 (get-cell-value cat-name 'power_in_3)))
    (cons (+ p1 p2 p3) '())))

;; Sub-System: Power Meter
(define-category power-meter
  (cells (Cell power_usage 0))) ;; in Watts

;; Sub-System: Power Aggregator
(define-category power-aggregator
  (cells (Cell power_in_1 0)
         (Cell power_in_2 0)
         (Cell power_in_3 0)
         (Cell total_power 0)
         (Cell peak_power 0))
  (propagators
   ((prop p-update-total-from-1 power_in_1 -> total_power)
    (lambda (v s) (update-total-power 'power-aggregator)))
   ((prop p-update-total-from-2 power_in_2 -> total_power)
    (lambda (v s) (update-total-power 'power-aggregator)))
   ((prop p-update-total-from-3 power_in_3 -> total_power)
    (lambda (v s) (update-total-power 'power-aggregator)))
   ((prop p-update-peak total_power -> peak_power)
    (lambda (current-total src-cell)
      (let* ((parts (string-split (symbol->string (cell-id src-cell)) #\.))
             (cat-name (string->symbol (if (= (length parts) 3) (list-ref parts 1) (car parts))))
             (current-peak (get-cell-value cat-name 'peak_power)))
        (if (> current-total current-peak)
            (cons current-total '())
            (cons current-peak '())))))))
