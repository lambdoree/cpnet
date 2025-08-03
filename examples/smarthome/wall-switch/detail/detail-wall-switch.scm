(define-module (examples smarthome wall-switch detail detail-wall-switch)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:export (wall-switch light-bulb))

;; Sub-System: Wall Switch
(define-category wall-switch
  (cells (Cell switch_state 'auto) ;; auto, on, off
         (Cell button_press #f))
  (propagators
   ((prop toggle_switch button_press -> switch_state)
    (lambda (v src-cell)
      (if v
          (let* ((parts (string-split (symbol->string (cell-id src-cell)) #\.))
                 (cat-name (string->symbol (if (= (length parts) 3) (list-ref parts 1) (car parts))))
                 (current-state (get-cell-value cat-name 'switch_state)))
            (let ((new-state (cond ((eq? current-state 'auto) 'on)
                                   ((eq? current-state 'on) 'off)
                                   ((eq? current-state 'off) 'on)
                                   (else current-state))))
              (cons new-state
                    (list (set-cell-effect cat-name 'button_press #f)))))
          (cons #f '()))))))

;; Sub-System: Light Bulb
(define-category light-bulb
  (cells (Cell state 'off))) ;; on, off

