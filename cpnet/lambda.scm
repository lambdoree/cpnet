(define-module (cpnet lambda)
  #:use-module (cpnet core)
  #:use-module (srfi srfi-1)
  #:export (p-const p-gate
		    p-cons p-car p-cdr
		    p-is-zero p-subtract-1 p-multiply
		    p-equal?
		    p-apply
		    make-fixpoint-net
		    ))

(define (p-const id trigger-cell output-cell const-value)
  (make-propagator id trigger-cell output-cell
    (lambda (_val src-cell)
      (cons const-value
            (list (make-effect 'set-value (cons src-cell #f)))))))

(define (p-gate id control-cell true-cell false-cell output-cell)
  (let ((on-ctrl-logic (lambda (ctrl _)
                         (cond ((eq? ctrl 'true) (cons (cell-value true-cell) '()))
                               ((eq? ctrl 'false) (cons (cell-value false-cell) '()))
                               (else (cons #f '())))))
        (on-true-logic (lambda (v _)
                         (if (eq? (cell-value control-cell) 'true)
                             (cons v '())
                             (cons #f '()))))
        (on-false-logic (lambda (v _)
                          (if (eq? (cell-value control-cell) 'false)
                              (cons v '())
                              (cons #f '())))))
    (list
     (make-propagator (string->symbol (format #f "~a-on-ctrl" id)) control-cell output-cell on-ctrl-logic)
     (make-propagator (string->symbol (format #f "~a-on-true" id)) true-cell output-cell on-true-logic)
     (make-propagator (string->symbol (format #f "~a-on-false" id)) false-cell output-cell on-false-logic))))

(define (p-cons id head-cell tail-cell output-cell)
  (list
   (make-propagator (string->symbol (format #f "~a-h" id)) head-cell output-cell
     (lambda (h _)
       (let ((t (cell-value tail-cell)))
         (if t (cons (list 'pair h t) '()) (cons #f '())))))
   (make-propagator (string->symbol (format #f "~a-t" id)) tail-cell output-cell
     (lambda (t _)
       (let ((h (cell-value head-cell)))
         (if h (cons (list 'pair h t) '()) (cons #f '())))))))

(define (p-car id pair-cell head-cell)
  (make-propagator id pair-cell head-cell
    (lambda (p _)
      (if (and (list? p) (>= (length p) 2) (eq? (car p) 'pair))
          (cons (cadr p) '())
          (cons #f '())))))

(define (p-cdr id pair-cell tail-cell)
  (make-propagator id pair-cell tail-cell
    (lambda (p _)
      (if (and (list? p) (>= (length p) 3) (eq? (car p) 'pair))
          (cons (caddr p) '())
          (cons #f '())))))

(define (p-is-zero id input-cell output-cell)
  (make-propagator id input-cell output-cell
    (lambda (n _) (cons (if (and (number? n) (zero? n)) 'true 'false) '()))))

(define (p-subtract-1 id input-cell output-cell)
  (make-propagator id input-cell output-cell
    (lambda (n _) (cons (if (number? n) (- n 1) #f) '()))))

(define (p-multiply id cell1 cell2 output-cell)
  (let ((mul-logic (lambda (_a _b)
                     (let ((v1 (cell-value cell1)) (v2 (cell-value cell2)))
                       (if (and v1 v2) (cons (* v1 v2) '()) (cons #f '()))))))
    (list
     (make-propagator (string->symbol (format #f "~a-on-1" id)) cell1 output-cell mul-logic)
     (make-propagator (string->symbol (format #f "~a-on-2" id)) cell2 output-cell mul-logic))))

(define (p-equal? id cell1 cell2 output-cell)
  (let ((eq-logic (lambda (_a _b)
                     (let ((v1 (cell-value cell1)) (v2 (cell-value cell2)))
                       (if (and v1 v2)
                           (cons (if (equal? v1 v2) 'true 'false) '())
                           (cons #f '()))))))
    (list
     (make-propagator (string->symbol (format #f "~a-on-1" id)) cell1 output-cell eq-logic)
     (make-propagator (string->symbol (format #f "~a-on-2" id)) cell2 output-cell eq-logic))))

(define (p-apply id func-cell arg-cell result-cell)
  (make-propagator id func-cell result-cell
		   (lambda (func-rep src-cell)
		     (let ((arg-val (cell-value arg-cell)))
		       (if (and arg-val func-rep)
			   (let ((lambda-input (cadr func-rep))
				 (lambda-output (caddr func-rep)))
			     (cons #f
				   (list (make-effect 'set-value (cons src-cell #f))
					 (make-effect 'connect (cons arg-cell lambda-input))
					 (make-effect 'connect (cons lambda-output result-cell)))))
			   (cons #f (list (make-effect 'set-value (cons src-cell #f)))))))))


(define (make-fixpoint-net
         state-cells
         next-cells
         step-propagators
         p-test
         test-cell
         final-cells)
  (let* ((feedback-props
          (apply append
                 (map (lambda (st ns)
                        (p-gate (string->symbol (format #f "p-gate-fb-~a" (cell-id st)))
                                test-cell
                                st
                                ns
                                st))
                      state-cells next-cells)))
         (output-props
          (apply append
                 (map (lambda (st fin)
                        (p-gate (string->symbol (format #f "p-gate-out-~a" (cell-id fin)))
                                test-cell
                                st
                                fin
                                fin))
                      (take state-cells (length final-cells))
                      final-cells)))
         (all-cells   (append state-cells next-cells final-cells (list test-cell)))
         (all-props   (append step-propagators
                              (list p-test)
                              feedback-props
                              output-props)))
    (make-cpnet-category all-cells all-props)))
