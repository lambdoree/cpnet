(define-module (examples test-map-reduce)
  #:use-module (srfi srfi-1)
  #:use-module (cpnet dsl)
  #:use-module (cpnet core)
  #:use-module (cpnet pure)
  #:use-module (cpnet apply)
  #:use-module (cpnet system)
  #:use-module (cpnet runtime))

;; The function to be mapped over the data.
(define-category double-it-body
  (objects
   (instance X Data #f)
   (instance Y Data #f))
  (morphisms
   ((morphism double (X) -> Y)
    (lambda (vals _)
      (let ((n (car vals)))
        (if (number? n)
            (cons (* n 2) '())
            (cons *nothing* '())))))))

(register-builder (make-category-builder
                   'double-it-body
                   double-it-body
                   '((inputs (X)) (outputs (Y)))))

;; The interface for our test system.
(define-category MapReduceTestInterface
  (objects
   (instance inputs Data '(#f #f #f) 'Replace)
   (instance mapper category-builder #f 'Replace)
   (instance results Data #f 'Maybe)))

(define-category proc-io
  (objects
   (instance in Data #f 'Replace)
   (instance out Data *nothing* 'Maybe)))

;; The system that maps a function over 3 inputs in parallel.
(define-cpnet-system MapReduceTestSystem
  (MapReduceTestInterface)
  ;; Create 3 "processors"
  (add-subsystem! (current-system) (apply-gate 'proc1))
  (add-subsystem! (current-system) (apply-gate 'proc2))
  (add-subsystem! (current-system) (apply-gate 'proc3))

  ;; Create intermediate cells for inputs and outputs for each processor
  (add-subsystem! (current-system) (proc-io 'proc1-io))
  (add-subsystem! (current-system) (proc-io 'proc2-io))
  (add-subsystem! (current-system) (proc-io 'proc3-io))
  
  ;; --- Setup and Fan-out Phase ---
  (propagator fan-out
              (get-cell 'MapReduceTestInterface 'inputs)
              -> (list (get-cell 'proc1-io 'proc-io 'in)
                       (get-cell 'proc2-io 'proc-io 'in)
                       (get-cell 'proc3-io 'proc-io 'in))
              (lambda (vals _)
                (let ((items (car vals)))
                  (if (and (list? items) (= (length items) 3))
                      (cons items '())
                      (cons (list *nothing* *nothing* *nothing*) '())))))

  ;; Wire the mapper to all processors and set up their args/results cells.
  (propagator setup-proc1
              (list (get-cell 'MapReduceTestInterface 'mapper) (get-cell 'proc1-io 'proc-io 'in) (get-cell 'proc1-io 'proc-io 'out))
              -> (list (get-cell 'proc1 'apply-interface 'code) (get-cell 'proc1 'apply-interface 'args) (get-cell 'proc1 'apply-interface 'results))
              (lambda (vals srcs) (cons (list (car vals) (list (cadr srcs)) (list (caddr srcs))) '())))
  (propagator setup-proc2
              (list (get-cell 'MapReduceTestInterface 'mapper) (get-cell 'proc2-io 'proc-io 'in) (get-cell 'proc2-io 'proc-io 'out))
              -> (list (get-cell 'proc2 'apply-interface 'code) (get-cell 'proc2 'apply-interface 'args) (get-cell 'proc2 'apply-interface 'results))
              (lambda (vals srcs) (cons (list (car vals) (list (cadr srcs)) (list (caddr srcs))) '())))
  (propagator setup-proc3
              (list (get-cell 'MapReduceTestInterface 'mapper) (get-cell 'proc3-io 'proc-io 'in) (get-cell 'proc3-io 'proc-io 'out))
              -> (list (get-cell 'proc3 'apply-interface 'code) (get-cell 'proc3 'apply-interface 'args) (get-cell 'proc3 'apply-interface 'results))
              (lambda (vals srcs) (cons (list (car vals) (list (cadr srcs)) (list (caddr srcs))) '())))
              
  ;; --- Fan-in Phase (Join) ---
  (propagator join
              (list (get-cell 'proc1-io 'proc-io 'out)
                    (get-cell 'proc2-io 'proc-io 'out)
                    (get-cell 'proc3-io 'proc-io 'out))
              -> (get-cell 'MapReduceTestInterface 'results)
              (lambda (vals _)
                (if (every (lambda (v) (not (eq? v *nothing*))) vals)
                    (cons vals '())
                    (cons *nothing* '())))))

;; --- Scenario ---
(parameterize ((current-system (MapReduceTestSystem)))
  (show-state "--- Test Map-Reduce: Before ---")
  (trigger (get-cell 'MapReduceTestInterface 'mapper) (get-builder 'double-it-body))
  (trigger (get-cell 'MapReduceTestInterface 'inputs) '(10 20 30))
  (run)
  (show-state "--- Test Map-Reduce: After ---")
  (visualize "test-map-reduce.dot"))
