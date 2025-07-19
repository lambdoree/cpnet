(use-modules (srfi srfi-1)
             (cpnet category)
             (cpnet core)
             (cpnet functor)
             (cpnet runtime))

(define A1 (make-cell 'A1 #f))
(define B1 (make-cell 'B1 #f))
(define C1 (make-cell 'C1 #f))
(define Net1
  (make-cpnet-category
   (list A1 B1 C1)
   (make-binary-constraint A1 B1 C1 + - - "adder")))

(runtime-show-state Net1 "Initial Net1 (A1+B1=C1)")

(define A2 (make-cell 'A2 #f))
(define B2 (make-cell 'B2 #f))
(define C2 (make-cell 'C2 #f))
(define Net2
  (make-cpnet-category
   (list A2 B2 C2)
   '()))

(runtime-show-state Net2 "Initial Net2 (empty)")

(display "\n--- Defining a functor F: Net1 -> Net2 ---\n")
(define cell-map `((,A1 . ,A2) (,B1 . ,B2) (,C1 . ,C2)))
(define F (make-cpnet-functor Net1 Net2 cell-map))
(display "Functor F: Net1 -> Net2 created.\n")
(functor-validate F)
(display "success\n")


(display "\n--- Applying functor to connect networks ---\n")
(display "Mapping propagators from Net1 to Net2...\n")
(let ((F1 (functor-morphism-map F)))
  (for-each
   (lambda (p)
     (category-add-morphism Net2 (F1 p)))
   (category-morphisms Net1)))

(category-validate Net2)
(runtime-show-state Net2 "Net2 after mapping from Net1")

(display "\n--- Testing the connection between networks ---\n")
(display "The propagators mapped to Net2 close over cells in Net1.\n")
(display "For example, the new propagator p-A2->C2 will use the value of B1.\n")

(cell-set-value! A1 #f) (cell-set-value! B1 #f) (cell-set-value! C1 #f)
(cell-set-value! A2 #f) (cell-set-value! B2 #f) (cell-set-value! C2 #f)
(runtime-show-state Net1 "Reset Net1")
(runtime-show-state Net2 "Reset Net2")

(display "\nSetting A2=100 and B1=20\n")
(cell-set-value! A2 100)
(cell-set-value! B1 20)
(runtime-show-state Net1 "Net1 state before settling")
(runtime-show-state Net2 "Net2 state before settling")

(runtime-execute-effects (runtime-settle! Net2))
(runtime-show-state Net2 "Net2 after settling (C2 should be 120 from A2+B1)")
(if (and (number? (cell-value C2)) (= (cell-value C2) 120)) (display "success\n"))


(display "\n\n========================================================\n")
(display "         Testing Functor Composition\n")
(display "========================================================\n\n")

(define A3 (make-cell 'A3 #f))
(define B3 (make-cell 'B3 #f))
(define C3 (make-cell 'C3 #f))
(define Net3
  (make-cpnet-category
   (list A3 B3 C3)
   '()))
(runtime-show-state Net3 "Initial Net3 (empty)")

(display "\n--- Defining a functor G: Net2 -> Net3 ---\n")
(define cell-map-G `((,A2 . ,A3) (,B2 . ,B3) (,C2 . ,C3)))
(define G (make-cpnet-functor Net2 Net3 cell-map-G))
(display "Functor G: Net2 -> Net3 created.\n")
(functor-validate G)
(display "success\n")

(display "\n--- Composing H = G o F : Net1 -> Net3 ---\n")
(define H (compose-functor G F))
(display "Functor H created.\n")
(functor-validate H)
(display "success\n")

(display "\n--- Applying composed functor H to connect Net1 and Net3 ---\n")
(let ((H1 (functor-morphism-map H)))
  (for-each
   (lambda (p)
     (category-add-morphism Net3 (H1 p)))
   (category-morphisms Net1)))
(runtime-show-state Net3 "Net3 after mapping from Net1 via H")

(display "\n--- Testing the connection from Net1 to Net3 ---\n")
(display "Setting A3=50 and B1=25\n")
(cell-set-value! A1 #f) (cell-set-value! B1 #f) (cell-set-value! C1 #f)
(cell-set-value! A2 #f) (cell-set-value! B2 #f) (cell-set-value! C2 #f)
(cell-set-value! A3 #f) (cell-set-value! B3 #f) (cell-set-value! C3 #f)

(cell-set-value! A3 50)
(cell-set-value! B1 25)
(runtime-show-state Net1 "Net1 state")
(runtime-show-state Net3 "Net3 state before settling")
(runtime-execute-effects (runtime-settle! Net3))
(runtime-show-state Net3 "Net3 after settling (C3 should be 75 from A3+B1)")
(if (and (number? (cell-value C3)) (= (cell-value C3) 75)) (display "success\n"))
