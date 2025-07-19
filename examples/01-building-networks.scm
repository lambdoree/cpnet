(use-modules (srfi srfi-1)
             (cpnet category)
             (cpnet core)
             (cpnet runtime))

(define (approx-equal? a b tolerance)
  (and (number? a) (number? b) (< (abs (- a b)) tolerance)))

(define (safe-div x y)
  (if (zero? y)
      0
      (/ x y)))

(define A (make-cell 'A #f))
(define B (make-cell 'B #f))
(define C (make-cell 'C #f))

(define p (make-propagator 'p A B (lambda (x) (cons (+ x 1) '()))))
(define q (make-propagator 'q B C (lambda (y) (cons (* y 2) '()))))

(define Net1
  (make-cpnet-category
   (list A B C)
   (list p q)))

(category-validate Net1)
(runtime-show-state Net1 "Initial Net1 (all nil)")

(cell-set-value! A 3)
(runtime-show-state Net1 "Set A=3")
(runtime-execute-effects (runtime-settle! Net1))
(runtime-show-state Net1 "After settling")
(if (and (number? (cell-value C)) (= (cell-value C) 8)) (display "success\n"))

(define X (make-cell 'X #f))
(define Y (make-cell 'Y #f))
(define Z (make-cell 'Z #f))

(define NetABC
  (make-cpnet-category
   (list X Y Z)
   (make-binary-constraint X Y Z + - - "adder")))

(runtime-show-state NetABC "Initial NetABC (all nil)")

(cell-set-value! X 5)
(cell-set-value! Y 2)
(runtime-show-state NetABC "Set X=5, Y=2")
(runtime-execute-effects (runtime-settle! NetABC))
(runtime-show-state NetABC "After settling (Z should be 7)")
(if (and (number? (cell-value Z)) (= (cell-value Z) 7)) (display "success\n"))

(cell-set-value! X #f) (cell-set-value! Y #f) (cell-set-value! Z #f)

(cell-set-value! X 3)
(cell-set-value! Z 10)
(runtime-show-state NetABC "Set X=3, Z=10")
(runtime-execute-effects (runtime-settle! NetABC))
(runtime-show-state NetABC "After settling (Y should be 7)")
(if (and (number? (cell-value Y)) (= (cell-value Y) 7)) (display "success\n"))

(define M1 (make-cell 'M1 #f))
(define M2 (make-cell 'M2 #f))
(define M3 (make-cell 'M3 #f))

(define NetMul
  (make-cpnet-category
   (list M1 M2 M3)
   (make-binary-constraint M1 M2 M3 * safe-div safe-div "multiplier")))

(runtime-show-state NetMul "Initial Multiplier Net (all nil)")

(cell-set-value! M1 3)
(cell-set-value! M2 5)
(runtime-show-state NetMul "Set M1=3, M2=5")
(runtime-execute-effects (runtime-settle! NetMul))
(runtime-show-state NetMul "After settling (M3 should be 15)")
(if (and (number? (cell-value M3)) (= (cell-value M3) 15)) (display "success\n"))

(cell-set-value! M1 #f) (cell-set-value! M2 #f) (cell-set-value! M3 #f)

(cell-set-value! M2 4)
(cell-set-value! M3 20)
(runtime-show-state NetMul "Set M2=4, M3=20")
(runtime-execute-effects (runtime-settle! NetMul))
(runtime-show-state NetMul "After settling (M1 should be 5)")
(if (and (number? (cell-value M1)) (= (cell-value M1) 5)) (display "success\n"))

(define C* (make-cell 'Celsius    #f))
(define F* (make-cell 'Fahrenheit #f))
(define K* (make-cell 'Kelvin     #f))

(define props
  (append
   (make-unary-constraint C* F*
			  (lambda (c) (+ (* c 9/5) 32))
			  (lambda (f) (* (- f 32) 5/9))
			  "celsius-fahrenheit")
   (make-unary-constraint C* K*
			  (lambda (c) (+ c 273.15))
			  (lambda (k) (- k 273.15))
			  "celsius-kelvin")
   (make-unary-constraint F* K*
			  (lambda (f) (+ (* (- f 32) 5/9) 273.15))
			  (lambda (k) (+ (* (- k 273.15) 9/5) 32))
			  "fahrenheit-kelvin")))

(define NetTemp (make-cpnet-category (list C* F* K*) props))

(runtime-show-state NetTemp "Initial Temperature Net (all nil)")

(cell-set-value! C* 100)
(runtime-show-state NetTemp "Set Celsius to 100")
(runtime-execute-effects (runtime-settle! NetTemp))
(runtime-show-state NetTemp "After settling (F=212, K=373.15)")
(if (and (approx-equal? (cell-value F*) 212 0.001)
         (approx-equal? (cell-value K*) 373.15 0.001))
    (display "success\n"))

(cell-set-value! C* #f) (cell-set-value! F* #f) (cell-set-value! K* #f)

(cell-set-value! F* 32)
(runtime-show-state NetTemp "Set Fahrenheit to 32")
(runtime-execute-effects (runtime-settle! NetTemp))
(runtime-show-state NetTemp "After settling (C=0, K=273.15)")
(if (and (approx-equal? (cell-value C*) 0 0.001)
         (approx-equal? (cell-value K*) 273.15 0.001))
    (display "success\n"))

(cell-set-value! C* #f) (cell-set-value! F* #f) (cell-set-value! K* #f)

(cell-set-value! K* 0)
(runtime-show-state NetTemp "Set Kelvin to 0")
(runtime-execute-effects (runtime-settle! NetTemp))
(runtime-show-state NetTemp "After settling (C=-273.15, F=-459.67)")
(if (and (approx-equal? (cell-value C*) -273.15 0.001)
         (approx-equal? (cell-value F*) -459.67 0.001))
    (display "success\n"))

(define CS1 (make-cell 'ConflictSource1 #f))
(define CS2 (make-cell 'ConflictSource2 #f))

(define (average-merge cell new-vals)
  (let* ((avg (/ (apply + new-vals) (length new-vals)))
         (effect (make-effect 'display
                              (format #f "CONFLICT RESOLVED on ~a: averaged ~a to ~a\n"
                                      (cell-id cell) new-vals avg))))
    (cons avg (list effect))))

(define CT (make-cell 'ConflictTarget #f average-merge))

(define p-add (make-propagator 'p-add CS1 CT (lambda (v) (cons (+ v 10) '()))))
(define p-mul (make-propagator 'p-mul CS2 CT (lambda (v) (cons (* v 2) '()))))

(define NetConflict
  (make-cpnet-category
   (list CS1 CS2 CT)
   (list p-add p-mul)))

(runtime-show-state NetConflict "Initial Conflict Net")

(cell-set-value! CS1 5)
(cell-set-value! CS2 20)
(runtime-show-state NetConflict "Set CS1=5, CS2=20")
(runtime-execute-effects (runtime-settle! NetConflict))
(runtime-show-state NetConflict "After settling (result is deterministic: 27.5)")
(if (approx-equal? (cell-value CT) 27.5 0.001) (display "success\n"))

(define (ttt-display-board cells)
  (let ((vals (map (lambda (c) (let ((v (cell-value c))) (if (eq? v #f) "." (symbol->string v)))) cells)))
    (string-append
     "\n"
     (format #f " ~a | ~a | ~a \n" (list-ref vals 0) (list-ref vals 1) (list-ref vals 2))
     "---+---+---\n"
     (format #f " ~a | ~a | ~a \n" (list-ref vals 3) (list-ref vals 4) (list-ref vals 5))
     "---+---+---\n"
     (format #f " ~a | ~a | ~a \n" (list-ref vals 6) (list-ref vals 7) (list-ref vals 8)))))

(define (game-status-merge-fn cell new-vals)
  (let ((win (find (lambda (v) (memq v '(X-wins O-wins))) new-vals)))
    (if win
        (cons win (list (make-effect 'display (format #f "\n*** GAME OVER: ~a wins! ***\n" (if (eq? win 'X-wins) 'X 'O)))))
        (if (memq 'draw new-vals)
            (cons 'draw (list (make-effect 'display "\n*** GAME OVER: Draw! ***\n")))
            (cons (car new-vals) '())))))

(define (make-line-win-propagators c1 c2 c3 game-status-cell)
  (let ((checker-fn
         (lambda (_)
           (let ((v1 (cell-value c1)) (v2 (cell-value c2)) (v3 (cell-value c3)))
             (if (and v1 (eq? v1 v2) (eq? v1 v3))
                 (cons (if (eq? v1 'X) 'X-wins 'O-wins) '())
                 (cons #f '()))))))
    (list
     (make-propagator (string->symbol (format #f "p-~a-on-~a~a~a" (cell-id c1) (cell-id c1) (cell-id c2) (cell-id c3))) c1 game-status-cell checker-fn)
     (make-propagator (string->symbol (format #f "p-~a-on-~a~a~a" (cell-id c2) (cell-id c1) (cell-id c2) (cell-id c3))) c2 game-status-cell checker-fn)
     (make-propagator (string->symbol (format #f "p-~a-on-~a~a~a" (cell-id c3) (cell-id c1) (cell-id c2) (cell-id c3))) c3 game-status-cell checker-fn))))

(define (make-draw-checker all-board-cells game-status-cell)
  (map (lambda (board-cell)
         (make-propagator
          (string->symbol (format #f "draw-check-on-~a" (cell-id board-cell)))
          board-cell
          game-status-cell
          (lambda (_)
            (let ((board-full? (every cell-value all-board-cells)))
              (if board-full? (cons 'draw '()) (cons #f '()))))))
       all-board-cells))

(define ttt-cells
  (list (make-cell 'C11 #f) (make-cell 'C12 #f) (make-cell 'C13 #f)
        (make-cell 'C21 #f) (make-cell 'C22 #f) (make-cell 'C23 #f)
        (make-cell 'C31 #f) (make-cell 'C32 #f) (make-cell 'C33 #f)))

(define game-status (make-cell 'game-status 'playing game-status-merge-fn))

(define winning-lines
  (let ((c (lambda (r k) (list-ref ttt-cells (+ (* (- r 1) 3) (- k 1))))))
    (list (list (c 1 1) (c 1 2) (c 1 3)) (list (c 2 1) (c 2 2) (c 2 3)) (list (c 3 1) (c 3 2) (c 3 3))
          (list (c 1 1) (c 2 1) (c 3 1)) (list (c 1 2) (c 2 2) (c 3 2)) (list (c 1 3) (c 2 3) (c 3 3))
          (list (c 1 1) (c 2 2) (c 3 3)) (list (c 1 3) (c 2 2) (c 3 1)))))

(define ttt-propagators
  (append
   (apply append (map (lambda (line)
                        (apply make-line-win-propagators (append line (list game-status))))
                      winning-lines))
   (make-draw-checker ttt-cells game-status)))

(define NetTTT (make-cpnet-category (cons game-status ttt-cells) ttt-propagators))

(define (play-move net cell player)
  (if (not (eq? (cell-value game-status) 'playing))
      (display "\nGame is already over.\n")
      (begin
        (display (format #f "\n--- Player ~a moves to ~a ---\n" player (cell-id cell)))
        (cell-set-value! cell player)
        (runtime-execute-effects (runtime-settle! net))
        (display (ttt-display-board ttt-cells))
        (display (format #f "Game status: ~a\n" (cell-value game-status))))))

(display "\n\n--- TIC-TAC-TOE GAME SIMULATION ---\n")
(runtime-show-state NetTTT "Initial Tic-Tac-Toe Net")
(display (ttt-display-board ttt-cells))

(play-move NetTTT (list-ref ttt-cells 4) 'X)
(play-move NetTTT (list-ref ttt-cells 0) 'O)
(play-move NetTTT (list-ref ttt-cells 2) 'X)
(play-move NetTTT (list-ref ttt-cells 6) 'O)
(play-move NetTTT (list-ref ttt-cells 8) 'X)
(play-move NetTTT (list-ref ttt-cells 1) 'O)
(play-move NetTTT (list-ref ttt-cells 5) 'X)
(if (eq? (cell-value game-status) 'X-wins) (display "success\n"))
(play-move NetTTT (list-ref ttt-cells 3) 'O)
