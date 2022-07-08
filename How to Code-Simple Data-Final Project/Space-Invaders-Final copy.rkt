;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Space-Invaders-Final copy|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; An implementation of the famous Space Invaders game
;; to run: 

(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =============
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)      

(define INVADE-RATE 98)   

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")             
              -5 6
              (ellipse 20 10 "solid"   "blue")))           

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       
                       (ellipse 30 10 "solid" "green"))     
              5 -14
              (above (rectangle 5 10 "solid" "black")       
                     (rectangle 20 10 "solid" "black"))))  

(define MISSILE (ellipse 5 15 "solid" "red"))

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))

(define MISSILE-WIDTH/2 (/ (image-width MISSILE) 2))

(define START-BUTTON (overlay (text "Start" 22 "black")
                              (rectangle 100 25 "outline" "blue")))

(define HEIGHT-RATIO 3)

(define WIDTH-RATIO 2)

;; =================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader)
       (invader-y invader)
       (invader-dx invader)))


;; List Of Invader is one of:
;; - empty
;; - (make-invader Natural Natural Integer) ListOfInvader

(define LOI1 (list (make-invader 3 3 INVADER-X-SPEED)))

#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]
        [else (...
               (fn-for-invader (first loinvader))
               (fn-for-loinvader (rest loinvader)))]))

 
(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; - (make-missile Natural Natural) ListOfMissile

(define LOM1 (list (make-missile 4 90)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (..)]
        [else (...
               (fn-for-missile (first lom))
               (fn-for-lom (rest lom)))]))
                      

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; ================
;; Functions

;; creates the welcome page and
;; calls real-main upon mouse click
(define (main inte)
  (big-bang inte
     (to-draw hello-page)
     (on-mouse start-game)))


(define (real-main game)
  (big-bang game
    (on-tick    move-objects)     
    (to-draw    render)           
    (on-key     shoot-and-move)  
    (stop-when  lost?)))          


;; Game -> Game
;; moves invaders and dispensed
;; missile (after filtering)

;; Tank movement and border protection 
(check-expect (move-objects
               (make-game empty empty (make-tank TANKS-LEFT-EDGE 1)))
              (make-game empty empty (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))
(check-expect (move-objects (make-game empty empty (make-tank 100 (- 1))))
              (make-game empty empty (make-tank (- 100 TANK-SPEED) (- 1))))
(check-expect (move-objects (make-game empty empty (make-tank TANKS-RIGHT-EDGE 1)))
              (make-game empty empty (make-tank (+ TANKS-RIGHT-EDGE 0) 1)))
(check-expect (move-objects (make-game empty empty (make-tank TANKS-LEFT-EDGE (- 1))))
              (make-game empty empty (make-tank (- TANKS-LEFT-EDGE 0) -1)))
 
;; Invader movement and border protection
(check-expect (move-objects (make-game (list (make-invader 100 100 INVADER-X-SPEED))
                                       empty (make-tank TANKS-LEFT-EDGE 1)))     
              (make-game (list (make-invader (+ 100 INVADER-X-SPEED)
                                             (+ 100 INVADER-X-SPEED) INVADER-X-SPEED))
                         empty (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))

(check-expect (move-objects (make-game (list (make-invader 100 100 (- INVADER-X-SPEED)))
                                       empty (make-tank TANKS-LEFT-EDGE 1)))
              (make-game (list (make-invader (- 100 INVADER-X-SPEED)
                                             (+ 100 INVADER-Y-SPEED) (- INVADER-X-SPEED)))
                         empty (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))

(check-expect (move-objects (make-game (list (make-invader WIDTH 100 INVADER-X-SPEED))
                                       empty (make-tank TANKS-LEFT-EDGE 1)))
              (make-game (list (make-invader (- WIDTH INVADER-X-SPEED)
                                             (+ 100 INVADER-X-SPEED) (- INVADER-X-SPEED)))
                         empty (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))
(check-expect (move-objects (make-game (list (make-invader 0 100 (- INVADER-X-SPEED)))
                                       empty (make-tank TANKS-LEFT-EDGE 1)))
              (make-game (list (make-invader (+ 0 INVADER-X-SPEED)  (+ 100 INVADER-X-SPEED)
                                             INVADER-X-SPEED))
                         empty (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))
(check-expect (move-objects (make-game (list (make-invader WIDTH 100 INVADER-X-SPEED)
                                             (make-invader 0 0 (- INVADER-X-SPEED)))
                                       empty (make-tank TANKS-LEFT-EDGE 1)))
              (make-game (list (make-invader (- WIDTH INVADER-X-SPEED)  (+ 100 INVADER-X-SPEED)
                                             (- INVADER-X-SPEED))
                               (make-invader (+ 0 INVADER-X-SPEED)

                                             (+ 0 INVADER-X-SPEED)  INVADER-X-SPEED))
                         empty (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))

;; Missile movement and filtering 
(check-expect (move-objects (make-game empty (list (make-missile 150 140)) (make-tank TANKS-LEFT-EDGE 1)))      
              (make-game empty (list (make-missile 150 (- 140 MISSILE-SPEED)))
                         (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))
(check-expect (move-objects (make-game empty (list (make-missile 150 140)
                                                   (make-missile 20 20)) (make-tank TANKS-LEFT-EDGE 1)))      
              (make-game empty (list (make-missile 150 (- 140 MISSILE-SPEED))
                                     (make-missile 20 (- 20 MISSILE-SPEED)))
                         (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))
(check-expect (move-objects (make-game empty (list (make-missile 150 0) (make-missile 20 20))
                                       (make-tank TANKS-LEFT-EDGE 1)))      
              (make-game empty (list (make-missile 20 (- 20 MISSILE-SPEED)))
                         (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))
(check-expect (move-objects (make-game empty (list (make-missile 150 140) (make-missile 20 0))
                                       (make-tank TANKS-LEFT-EDGE 1)))      
              (make-game empty (list (make-missile 150 (- 140 MISSILE-SPEED)))
                         (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))

;; Missle and invader collision
(check-expect (move-objects
               (make-game (list (make-invader (- 40 INVADER-X-SPEED)
                                              (- 40 INVADER-Y-SPEED)
                                              INVADER-X-SPEED))
                          (list (make-missile 40 (+ 40 MISSILE-SPEED)))
                          (make-tank TANKS-LEFT-EDGE 1)))  
              (make-game empty empty (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1)))


(define (move-objects s)
  (make-game      
   (spawn-invaders
    (check-crashes-invaders
     (move-invaders (game-invaders s))
     (move-missiles (game-missiles s))))
        
   (check-crashes-missiles
    (move-invaders (game-invaders s))
    (move-missiles (game-missiles s)))
           
   (move-tank (game-tank s))))


;; ListOfInvaders -> ListOfInvaders
;; moves each invader based on INVADER-X-SPEED and INVADER-Y-SPEED while maintaining
;; the invaders within the borders of the game
(check-expect (move-invaders (list (make-invader 100 100 INVADER-X-SPEED)))  
              (list (make-invader (+ 100 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) INVADER-X-SPEED)))                                            
(check-expect (move-invaders (list (make-invader 100 100 (- INVADER-X-SPEED))))                                     
              (list (make-invader (- 100 INVADER-X-SPEED)
                                  (+ 100 INVADER-Y-SPEED) (- INVADER-X-SPEED))))
(check-expect (move-invaders (list (make-invader WIDTH 100 INVADER-X-SPEED)))                                     
              (list (make-invader (- WIDTH INVADER-X-SPEED)
                                  (+ 100 INVADER-X-SPEED) (- INVADER-X-SPEED))))                         
(check-expect (move-invaders (list (make-invader 0 100 (- INVADER-X-SPEED))))  
              (list (make-invader (+ 0 INVADER-X-SPEED)  (+ 100 INVADER-X-SPEED)
                                  INVADER-X-SPEED)))                         
(check-expect (move-invaders (list (make-invader WIDTH 100 INVADER-X-SPEED)
                                   (make-invader 0 0 (- INVADER-X-SPEED))))                                     
              (list (make-invader (- WIDTH INVADER-X-SPEED)  (+ 100 INVADER-Y-SPEED)
                                  (- INVADER-X-SPEED))
                    (make-invader (+ 0 INVADER-X-SPEED)
                                  (+ 0 INVADER-Y-SPEED)  INVADER-X-SPEED)))
                              

(define (move-invaders loinvader)
  (cond [(empty? loinvader) empty]
        [else  
         (cons (move-invader (first loinvader))
               (move-invaders (rest loinvader)))]))


;; ListOfMissiles -> ListOfMissiles
;; updates missiles' locations and filters
;; the off-screen missiles
(define (move-missiles lom)
  (filter-missiles (tick-missiles lom)))    


;; Tank -> Tank
;; updates the tanks location while
;; maintaining it with borders
(define (move-tank t)
  (cond [(tank-off-screen? t) t]
        [else (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))


;; Invader -> Invader
;; updates an invader's position while maintaing
;; it within the walls of the game
(check-expect (move-invader (make-invader 5 5 INVADER-X-SPEED))
              (make-invader (+ 5 INVADER-X-SPEED) (+ 5 INVADER-Y-SPEED) INVADER-X-SPEED))
(check-expect (move-invader (make-invader 100 5 (- INVADER-X-SPEED)))
              (make-invader (- 100 INVADER-X-SPEED) (+ 5 INVADER-Y-SPEED) (- INVADER-X-SPEED)))
(check-expect (move-invader (make-invader (- WIDTH 0.5) 5 INVADER-X-SPEED))
              (make-invader (- (- WIDTH 0.5) INVADER-X-SPEED) (+ 5 INVADER-Y-SPEED) (- INVADER-X-SPEED)))
(check-expect (move-invader (make-invader 0 5 (- INVADER-X-SPEED)))
              (make-invader (+ 0 INVADER-X-SPEED) (+ 5 INVADER-Y-SPEED) INVADER-X-SPEED)) 


(define (move-invader invader)
  (cond [(not
          (or
           (moving-invader-off-screen-left? invader)
           (moving-invader-off-screen-right? invader)))
         (make-invader (+ (invader-x invader) (invader-dx invader))
                       (+ (invader-y invader) INVADER-Y-SPEED)
                       (invader-dx invader))]
        
        [(moving-invader-off-screen-right? invader)
         (change-invader-direction invader)]
        
        [(moving-invader-off-screen-left? invader)
         (change-invader-direction invader)]))       


;; ListOfInvaders -> ListOfInvaders
;; appends the current list with new invaders with
;; initial height = 0, according to INVADE-RATE

(define (spawn-invaders loi)
  (if (> (random 100) INVADE-RATE)
      (cons (random-invader loi) loi)
      loi))


;; -> Invader
;; produces an invader with a random x
;; coordinate
(define (random-invader loi) (make-invader (* (/ (random 100) 100) WIDTH)
                                           0 INVADER-X-SPEED))                                              


;; ListOfMissiles -> ListOfMissiles
;; updates the missiles' coordinates
(check-expect (move-missiles (list (make-missile 150 140)))   
              (list (make-missile 150 (- 140 MISSILE-SPEED))))
                      
(check-expect (move-missiles (list (make-missile 150 140)
                                   (make-missile 20 20)))     
              (list (make-missile 150 (- 140 MISSILE-SPEED))
                    (make-missile 20 (- 20 MISSILE-SPEED))))
                        
(check-expect (move-missiles (list (make-missile 150 0))) empty)
                        
(check-expect (move-missiles (list (make-missile 150 140) (make-missile 20 0)))                                            
              (list (make-missile 150 (- 140 MISSILE-SPEED))))


;; Invader -> Invader
;; given an invader that is about to go off-screen
;; produces the new position of the invader
(define (change-invader-direction invader)
  (make-invader (- (invader-x invader) (invader-dx invader))
                (+ (invader-y invader) INVADER-Y-SPEED)
                (- (invader-dx invader))))


;; Invader -> Boolean
;; produces true if invader has gone off-screen
;; to the right
(define (moving-invader-off-screen-right? invader)
  (< WIDTH (+ (invader-x invader) (invader-dx invader))))


;; Invader -> Boolean
;; produces true if invader has gone off-screen
;; to the left
(define (moving-invader-off-screen-left? invader)
  (> 0 (+ (invader-x invader) (invader-dx invader))))


;; ListOfMissiles -> ListOfMissiles
;; increases missiles' y value by MISSILE-SPEED
(check-expect (tick-missiles (list (make-missile 150 140)))  (list (make-missile 150 (- 140 MISSILE-SPEED))))                        
(check-expect (tick-missiles (list (make-missile 150 140) (make-missile 20 20)))
              (list (make-missile 150 (- 140 MISSILE-SPEED))  (make-missile 20 (- 20 MISSILE-SPEED))))
(check-expect (tick-missiles  (list (make-missile 150 140)))
              (list (make-missile 150 (- 140 MISSILE-SPEED))))             
              
                        
(define (tick-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (move-missile (first lom)) (tick-missiles (rest lom)))]))
 

;; ListOfMissiles -> ListOfMissiles
;; eliminates off-screen missiles from the list
(check-expect (filter-missiles empty) empty) 
(check-expect (filter-missiles (list (make-missile 3 4)))
              (list (make-missile 3 4)))
(check-expect (filter-missiles (list (make-missile 3 0)))
              (list (make-missile 3 0)))
(check-expect (filter-missiles (list (make-missile 3 4) (make-missile 300 300)))
              (list (make-missile 3 4) (make-missile 300 300)))
(check-expect (filter-missiles (list (make-missile 3 4) (make-missile 300 (- 3))))
              (list (make-missile 3 4)))
(check-expect (filter-missiles (list (make-missile 3 (- 4)) (make-missile 300 300)))
              (list (make-missile 300 300)))


(define (filter-missiles lom)
  (cond [(empty? lom) empty]
        [else (if (out-of-border? (first lom))
                  (filter-missiles (rest lom))
                  (cons (first lom) (filter-missiles (rest lom))))]))                      


;; Missile -> Missile
;; updates Missile's y value
(check-expect (move-missile (make-missile 100 5)) (make-missile 100 (- 5 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 400 400)) (make-missile 400 (- 400 MISSILE-SPEED)))


(define (move-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))


;; Missile -> Boolean
;; returns true if the missile has gone outside
;; the borders of the game
(check-expect (out-of-border? (make-missile 4 100)) false)
(check-expect (out-of-border? (make-missile 4 (- 3))) true)
(check-expect (out-of-border? (make-missile 4 0)) false)


(define (out-of-border? m)
  (< (missile-y m) 0))


;; Tank -> Tank
;; updates the tank's position and
;; maintains its position within the borders
(check-expect (move-tank (make-tank 30 1)) (make-tank (+ 30 TANK-SPEED) 1))
(check-expect (move-tank (make-tank 30 (- 1))) (make-tank (- 30 TANK-SPEED) (- 1)))
(check-expect (move-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (move-tank (make-tank 0 (- 1))) (make-tank 0 (- 1)))
(check-expect (move-tank (make-tank TANKS-RIGHT-EDGE (- 1))) (make-tank (- TANKS-RIGHT-EDGE TANK-SPEED) (- 1)))
(check-expect (move-tank (make-tank TANKS-LEFT-EDGE 1)) (make-tank (+ TANKS-LEFT-EDGE TANK-SPEED) 1))


;; Tank -> Boolean
;; produces true if the tank is
;; about to go off-screen

(define (tank-off-screen? t) (or
                              (< (- WIDTH TANK-WIDTH/2) (+ (tank-x t) (* (tank-dir t) TANK-SPEED)))
                              (> (+ 0 TANK-WIDTH/2) (+ (tank-x t) (* (tank-dir t) TANK-SPEED)))))


;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; checks if the newly moved missiles and invaders crashed into each other
;; returns the updated ListOfMissiles
(check-expect (check-crashes-missiles empty empty)
              empty)
(check-expect (check-crashes-missiles 
               empty (list (make-missile 40 40)))  
              (list (make-missile 40 40))) 
(check-expect (check-crashes-missiles (list (make-invader 40 40 INVADER-X-SPEED))
                                      empty)  
              empty)
(check-expect (check-crashes-missiles (list (make-invader 40 40 INVADER-X-SPEED))
                                      (list (make-missile 40 40)))  
              empty)
(check-expect (check-crashes-missiles (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 40 40) (make-missile 30 50)))  
              empty) 
(check-expect (check-crashes-missiles (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 20 40) (make-missile 40 40)))  
              (list (make-missile 20 40)))
(check-expect (check-crashes-missiles (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 100 100) (make-missile 20 40)))  
              (list (make-missile 20 40)))
(check-expect (check-crashes-missiles (list (make-invader 20 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 40 40) (make-missile 100 100)))  
              (list (make-missile 40 40)))
(check-expect (check-crashes-missiles (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 40 40) (make-missile 100 100)))  
              empty)
(check-expect (check-crashes-missiles (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 100 100) (make-missile 40 40)))  
              empty)


(define (check-crashes-missiles loi lom)
  (cond [(or (empty? loi) (empty? lom)) lom]
        [else (if (gets-hit-missiles? (first lom) loi)            
                  (check-crashes-missiles loi (rest lom))
                  (cons (first lom) (check-crashes-missiles loi (rest lom))))]))


;; Missile ListOfInvaders -> Boolean
;; produces true if Invader's coordinates are
;; equal to any of the missiles'
(check-expect (gets-hit-missiles? (make-missile 40 40) empty)
              false)
(check-expect (gets-hit-missiles? (make-missile 40 40)
                                  (list (make-invader 40 20 1)))
              false)
(check-expect (gets-hit-missiles? (make-missile 40 40)
                                  (list (make-invader 40 20 1)
                                        (make-invader 30 30 1)))
              true)
(check-expect (gets-hit-missiles? (make-missile 40 40) (list (make-invader 40 20 1)
                                                             (make-invader 40 40 1)))
              true)
(check-expect (gets-hit-missiles? (make-missile 40 40)
                                  (list (make-invader 40 40 1)
                                        (make-invader 40 20 1))) true)

             
(define (gets-hit-missiles? missile loi)
  (cond [(empty? loi) false]
        [else (if (collide? (first loi) missile)
                  true
                  (gets-hit-missiles? missile (rest loi)))]))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; checks if the newly moved missiles and invaders crashed into each other
;; returns the updated ListOfInvaders
(check-expect (check-crashes-invaders empty empty)
              empty)
(check-expect (check-crashes-invaders empty
                                      (list (make-missile 40 40)))  
              empty)
(check-expect (check-crashes-invaders (list (make-invader 40 40 INVADER-X-SPEED))
                                      empty)  
              (list (make-invader 40 40 INVADER-X-SPEED)))
(check-expect (check-crashes-invaders (list (make-invader 40 40 INVADER-X-SPEED))
                                      (list (make-missile 40 40)))  
              empty)
(check-expect (check-crashes-invaders (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 40 40) (make-missile 50 50)))  
              (list (make-invader 100 100 INVADER-X-SPEED))) 
(check-expect (check-crashes-invaders (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 30 40)
                                            (make-missile 40 40)))  
              (list (make-invader 100 100 INVADER-X-SPEED)))
(check-expect (check-crashes-invaders (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 100 100)
                                            (make-missile 20 40)))  
              (list (make-invader 40 40 INVADER-X-SPEED)))
(check-expect (check-crashes-invaders (list (make-invader 20 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 40 40)
                                            (make-missile 100 100)))
              (list (make-invader 20 40 INVADER-X-SPEED)))
(check-expect (check-crashes-invaders (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 40 40)
                                            (make-missile 100 100)))  
              empty)
(check-expect (check-crashes-invaders (list (make-invader 40 40 INVADER-X-SPEED)
                                            (make-invader 100 100 INVADER-X-SPEED))
                                      (list (make-missile 100 100)
                                            (make-missile 40 40)))  
              empty)


(define (check-crashes-invaders loi lom)
  (cond [(or (empty? loi) (empty? lom)) loi]
        [else (if (gets-hit? (first loi) lom)            
                  (check-crashes-invaders (rest loi) lom)
                  (cons (first loi) (check-crashes-invaders (rest loi) lom)))]))


;; Invader ListOfMissiles -> Boolean
;; produces true if Invader's coordinates are
;; equal to any of the missiles'
(check-expect (gets-hit? (make-invader 40 40 1)
                         empty)
              false)
(check-expect (gets-hit? (make-invader 40 40 1)
                         (list (make-missile 40 20)))
              false)
(check-expect (gets-hit? (make-invader 40 40 1)
                         (list (make-missile 40 20) (make-missile 30 30)))
              true)
(check-expect (gets-hit? (make-invader 40 40 1)
                         (list (make-missile 40 20) (make-missile 40 40))) true)
(check-expect (gets-hit? (make-invader 40 40 1)
                         (list (make-missile 40 40) (make-missile 40 20))) true)


(define (gets-hit? invader lom)
  (cond [(empty? lom) false]
        [else (if (collide? invader (first lom))
                  true
                  (gets-hit? invader (rest lom)))]))


;; Invader Missile -> Boolean
;; produces true if invader and missile
;; have the same coordiantes
(check-expect (collide? (make-invader 30 30 1)
                        (make-missile 30 30))
              true)
(check-expect (collide? (make-invader 30 0 1)
                        (make-missile 30 30))
              false)
(check-expect (collide? (make-invader 33 31.5 1)
                        (make-missile 43 31))
              true)
(check-expect (collide? (make-invader 30 15 1)
                        (make-missile 30 0))
              false)

(check-expect (collide? (make-invader 30 0 1)
                        (make-missile 50 30))
              false)


(define (collide? invader missile)
  (and
   (check-range2 (invader-x invader)
                (- (missile-x missile) HIT-RANGE)
                (+ (missile-x missile) HIT-RANGE))
   (check-range2 (invader-y invader)
                (- (missile-y missile) HIT-RANGE)
                (+ (missile-y missile) HIT-RANGE))))


;; Integer Integer Integer -> Boolean
;; produces true if a is between b and c
;; when b is lower bound and c is upper bound
(define (check-range2 a b c)
  (and
   (>= a b)
   (<= a c)))


;; ===============
;; Game graphics

(define TANKS-LEFT-EDGE  (+ 0 TANK-WIDTH/2))
(define TANKS-RIGHT-EDGE (- WIDTH TANK-WIDTH/2))

;; Game -> Image
;; produces an image of all objects in the game

;; Tank and its interaction with borders
(check-expect (render (make-game empty empty (make-tank 100 1)))
              (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render (make-game empty empty (make-tank TANKS-RIGHT-EDGE 1)))
              (place-image TANK (- WIDTH TANK-WIDTH/2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render (make-game empty empty (make-tank TANKS-LEFT-EDGE -1)))
              (place-image TANK (+ 0 TANK-WIDTH/2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; Invaders and their interaction with borders
(check-expect (render (make-game (list (make-invader 100 100 INVADER-X-SPEED))
                                 empty
                                 (make-tank 100 1)))
              (place-image INVADER 100 100
                           (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                        BACKGROUND)))
;; tests left border when == 0
(check-expect (render (make-game (list (make-invader 0 200 (- INVADER-X-SPEED))
                                       (make-invader 100 100 INVADER-X-SPEED))
                                 empty
                                 (make-tank 100 1)))
              (place-image INVADER (+ 0 INVADER-WIDTH/2) 200 
                           (place-image INVADER 100 100 (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                                     BACKGROUND))))
;; tests right border when == 0
(check-expect (render (make-game
                       (list (make-invader WIDTH 200 (- INVADER-X-SPEED))
                             (make-invader 100 100 INVADER-X-SPEED))
                       empty
                       (make-tank 100 1)))
              (place-image INVADER (- WIDTH INVADER-WIDTH/2) 200 
                           (place-image INVADER 100 100 (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                                     BACKGROUND))))

;; tests right border when != 0
(check-expect (render (make-game
                       (list (make-invader 0 200 (- INVADER-X-SPEED))
                             (make-invader (- WIDTH 0.1) 100 INVADER-X-SPEED))
                       empty
                       (make-tank 100 1)))
              (place-image INVADER (+ 0 INVADER-WIDTH/2) 200
                           (place-image INVADER (- WIDTH INVADER-WIDTH/2) 100
                                        (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))
;; tests left border when != 0
(check-expect (render (make-game
                       (list (make-invader 0 200 (- INVADER-X-SPEED))
                             (make-invader (+ 0 0.1) 100 INVADER-X-SPEED))
                       empty
                       (make-tank 100 1)))
              (place-image INVADER (+ 0 INVADER-WIDTH/2) 200
                           (place-image INVADER (+ 0 INVADER-WIDTH/2) 100
                                        (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))
;; tests both left and right borders
(check-expect (render (make-game
                       (list (make-invader 0 200 (- INVADER-X-SPEED))
                             (make-invader (- WIDTH 0.1) 100 INVADER-X-SPEED))
                       empty
                       (make-tank 100 1)))
              (place-image INVADER (+ 0 INVADER-WIDTH/2) 200 
                           (place-image INVADER (- WIDTH INVADER-WIDTH/2) 100
                                        (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))
;; Missiles and their interactions with border
(check-expect (render (make-game
                       empty
                       (list (make-missile 50 50))
                       (make-tank 100 1)))
              (place-image MISSILE 50 50
                           (draw-tank (make-tank 100 1))))
;; tests right border when == WIDTH & list with at least 2 cases
(check-expect (render (make-game
                       empty
                       (list
                        (make-missile WIDTH 50)
                        (make-missile 100 100)) (make-tank 100 1)))
              (place-image MISSILE 100 100
                           (place-image MISSILE (- WIDTH MISSILE-WIDTH/2) 50                                       
                                        (draw-tank (make-tank 100 1)))))
;; tests right border when != WIDTH
(check-expect (render (make-game
                       empty
                       (list (make-missile (- WIDTH 2) 50))
                       (make-tank 100 1)))
              (place-image MISSILE (- WIDTH MISSILE-WIDTH/2) 50                          
                           (draw-tank (make-tank 100 1))))
;; tests left border when == 0
(check-expect (render (make-game
                       empty
                       (list (make-missile 0 50))
                       (make-tank 100 1)))
              (place-image MISSILE (+ 0 MISSILE-WIDTH/2) 50     
                           (draw-tank (make-tank 100 1))))
;; tests left border when != 0 
(check-expect (render (make-game
                       empty
                       (list (make-missile (+ 0 2) 50))
                       (make-tank 100 1)))
              (place-image MISSILE (+ 0 MISSILE-WIDTH/2) 50
                           (draw-tank (make-tank 100 1))))


(define (render s)
  (draw-missiles (game-missiles s)
                 (draw-invaders (game-invaders s)
                                (draw-tank (game-tank s)))))


;; Tank -> Image
;; produces an image with a tank (using given
;; tank's variables) on top of a blank canvas 
(check-expect (draw-tank
               (make-tank 100 1))
              (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (draw-tank
               (make-tank TANKS-RIGHT-EDGE 1))
              (place-image TANK TANKS-RIGHT-EDGE (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (draw-tank
               (make-tank TANKS-RIGHT-EDGE -1))
              (place-image TANK TANKS-RIGHT-EDGE (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))


(define (draw-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; Tank -> Boolean
;; produces true if tank image would be
;; drawn off sceen on left border
(define (tank-off-screen-left? t)
  (< (tank-x t) (+ 0 TANK-WIDTH/2)))


;; Tank -> Boolean
;; produces true if tank image would be
;; drawn off sceen on right border
(define (tank-off-screen-right? t)
  (> (tank-x t) (- WIDTH TANK-WIDTH/2)))


;; ListOfInvaders Image -> Image
;; produces an image with the invaders drawn
;; on top of the provided picture as background

;; Invaders and their interaction with borders
(check-expect (draw-invaders
               (list (make-invader 100 100 INVADER-X-SPEED))
               (draw-tank (make-tank 100 1)))
              (place-image INVADER 100 100 (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                        BACKGROUND)))
;; tests left border when == 0
(check-expect (draw-invaders
               (list
                (make-invader 0 200 (- INVADER-X-SPEED))
                (make-invader 100 100 INVADER-X-SPEED))
               (draw-tank(make-tank 100 1)))
              (place-image INVADER (+ 0 INVADER-WIDTH/2) 200 
                           (place-image INVADER 100 100
                                        (draw-tank(make-tank 100 1)))))
;; tests right border when == 0
(check-expect (draw-invaders
               (list (make-invader WIDTH 200 (- INVADER-X-SPEED))
                     (make-invader 100 100 INVADER-X-SPEED))
               (draw-tank(make-tank 100 1)))
              (place-image INVADER (- WIDTH INVADER-WIDTH/2) 200 
                           (place-image INVADER 100 100 (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                                     BACKGROUND))))
;; tests left border when != 0
(check-expect (draw-invaders
               (list (make-invader 0 200 (- INVADER-X-SPEED))
                     (make-invader (- WIDTH 0.1) 100 INVADER-X-SPEED))
               (draw-tank(make-tank 100 1)))
              (place-image INVADER (+ 0 INVADER-WIDTH/2) 200
                           (place-image INVADER (- WIDTH INVADER-WIDTH/2) 100
                                        (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))
;; tests left border when != 0
(check-expect (draw-invaders
               (list (make-invader 0 200 (- INVADER-X-SPEED))
                     (make-invader (+ 0 0.1) 100 INVADER-X-SPEED))
               (draw-tank(make-tank 100 1)))
              (place-image INVADER (+ 0 INVADER-WIDTH/2) 200
                           (place-image INVADER (+ 0 INVADER-WIDTH/2) 100
                                        (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))
;; tests both left and right border
(check-expect (draw-invaders
               (list
                (make-invader 0 200 (- INVADER-X-SPEED))
                (make-invader (- WIDTH 0.1) 100 INVADER-X-SPEED))
               (draw-tank(make-tank 100 1)))
              (place-image INVADER (+ 0 INVADER-WIDTH/2) 200 
                           (place-image INVADER (- WIDTH INVADER-WIDTH/2) 100 (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2)
                                                                                           BACKGROUND))))
(check-expect (draw-invaders
               (check-crashes-invaders
                (list (make-invader 100 100 INVADER-X-SPEED)
                      (make-invader 200 200 INVADER-X-SPEED))
                (list
                 (make-missile 100 100)
                 (make-missile 200 200)))
               BACKGROUND)
              BACKGROUND)
                                                           

(define (draw-invaders loi img)
  (cond [(empty? loi) img]
        [else 
         (draw-invaders (rest loi)
                        (draw-invader (first loi) img))]))


;; Invader Image -> Image
;; draws invader on top of Image as a background
(define (draw-invader invader img)
  (cond [(not (invader-off-screen? invader))
         (place-image INVADER (invader-x invader) (invader-y invader) img)]
        [(invader-off-screen-left? invader)
         (place-image INVADER INVADER-WIDTH/2 (invader-y invader) img)]
        [(invader-off-screen-right? invader)
         (place-image INVADER (- WIDTH INVADER-WIDTH/2) (invader-y invader) img)]))


;; Invader -> Boolean
;; produces true if the drawing of invader will be off-screen 
(define (invader-off-screen? invader)
  (or
   (invader-off-screen-left? invader)
   (invader-off-screen-right? invader)))


;; Invader -> Boolean
;; produces true if invader would be drawn off-screen
;; to the right
(define (invader-off-screen-right? invader)
  (< WIDTH
     (+ (invader-x invader) INVADER-WIDTH/2)))

;; Invader -> Boolean
;; produces true if invader would be off-screen
;; to the left
(define (invader-off-screen-left? invader)
  (> 0
     (- (invader-x invader) INVADER-WIDTH/2)))


;; ListOfMissiles Image -> Image
;; produces an image with the missiles drawn
;; on top of the provided picture as background

;; Missiles and their interactions with border
(check-expect (draw-missiles
               (list (make-missile 50 50))
               (draw-tank (make-tank 100 1)))
              (place-image MISSILE 50 50
                           (draw-tank (make-tank 100 1))))
;; tests right border when == WIDTH & list with at least 2 cases
(check-expect (draw-missiles
               (list (make-missile WIDTH 50)
                     (make-missile 100 100))
               (draw-tank (make-tank 100 1)))
              (place-image MISSILE 100 100                          
                           (place-image MISSILE (- WIDTH MISSILE-WIDTH/2) 50
                                        (draw-tank (make-tank 100 1)))))
;; tests right border when != WIDTH
(check-expect (draw-missiles
               (list (make-missile (- WIDTH 2) 50))
               (draw-tank (make-tank 100 1)))
              (place-image MISSILE (- WIDTH MISSILE-WIDTH/2) 50                          
                           (draw-tank (make-tank 100 1))))
;; tests left border when == 0
(check-expect (draw-missiles
               (list (make-missile 0 50))
               (draw-tank (make-tank 100 1)))
              (place-image MISSILE (+ 0 MISSILE-WIDTH/2) 50                         
                           (draw-tank (make-tank 100 1))))
;; tests left border when != 0 
(check-expect (draw-missiles
               (list (make-missile (+ 0 2) 50))
               (draw-tank (make-tank 100 1)))
              (place-image MISSILE (+ 0 MISSILE-WIDTH/2) 50                          
                           (draw-tank (make-tank 100 1))))


(define (draw-missiles loi img)
  (cond [(empty? loi) img]
        [else 
         (draw-missiles (rest loi)
                        (draw-missile (first loi) img))]))


;; missile Image -> Image
;; draws missile on top of Image as a background

(define (draw-missile missile img)
  (cond [(not (missile-off-screen? missile))
         (place-image MISSILE (missile-x missile) (missile-y missile) img)]
        [(missile-off-screen-left? missile)
         (place-image MISSILE MISSILE-WIDTH/2 (missile-y missile) img)]
        [(missile-off-screen-right? missile)
         (place-image MISSILE (- WIDTH MISSILE-WIDTH/2) (missile-y missile) img)]))


;; missile -> Boolean
;; produces true if the drawing of missile will be off-screen 
(define (missile-off-screen? missile)
  (or
   (missile-off-screen-left? missile)
   (missile-off-screen-right? missile)))


;; missile -> Boolean
;; produces true if missile would be drawn off-screen
;; to the right
(define (missile-off-screen-right? missile)
  (< WIDTH
     (+ (missile-x missile) MISSILE-WIDTH/2)))


;; missile -> Boolean
;; produces true if missile would be off-screen
;; to the left
(define (missile-off-screen-left? missile)
  (> 0
     (- (missile-x missile) MISSILE-WIDTH/2)))


;; Game -> Game
;; changes tank-direction based on arrow keys click
;; and creates new missile objects based on space bar click,
;; dispensed from the top of the tank



;; ============================
;; Game interaction


(check-expect (shoot-and-move
               (make-game
                empty
                empty
                (make-tank 100 1)) " ")
              (make-game
               empty
               (list (make-missile 100 (- HEIGHT TANK-HEIGHT/2)))
               (make-tank 100 1)))
(check-expect (shoot-and-move
               (make-game empty empty (make-tank WIDTH (- 1)))
               "a")
              (make-game empty empty (make-tank WIDTH -1)))
(check-expect (shoot-and-move
               (make-game empty empty
                          (make-tank 200 1))
               "left")
              (make-game empty empty (make-tank 200 (- 1))))
(check-expect (shoot-and-move
               (make-game empty empty (make-tank 200 (- 1)))
               "right")
              (make-game empty empty (make-tank 200 1)))

(check-expect (shoot-and-move
               (make-game empty empty (make-tank 200 1))
               "right")
              (make-game empty empty (make-tank 200 1)))
(check-expect (shoot-and-move
               (make-game empty empty (make-tank 200 (- 1)))
               "left")
              (make-game empty empty (make-tank 200 (- 1))))


(check-expect (shoot-and-move
               (make-game empty empty (make-tank 100 1))
               " ")
              (make-game empty (list (make-missile 100 (- HEIGHT TANK-HEIGHT/2))) (make-tank 100 1)))


(define (shoot-and-move s ke)
  (cond [(not (active-key? ke)) s]
        [(string=? ke " ")
         (make-game (game-invaders s)
                    (cons (make-missile (tank-x (game-tank s)) (- HEIGHT  TANK-HEIGHT/2))
                          (game-missiles s))
                    (game-tank s))]
        [(string=? ke "left")
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s)) (- 1)))]
        [else (make-game (game-invaders s)
                         (game-missiles s)
                         (make-tank (tank-x (game-tank s)) 1))]))


;; KeyEvent -> Boolean
;; produces true if the keyEvent is one of
;; space, left, or right


(define (active-key? ke)
  (or
   (string=? ke " ")
   (string=? ke "left")
   (string=? ke "right")))



;; ===============
;; Status Checking


;; Game -> Boolean
;; produces true when an invader has
;; y == HEIGHT
(check-expect (lost?
               (make-game empty empty (make-tank 40 -1)))
              false)
(check-expect (lost?
               (make-game
                (list (make-invader 50 50 INVADER-X-SPEED))
                empty
                (make-tank 100 1)))
              false)
(check-expect (lost?
               (make-game
                (list (make-invader 50 (- HEIGHT TANK-HEIGHT/2) INVADER-X-SPEED))
                empty
                (make-tank 100 1)))
              false)
(check-expect (lost?
               (make-game
                (list (make-invader 50 0 INVADER-X-SPEED))
                empty
                (make-tank 100 1)))
              false)
(check-expect (lost?
               (make-game
                (list (make-invader 50 (+ 1 (- HEIGHT TANK-HEIGHT/2)) INVADER-X-SPEED))
                empty
                (make-tank 100 1)))
              true)
(check-expect (lost?
               (make-game
                (list (make-invader 50 HEIGHT INVADER-X-SPEED))
                empty
                (make-tank 100 1)))
              true)
(check-expect (lost?
               (make-game
                (list (make-invader 50 HEIGHT INVADER-X-SPEED) (make-invader 100 50 INVADER-X-SPEED))
                empty
                (make-tank 100 1)))
              true)
(check-expect (lost?
               (make-game
                (list (make-invader 50 100 INVADER-X-SPEED) (make-invader 100 HEIGHT INVADER-X-SPEED))
                empty
                (make-tank 100 1)))
              true)
(check-expect (lost?
               (make-game
                (list (make-invader 50 HEIGHT INVADER-X-SPEED) (make-invader HEIGHT 50 INVADER-X-SPEED))
                empty
                (make-tank 100 1)))
              true)


(define (lost? game)
  (cond [(empty? (game-invaders game)) false]
        [else
         (if (< (- HEIGHT TANK-HEIGHT/2)
                (invader-y (first (game-invaders game))))
             true
             (lost? (make-game
                     (rest (game-invaders game))
                     (game-missiles game)
                     (game-tank game))))]))




;; Inte -> Image
;; produces the hello image
(define (hello-page inte)
  (place-image START-BUTTON (/ WIDTH 2) (/ HEIGHT HEIGHT-RATIO) BACKGROUND))


;; Inte Integer Integer MouseEvent -> Inte
;; Upon click on the right place of the screen
;; starts the game
(define (start-game inte x y me)
  (if (and
       (with-in-bound x y)
       (string=? me "button-down"))
  (real-main (make-game empty empty (make-tank (/ WIDTH 2) 1)))
  (+ 4 4)))

;; Integer Integer -> Boolean
;; produces true if the mouse click
;; was on the button
(define (with-in-bound x y)
  (and
   (< (- (/ WIDTH 2) (/ (image-width START-BUTTON) 2)) x)
   (> (+ (/ WIDTH 2) (/ (image-width START-BUTTON) 2)) x)
   
   (< (- (/ HEIGHT HEIGHT-RATIO) (/ (image-height START-BUTTON) 2)) y)
   (> (+ (/ HEIGHT HEIGHT-RATIO) (/ (image-height START-BUTTON) 2)) y)))
   
 







