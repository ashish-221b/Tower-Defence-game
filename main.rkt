#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require (prefix-in a: racket/gui))
(require "base-map.rkt")
(require "enemy.rkt")
(require "towers.rkt")
(require "object.rkt")
(require "layers.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (zip l1 l2)
  (if (or (null? l1) (null? l2)) `()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define money 500)
(define time 0)
(define list_of_tow_objects '())
(define game-state 0)
(define build-mode 0)
(define selected-tile (cons 1 1))
(define l1 l2)
(define can_build lim) ; list of lists containing #t for blocks where tower can be built
(define enemy-stat '()) ; list of 4 lists and each of these list contains a number(index)
;list of enemy objects and entry time
(define active-enemy '()) ;list of all alive enemies on the map
(define strike-list '())  ;used to show turret attacking enemies using lines
(define projectile-list '()) ;contains projectile objects
(define paused? #f)
(define base-health 20)
(define exploding-list '())
(define kills 0)
(define next-wave (zip '(1 2 3 4 5) (wave-report 1)))
(define mapb (freeze (bitmap "Used/map.png"))) ;game-map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (tower-cost n)
  (cond ((= n 1) 100)
        ((= n 2) 250)
        ((= n 3) 250)
        ((= n 4) 500)
        ((= n 5) 300)
        ((= n 6) 300)))
(define (enemy-bounty n)
  (cond ((= n 1) 50)
        ((= n 2) 100)
        ((= n 3) 200)
        ((= n 4) 200)
        ((= n 5) 250)))
(define (enemy-spawn)
  (begin                           ;adds enemy-objects to active-enemy from various categories
    (map (λ(x) (cond ((not (null? (caddr x)))  
                      (cond ((= time (car (caddr x)))  ;according to time
                             (e-adder x)))))) enemy-stat)))
(define (e-adder x)
  (begin
    (let [(current-addition(car (cadr x)))
          (y (list (car x) (cdr (cadr x)) (cdr (caddr x))))]
      (set! active-enemy (append active-enemy (list current-addition)))
      (set! enemy-stat (simple-m (car x) y enemy-stat)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (executions)
  (set! exploding-list (append exploding-list
                               (map (λ(x) (cons x 0))
                                    (filter (λ(x) (<= (send x get-h) 0)) active-enemy)))) 
  (set! active-enemy
        (filter (λ(x) (cond ((> (send x get-yc) 400)
                             (begin (set! base-health (- base-health 1)) #f))
                            ((> (send x get-h) 0) #t)
                            (#t (begin (set! money (+ (enemy-bounty (send x get-type)) money))
                                       (set! kills (+ 1 kills))
                                       #f)))) active-enemy))
  (set! strike-list '())
  (set! exploding-list (map (λ(x) (cons (car x) (+ 1 (cdr x)))) exploding-list))
  (set! exploding-list (filter (λ(x) (< (cdr x) 11)) exploding-list))
  (map (λ(x) (send x locate-enemy active-enemy)) list_of_tow_objects)
  (map (λ(x) (begin (send x execute time) (set! strike-list (append strike-list (send x get-l)))
                    (set! projectile-list (append projectile-list (send x get-p-list)))))
       (filter (λ(x) (not (null? (send x get-er)))) list_of_tow_objects))
  ;;filter function above calls execute function on those tower objects only which have enemy in range
  (enemy-spawn)
  (map (λ(x) (send x move-along)) active-enemy) ;moves all enemies in the list forward along the path
  (set! projectile-list (filter (λ(x) (send x active?)) projectile-list))
  (map (λ(x) (send x move)) projectile-list)
  (map (λ(x) (send x damage)) projectile-list)
  (map (λ(x) (send x revert-v-f)) active-enemy))
(define (display-wartime)
  (underlay/xy
   (scale 1.5 (underlay/xy
               (underlay/xy
                (underlay/xy
                 (underlay/xy
                  (underlay/xy
                   (underlay/xy
                    (underlay/xy
                     (rectangle 1300 700 "solid" "black") 20 20 mapb)
                    0 0 (tower-dynam list_of_tow_objects))
                   0 0 (death-map exploding-list))
                  20 20 (shot-map strike-list))
                 20 20 (e-map active-enemy))
                20 20 (projectile-map projectile-list))
               0 0 (health-map active-enemy)))
   630 0 (side-pane (list money base-health kills next-wave))))
(define (display-peacetime)
  (underlay/xy
   (scale 1.5 (underlay/xy
               (underlay/xy
                (rectangle 1300 700 "solid" "black") 20 20
                (if (= build-mode 1)
                    (underlay/xy
                     (underlay/xy mapb 0 0 (map-generator l1)) 0 0
                     (freeze 20 20 400 400 (range-circle selected-tile list_of_tow_objects)))
                    mapb))
               0 0 (tower-dynam list_of_tow_objects)))
   630 0 (side-pane (list money base-health kills next-wave))))
(define (board h)
  (cond ((= (modulo game-state 2) 1)
         (cond ((not paused?)
                (executions)))
         (display-wartime))
        ((= (modulo game-state 2) 0)
         (display-peacetime))))
(define (allenemy)
  (andmap (λ (x) (> (send x get-y) 400)) active-enemy))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (import-enemy)
  (let* [(l(wave-generator(/ (+ 1 game-state) 2)))
         (add-ground1 (list-ref l 0))
         (add-ground2 (list-ref l 1))
         (add-ground3 (list-ref l 2))
         (add-air (list-ref l 6))
         (ground-time1 (list-ref l 3))
         (ground-time2 (list-ref l 4))
         (ground-time3 (list-ref l 5))
         (air-time (list-ref l 7))]
    (set! enemy-stat
          (list
           (list 1 add-ground1 ground-time1)
           (list 2 add-ground2 ground-time2)
           (list 3 add-ground3 ground-time3)
           (list 4 add-air air-time)))
    (if (< (/ (+ 3 game-state) 2) 9)
    (set! next-wave (zip (list 1 2 3 4 5)(wave-report (/ (+ 3 game-state) 2))))
    (set! next-wave '()))))
(define (ready-for-battle)
  (begin
    (set! game-state (+ game-state 1))
    (set! time 0)
    (set! active-enemy '())
    (import-enemy)
    (map (λ(x) (send x reload)) list_of_tow_objects)))
(define (war-to-peace)
  (begin
    (set! game-state (+ game-state 1))
    (set! time 0)
    (map (λ(x) (send x reset-angle)) list_of_tow_objects)))
(define (preparation d)
  (cond ((key=? d "b")
         (set! l1 (m-l 1 1 3 l2))
         (set! build-mode 1))
        ((key=? d " ")
         (ready-for-battle))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GUI Implementation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (im x)
  (cond [(= x 1) (a:read-bitmap "Used/1.png")]
        [(= x 2) (a:read-bitmap "Used/2.png")]
        [(= x 3) (a:read-bitmap "Used/3.png")]
        [(= x 4) (a:read-bitmap "Used/4.png")]
        [(= x 5) (a:read-bitmap "Used/5.png")]
        [(= x 6) (a:read-bitmap "Used/6.png")]))
(define button-icon1 (im 1))
(define button-icon2 (im 2))
(define button-icon3 (im 3))
(define button-icon4 (im 4))
(define button-icon5 (im 5))
(define button-icon6 (im 6))
(define (select-turret)
  (define (output x)
    (define (fr x)
      (cond [(= x 1) fr-1]
            [(= x 2) fr-2]
            [(= x 3) fr-3]
            [(= x 4) fr-4]
            [(= x 5) fr-5]
            [(= x 6) fr-6]))
    (define fr-1 
      (new a:frame% [label "MACHINE-GUN"] [width 350] [height 350] [x 420] [y 350]))
    (define fr-2 
      (new a:frame% [label "SNIPER"] [width 350] [height 350] [x 420] [y 350]))
    (define fr-3 
      (new a:frame% [label "EDDY"] [width 350] [height 350] [x 420] [y 350]))
    (define fr-4 
      (new a:frame% [label "LIGHTNING"] [width 350] [height 350] [x 420] [y 350]))
    (define fr-5 
      (new a:frame% [label "AIR-DEFENSE"] [width 350] [height 350] [x 420] [y 350]))
    (define fr-6 
      (new a:frame% [label "ANTI-TANK"] [width 350] [height 350] [x 420] [y 350]))

    (define image-1 (a:read-bitmap "Used/mac-shot.png"))
    (define image-2 (a:read-bitmap "Used/sniper2.png"))
    (define image-3 (a:read-bitmap "Used/eddy2.png"))
    (define image-4 (a:read-bitmap "Used/lightning2.png"))
    (define image-5 (a:read-bitmap "Used/air2.png"))
    (define image-6 (a:read-bitmap "Used/anti2.png"))

    (void (new a:message% [parent fr-1] [label image-1]))
    (void (new a:message% [parent fr-2] [label image-2]))
    (void (new a:message% [parent fr-3] [label image-3]))
    (void (new a:message% [parent fr-4] [label image-4]))
    (void (new a:message% [parent fr-5] [label image-5]))
    (void (new a:message% [parent fr-6] [label image-6]))
    (void (new a:message% [parent (fr x)] [label (im x)]))  

    (define scrolling-panel
      (new a:vertical-panel%
           (parent (fr x))
           (min-width 10)
           (min-height 90)
           (style '(auto-vscroll))))
    (new a:button%
         (parent scrolling-panel)
         (style (list 'border))
         (label "BUY")
         (callback (λ (button event)
                     (send (fr x) show #f)
                     (the-tower x))))
    (new a:button%
         (parent scrolling-panel)
         (style (list 'border))
         (label "CANCEL")
         (callback (λ (button event)
                     (send (fr x) show #f)
                     (the-tower 0))))  
    (send (fr x) show #t))
  (define frame (new a:frame% [label ""] [x 800] [y 350]))
  
    (new a:button% [label button-icon1]
         [parent frame]
         [min-width 100]
         [callback (lambda (button event)
                     (send frame show #f)
                     (output 1))])
    (new a:button% [label button-icon2]
         [parent frame]
         [min-width 100]
         [callback (lambda (button event)
                     (send frame show #f)
                     (output 2))])
    (new a:button% [label button-icon3]
         [parent frame]
         [min-width 100]
         [callback (lambda (button event)
                     (send frame show #f)
                     (output 3))])
    (new a:button% [label button-icon4]
         [parent frame]
         [min-width 100]
         [callback (lambda (button event)
                     (send frame show #f)
                     (output 4))])
    (new a:button% [label button-icon5]
         [parent frame]
         [min-width 100]
         [callback (lambda (button event)
                     (send frame show #f)
                     (output 5))])
    (new a:button% [label button-icon6]
         [parent frame]
         [min-width 100]
         [callback (lambda (button event)
                     (send frame show #f)
                     (output 6))])
    (send frame show #t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define nem (a:read-bitmap "Used/not-enough-money.jpg"))
(define (not-enough)
  (define no-money
  (new a:frame% [label "Oops.."]
       [width 200] [height 200]))
  (void (new a:message% [parent no-money] [label nem]))
  (new a:button% (parent no-money)
       [label "BACK"]
       [callback (λ(button event)
                   (send no-money show #f))])
  (send no-money show #t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define congrats (a:read-bitmap "Used/congrats.png"))
(define (Winner)
  (define congo
  (new a:frame% [label ""]
       [width 200] [height 200]))
  (void (new a:message% [parent congo] [label congrats]))
  (new a:button% (parent congo)
       [label "PLAY AGAIN"]
       [callback (λ(button event)
                   (start)
                   (send congo show #f))])
  (new a:button% (parent congo)
       [label "QUIT"]
       [callback (λ(button event)
                   (send congo show #f))])
  (send congo show #t))


(define lost (a:read-bitmap "Used/lost.jpg"))
(define (Looser)
  (define loose
  (new a:frame% [label ""]
       [width 200] [height 200]))
  (void (new a:message% [parent loose] [label lost]))
  (new a:button% (parent loose)
       [label "PLAY AGAIN"]
       [callback (λ(button event)
                   (start)
                   (send loose show #f))])
  (new a:button% (parent loose)
       [label "QUIT"]
       [callback (λ(button event)
                   (send loose show #f))])
  (send loose show #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (move-selector a b c d n)                      ;Higher Order Function
  (cond ((c (d selected-tile) n)
         (set! selected-tile (cons (a (car selected-tile) 1) (b (cdr selected-tile) 1)))
         (cond ((not (land?)) (move-selector a b c d n))
               (#t (set! l1 (m-l (cdr selected-tile) (car selected-tile) 3 l2)))))))
(define (build-tower)
  (cond ((list-ref (list-ref can_build (- (cdr selected-tile) 1)) (- (car selected-tile) 1))
         (select-turret))))
(define (the-tower d)
  (cond ((not (= d 0))
         (let [(cost(tower-cost d))]
    (cond ((>= money cost)
           (begin
             (set! money (- money cost))
             (set! can_build (m-l (cdr selected-tile) (car selected-tile) #f can_build))
             (set! list_of_tow_objects
                   (cons (tow-gen (list (car selected-tile) (cdr selected-tile) d))
                         list_of_tow_objects))))
          (#t (not-enough)))))))
(define (remove-tower)
  (cond ((and (not (list-ref (list-ref can_build (- (cdr selected-tile) 1))
                             (- (car selected-tile) 1)))
              (land?))
         (let* [(a(findf (λ(x) (equal? (cons (send x get-x) (send x get-y))
                                       selected-tile)) list_of_tow_objects))
                (type(send a get-type))
                (cost(tower-cost type))]
           (set! money (+ (/ cost 2) money))
           (set! list_of_tow_objects (remove a list_of_tow_objects))
           (set! can_build (m-l (cdr selected-tile) (car selected-tile) #t can_build))))))
(define (end-build)
  (set! build-mode 0)
  (set! l1 l2)
  (set! selected-tile (cons 1 1)))
(define (land?)
  (= (list-ref (list-ref l2 (- (cdr selected-tile) 1)) (- (car selected-tile) 1)) 1)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (builder-mode d)     ;for movement of selector during tower building
  (cond((key=? d "left")
        (move-selector - (λ(x y) x) > car 1))
       ((key=? d "right")
        (move-selector + (λ(x y) x) < car 10))
       ((key=? d "up")
        (move-selector (λ(x y) x) - > cdr 1))
       ((key=? d "down")
        (move-selector (λ(x y) x) + < cdr 10))
       ((key=? d "\r")
        (build-tower))
       ((key=? d "r")
        (remove-tower))
       ((key=? d "b")
        (end-build))))
(define (pause-play)
  (set! paused? (not paused?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (abc w d)    ;functions for key-bindings
  (cond ((= (modulo game-state 2) 0) 
         (cond ((= build-mode 0)
                (preparation d))
               ((= build-mode 1)
                (builder-mode d))))
        ((= (modulo game-state 2) 1)
         (cond ((and (key=? d " ")(allenemy))
                (war-to-peace))
               ((key=? d "p")
                (pause-play)))))) ;makes transition from fight state to buliding and transit stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (timer t)
  (cond ((not paused?) (begin (set! time (+ time 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define instructions (a:read-bitmap "Used/controls.png"))
(define (initialise)
  (set! money 500)
  (set! time 0)
  (set! list_of_tow_objects '())
  (set! game-state 0)
  (set! build-mode 0)
  (set! selected-tile (cons 1 1))
  (set! l1 l2)
  (set! can_build lim) 
  (set! enemy-stat '()) 
  (set! active-enemy '())   
  (set! strike-list '())  
  (set! projectile-list '()) 
  (set! paused? #f)
  (set! base-health 20)
  (set! exploding-list '())
  (set! mapb (freeze (bitmap "Used/map.png")))
  (set! kills 0)
  (set! next-wave (zip '(1 2 3 4 5) (wave-report 1))))
(define frame
  (new a:frame% [label "INSTRUCTIONS"]
       [width 700] [height 700]))
(define (output)
  (void (new a:message% [parent frame] [label instructions]))
  (new a:button% (parent frame)
       [label "BACK"]
       [callback (λ(button event)
                   (send frame show #f))])
  (send frame show #t))
(define myfr 
  (new a:frame% [label "tower-defense"] [width 1000] [height 700] [x 350] [y 50]))
(define myfr2 
  (new a:frame% [label "INSTRUCTIONS"] [width 500] [height 500] [x 350] [y 50]))
(define main-canvas%
  (class a:canvas%
    (define/override (on-event event)
      (let* ((x (if (eq? 'left-down (send event get-event-type)) (send event get-x) 0))
             (y (if (eq? 'left-down (send event get-event-type)) (send event get-y) 0)))
        (define (rep)
          (cond [(and (<= x 636) (>= x 378) (>= y 279) (<= y 311))
                 (begin (send myfr show #f)
                        (initialise)
                        (big-bang 0
                                  (on-draw board) 
                                  (on-tick timer .05)
                                  (on-key abc)
                                  (stop-when (λ(x) (cond ((<= base-health 0)
                                                          (begin (Looser) #t))
                                                         ((>= game-state 16)
                                                          (begin (Winner) #t))
                                                         (#t #f))))
                                  (close-on-stop 2)))]
                [(and (<= x 682) (>= x 333) (>= y 385) (<= y 414)) (output)]
                [(and (<= x 636) (>= x 376) (>= y 494) (<= y 525)) (send myfr show #f)]
                ))
        (rep)))
    (super-new)))
(define mcanvas (new main-canvas% [parent myfr]
                     [paint-callback (λ (c d)
                                       (send d draw-bitmap
                                             (a:read-bitmap "background_im.jpeg") 0 0))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (start)
  (send myfr show #t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;https://opengameart.org/content/tower-defense-300-tilessprites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;