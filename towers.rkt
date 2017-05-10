#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "object.rkt")
(provide tow-gen)
(define (f n)
  (if (= n 5) 200
      100))
(define (tan-to-sin/cos v)
  (if (= v 0) (cons 1 0)
      (cons (/ 1 (sqrt (+ (* v v) 1))) (/ v (sqrt (+ (* v v) 1))))))
(define (tan-to2 x y)
  (if (= x 0)
      (cons 0 1)
      (tan-to-sin/cos (/ y x))))
(define (theta l)
  (let* [(x-val(- (car l) (caddr l)))
         (y-val(- (cadddr l) (cadr l)))
         (angle
          (if (= x-val 0)
              90
              (* 180 (/ (atan (/ y-val x-val)) pi))))]
    (if (> x-val 0) angle
        (+ 180 angle))))
(define (value-init type)
  (cond ((= type 1)
         (list 5 60 100 40 (λ(x) (send x get-h)) < (λ(x) (or (= (send x get-type) 1)
                                                             (= (send x get-type) 2)
                                                             (= (send x get-type) 3)))))
        ((= type 2)
         (list 200 100 1 40 (λ(x) (send x get-h)) > (λ(x) (or (= (send x get-type) 1)
                                                              (= (send x get-type) 2)))))
        ((= type 3)
         (list 2 80 100 0 (λ(x) (send x get-h)) < (λ(x) #t)))
        ((= type 4)
         (list 10 80 100 40 (λ(x) (send x get-h)) < (λ(x) (or (= (send x get-type) 1)
                                                              (= (send x get-type) 2)))))
        ((= type 5)
         (list 500 120 1 40 (λ(x) (send x get-type)) > (λ(x) (or (= (send x get-type) 4)
                                                                 (= (send x get-type) 5)))))
        ((= type 6)
         (list 1000 200 1 80 (λ(x) (- 400 (send x get-yc))) < (λ(x) (= (send x get-type) 3))))))
(define projectile
  (class object%
    (init-field x-pos)
    (init-field y-pos)
    (init-field angle)
    (init-field v)
    (init-field target)
    (init-field dam)
    (super-new)
    (define x-cen (+ x-pos 12))
    (define y-cen (+ y-pos 12))
    (define active #t)
    (define/public (move)            ;moves the projectile towards target 
      (let* [(x(send target get-xc)) ;Tracking algorithm (Special feature)
             (y(send target get-yc))
             (xd(if (>= (- x x-cen) 0) 1
                    -1))
             (yd(if (>= (- y y-cen) 0) 1
                    -1))
             (dir(tan-to2 (- x x-cen) (- y y-cen)))]
        (set! x-cen (+ x-cen (* v (* xd (abs (car dir))) .05)))
        (set! y-cen (+ y-cen (* v (* yd (abs (cdr dir))) .05)))
        (set! x-pos (- x-cen 12))
        (set! y-pos (- y-cen 12))
        (set! angle (theta (list x
                                 y
                                 x-cen y-cen)))))
    (define/public (damage)
      (let* [(x(send target get-xc))
             (y(send target get-yc))]
        (cond ((< (sqrt (+ (expt (- x x-cen) 2) (expt (- y y-cen) 2))) 10)
               (send target do-damage dam)
               (set! active #f)))))
    (define/public (active?)
      active)
    (define/public (get-pos)
      (cons x-pos y-pos))
    (define/public (get-angle)
      angle)))
(define tow                                        ;the class for all defensive stuctures
  (class object%                                        
    (init-field x-pos)                        ;x-pos ,y-pos just squre number from(1,1) to(10,10)
    (init-field y-pos)
    (init-field type)                     ;which out of six type of defences
    (super-new)
    (define setter (value-init type)) ;function to initialise the the tower
    (define base-damage (list-ref setter 0)) ;parameters according to type of the tower
    (define range (list-ref setter 1))      
    (define max-cap (list-ref setter 2))
    (define reload-time (list-ref setter 3))     ; initialising all the variables
    (define pref (list-ref setter 4))
    (define oper (list-ref setter 5))
    (define realm-pred? (list-ref setter 6))
    (define remaining-shots max-cap)             ; maximum shots before reload
    (define reloaded #t)
    (define will-be-ready-by 0)
    (define x-cor (- (* x-pos 40) 20))           ; actuall position according to map
    (define y-cor (- (* y-pos 40) 20))
    (define enemy_range (list ))        ;list containing enemy sorted according to prference set 
    (define l '())
    (define p-list '())
    (define angle 0)
    (define/public (locate-enemy elist)
      (cond (reloaded
             (let [(in_range(filter
                             realm-pred?  ;filters enemy according to range in wether groung/air type
                             (filter
                              (λ(x) (<=
                                     (+ (expt
                                         (- (send x get-xc) x-cor) 2)
                                        (expt
                                         (- (send x get-yc) y-cor) 2))
                                     (* range range))) elist)))]
               (set! enemy_range (sort in_range #:key pref oper))
               (if (not (null? enemy_range))
                   (set! angle (theta (list
                                       (send (car enemy_range) get-xc)
                                       (send (car enemy_range) get-yc) x-cor y-cor)))
                   (set! angle angle)
                   )))))
    
    (define/public (execute q)          ; executes attack or the slowdown effect
      (begin
        (set! l '()) 
        (cond ((= remaining-shots 0)      ;checks remaining ammo
               (cond ((= q (- will-be-ready-by 20)) (set! reloaded #t)))
               (cond ((>= q will-be-ready-by) (set! remaining-shots max-cap))))
              (#t
               (cond ((<= q (- will-be-ready-by 5)) (set! reloaded #t)))
               (set! remaining-shots (- remaining-shots 1))
               (set! will-be-ready-by (if (= remaining-shots 0) (+ q reload-time)
                                          will-be-ready-by))
               (cond ((or (= type 1) (= type 2))
                      (begin
                        (set! l (cons
                                 (list
                                  (send (car enemy_range) get-xc)
                                  (send (car enemy_range) get-yc) x-cor y-cor) l))
                        (send (car enemy_range) do-damage base-damage)))
                     ((= type 3)
                      (map (λ(x) (send x set-v-f base-damage)) enemy_range))
                     ((= type 4)
                      (let [(c(length enemy_range))]
                        (begin
                          (set! l (append l (map (λ(x) (list (send x get-xc)
                                                             (send x get-yc)
                                                             x-cor y-cor)) enemy_range )))
                          (map (λ(x) (begin (send x do-damage (/ base-damage c))
                                            (send x set-v-f 2))) enemy_range))))
                     ((or (= type 5) (= type 6))
                      (set! reloaded #f)
                      (set! p-list (list (new projectile
                                              [x-pos (- (+ x-cor (* 4 (cos (/ (* pi angle)
                                                                              180)))) 12)]
                                              [y-pos (- y-cor (+ (* 4 (sin (/ (* pi angle)
                                                                              180))) 12))]
                                              [angle angle]
                                              [v (f type)]
                                              [target (car enemy_range)]
                                              [dam base-damage])))))))))
    (define/public (reload)
      (set! remaining-shots max-cap))
    (define/public (set-pred p)
      (set! pref (car p))
      (set! oper (cdr p)))
    (define/public (get-x)
      x-pos)
    (define/public (get-y)
      y-pos)
    (define/public (get-type)
      type)
    (define/public (get-er)
      enemy_range)
    (define/public (get-re)
      remaining-shots)
    (define/public (get-l)
      l)
    (define/public (get-angle)
      angle)
    (define/public (reset-angle)
      (set! angle 0))
    (define/public (get-p-list)
      (let [(a p-list)]
        (set! p-list '())
        a))
    (define/public (is-reloaded)
      reloaded)
    (define/public (get-range)
      range)))
(define (tow-gen c)
  (new tow [x-pos (car c)] [y-pos (cadr c)] [type (caddr c)] )) ;tower generator