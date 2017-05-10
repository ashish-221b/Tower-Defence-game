#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mac-turret (freeze (rotate -90 (scale .625 (bitmap "Used/Machinegun.png")))))
(define snipe-turret (freeze (scale .625 (bitmap "Used/Sniper.png"))))
(define eddy-turret
  (freeze (bitmap "Used/3.png")))
(define lightning-turret
  (freeze 
   (bitmap "Used/4.png")))
(define air-defense (freeze (rotate -90 (scale .625 (bitmap "Used/airdefense.png")))))
(define anti-tank (freeze (rotate -90 (scale .625 (bitmap "Used/anti-tank.png")))))
(define anti-tank1 (freeze (rotate -90 (scale .625 (bitmap "Used/anti-tank1.png")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define e1 (freeze (underlay/xy (circle (* .35 28) "outline" "transparent") 0 2.8
                                (rotate -90 (scale .7 (freeze 22 18 20 28 (bitmap "Used/e1.png")))))))
(define e2 (freeze (underlay/xy (circle (* .35 28) "outline" "transparent") 0 2.8
                                (rotate -90 (scale .7 (freeze 22 18 20 28 (bitmap "Used/e2.png")))))))
(define e4 (freeze (underlay/xy (circle 19.2 "outline" "transparent") 0 5.7
                                (rotate -90 (scale .6 (freeze 10 0 45 64 (bitmap "Used/air-med.png")))))))
(define e3 (freeze (underlay/xy (circle 21.75 "outline" "transparent") 3 0
                                (rotate -90
                                        (scale .75
                                               (freeze 6 7 58 50 (underlay/xy
                                                                  (bitmap "Used/e3-1.png") 0 0
                                                                  (bitmap "Used/e3-2.png"))))))))
(define e5 (freeze (underlay/xy (circle 19.2 "outline" "transparent") 0 2.8
                                (rotate -90 (scale .6 (freeze 10 0 45 64 (bitmap "Used/air-big.png")))))))
(define projectile (freeze (underlay/xy
                            (circle 12 "outline" "transparent" ) 0 6
                            (rotate -90 (scale .6 (freeze 22 12 20 40 (bitmap "Used/projectile.png")))))))
(define explode (freeze (scale .5 (freeze 4 4 160 160 (bitmap "Used/explosion.png")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
