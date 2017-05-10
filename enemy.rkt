#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "object.rkt")
(require "enemydata.rkt")
(provide wave-generator wave-report)
(define time-diff 40)
(define (expander l)
  (define x 0)
  (define (exp-h l res)
    (if (null? l) res
        (begin
          (set! x (+ x 1))
          (exp-h (cdr l) (append res (build-list (car l) (λ(a) x)))))))
  (exp-h l '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ground_queuer l)
  (let* [(elist_gate1(map (λ(x) (creater x 1)) (expander (car l))))
         (elist_gate2(map (λ(x) (creater x 2)) (expander (cadr l))))
         (elist_gate3(map (λ(x) (creater x 3)) (expander (caddr l))))]
    (list elist_gate1 elist_gate2 elist_gate3)))     ;makes time queue for ground enemies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (neolist-ref l n)           ;modified list-ref for refrencing lists beyond their index 
  (cond ((< n (length l)) (list (list-ref l n)))
        (#t '())))            ;also it returns '(element)
(define (list-comb l)
  (define n 0)
  (let* [(l-max(foldr (λ(x y) (max (length x) y)) 0 l))]
    (define (loop n res)
      (if (< (- l-max 1) n) res
          (begin
            (set! n (+ n 1))
          (loop n (append res (list (append* (map (λ(x) (neolist-ref x (- n 1))) l))))))))
    (loop n '())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (air-lister l)
  (append (map (λ (x) (air-creater 4 x)) (car l)) (map (λ (x) (air-creater 5 x)) (cadr l))))
(define (airqueuer l-air)  
    (if (null? l-air) l-air   ;generates list of lists containing air 
        (append* (map air-lister l-air)))) ;units entering at a particular time
(define (wave-generator n)
  (let* [(l-enemy(wave-data n))
         (x1
          (if (not (null? (list-ref l-enemy 0)))
                 (+ (list-ref (list-ref l-enemy 0) 0) (list-ref (list-ref l-enemy 0) 1)) 0))         
         (x2(if (not (null? (list-ref l-enemy 1)))
                   (+ (list-ref (list-ref l-enemy 1) 0) (list-ref (list-ref l-enemy 1) 1)) 0))
         (x3
          (if (not (null? (list-ref l-enemy 2)))
          (+ (list-ref (list-ref l-enemy 2) 0) (list-ref (list-ref l-enemy 2) 1)) 0))
         (l-ground(ground_queuer l-enemy))
         (l-g1(car l-ground))
         (l-g2(cadr l-ground))
         (l-g3(caddr l-ground))
         (l-air(airqueuer (cdddr l-enemy)))
         (g-t1 (build-list (length l-g1) (λ(x) (cond ((<= x x1) (* (+ x 1) 30))
                                                     (#t (+ (* (+ x1 1) 30) (* (- x x1) 60)))))))
         (g-t2 (build-list (length l-g2) (λ(x) (cond ((<= x x2) (* (+ (* 2 x) 1) 20))
                                                     (#t (+ (* (+ (* 2 x2) 1) 20)
                                                            (* 2 (- x x2) 50)))))))
         (g-t3 (build-list (length l-g3) (λ(x) (cond ((< x x3) (* (+ (* 2 x) 2) 20))
                                                     (#t (+ (* 2 x3 20) 70
                                                            (* 2 (- x x3) 50)))))))
         (air-timer(air-time n))]
    (list l-g1 l-g2 l-g3 g-t1 g-t2 g-t3 l-air air-timer)))