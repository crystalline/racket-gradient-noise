#lang racket

(require "lib3d.rkt")
(require "util.rkt")
(require "matrix.rkt")

(require racket/list)
(require racket/vector)
(require plot)

;Graphics
(define (lerp param a b) (+ a (* param (- b a))))

(define (fade t) (* t t t (+ (* 6 t t) (* -15 t) 10)))

(define gtl 512)

(define (random-float)
  (define N 4000000000)
  (/ (exact->inexact (random N)) N))

(define grad-table (build-vector gtl (lambda (i) (normalize (list (- (* (random-float) 2.0) 1.0)
                                                                  (- (* (random-float) 2.0) 1.0))))))

;Slow integer hash
(define b^ bitwise-xor)
(define (b<< x s) (arithmetic-shift x s))
(define (b>> x s) (arithmetic-shift x (- s)))

(define (inthash a)
  (let ((t a))
    (set! a (b^ (b^ a 61) (b>> a 16)))
    (set! a (+ a (b<< a 3)))
    (set! a (b^ a (b>> a 4)))
    (set! a (remainder (* a #x27d4eb2d) (expt 2 32)))
    (set! a (b^ a (b>> a 15)))
    a))

(define (gradient-lookup v)
  (let* ((vi (map inexact->exact v))
         (mix (b^ #x27d4eb2f (bitwise-not
                              (+ (b>> (@ vi 0) 2)
                                 (b^ #b10100101010100110101 (@ vi 0))))
                  (@ vi 1))))
    (vector-ref grad-table (modulo (inthash mix) gtl))))

(define 10v '(1.0 0.0))
(define 01v '(0.0 1.0))
(define 11v '(1.0 1.0))

(define (grad-noise p freq)
  (let* ((ps (s*v freq p))
         (p-grid (map floor ps))
         (prel (v- ps p-grid))
         (pv (list p-grid
                   (v+ p-grid 10v)
                   (v+ p-grid 01v)
                   (v+ p-grid 11v)))
         (dist (map (lambda (v) (v- ps v)) pv))
         (grad (map gradient-lookup pv))
         (vals (map v.v grad dist)))
    ;Interpolation
    (lerp (fade (.y prel))
          (lerp (fade (.x prel)) (@ vals 0) (@ vals 1))
          (lerp (fade (.x prel)) (@ vals 2) (@ vals 3)))))

(define (grad-noise* x y freq)
  (grad-noise (list x y) freq))

(grad-noise '(1.2 1.2) 1.0)

(define B 100.0)

;(plot3d-samples 150)
;(plot3d (surface3d (λ (x y) (+ (grad-noise (list x y) 0.03) (grad-noise (list x y) 0.005)))
;                   (- B) B (- B) B) #:width 800 #:height 800)
;(plot (function fade 0.0 1.0))

(define screen (build/matrix (lambda (i j) (+ (grad-noise* i j 0.02)
                                              (* 0.5 (grad-noise* i j 0.05))
                                              (* 0.25 (grad-noise* i j 0.1))
                                              (* 0.15 (grad-noise* i j 0.2))
                                              (* 0.08 (grad-noise* i j 0.4))))
                                              512 512))

(matrix-show screen)

