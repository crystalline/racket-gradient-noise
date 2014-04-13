#lang racket

(provide syntax
         (except-out 
          (all-defined-out)
          @))


;Misc
(define @ list-ref)
(define pi 3.14159265)

(define .x (λ (v) (@ v 0)))
(define .y (λ (v) (@ v 1)))
(define .z (λ (v) (@ v 2)))

;Vectors
(define (l2-norm lst)
  (sqrt (foldl + 0.0 (map (lambda (x) (* x x)) lst))))

(define (norm lst (n 2))
  (sqrt (foldl + 0.0 (map (lambda (x) (expt x n)) lst))))

(define (m*v mat lst)
  (map (lambda (row) (foldl + 0.0 (map * row lst)))
       mat))

(define (zero-v N)
  (build-list N (λ (x) 0.0)))

(define (s*v s v)
  (map (λ (x) (* s x)) v))

(define  (normalize v)
  (s*v (/ 1.0 (l2-norm v)) v))

(define (v+v A B)
  (map + A B))

(define (v+ . args)
  (foldl v+v (first args) (cdr args)))

(define (v- A [B #f])
  (if (eq? B #f)
      (map - A)
      (map - A B)))

(define (v.v A B)
  (foldl + 0 (map * A B)))

(define (cross* A B)
  (let ((a1 (@ A 0)) (a2 (@ A 1)) (a3 (@ A 2))
                     (b1 (@ B 0)) (b2 (@ B 1)) (b3 (@ B 2)))
    (list (- (* a2 b3) (* a3 b2))
          (- (* a3 b1) (* a1 b3))
          (- (* a1 b2) (* a2 b1)))))

(define (project target vect)
  (s*v (v.v target vect) (normalize target)))

(define (normal-part normal vect) (project normal vect))

(define (tangential-part normal vect) (v- vect (project normal vect)))

;Matrices
(define (take-row A i)
  (list-ref A i))

(define (take-col A j)
  (map (λ (row) (list-ref row j)) A))

(define (row*col A i B j)
  (v.v (take-row A i) (take-col B j)))

(define (build-mat M N proc)
  (build-list M (λ (i) (build-list N (λ (j) (proc i j))))))

(define (n-rows mat)
  (length mat))

(define (n-cols mat)
  (length (car mat)))

(define (m*m A B)
  (build-mat (n-rows A)
             (n-cols B)
             (λ (i j) (row*col A i B j))))

(define (print-v3 lst)
  (printf "[X:~s Y:~s Z:~s]\n" (@ lst 0) (@ lst 1) (@ lst 2)))

(define m3-I
  '((1.0 0.0 0.0)
    (0.0 1.0 0.0)
    (0.0 0.0 1.0)))

(define v3-I '(1.0 1.0 1.0))

(define (ang->rad ang)
  (* 2 pi (/ ang 360)))

;Rotation matrices 
(define (rot-x-m3 phi)
  (list
   (list 1.0 0.0 0.0)
   (list 0.0 (cos phi) (- (sin phi)))
   (list 0.0 (sin phi)    (cos phi))))

(define (rot-y-m3 phi)
  (list
   (list (cos phi) 0.0 (sin phi))
   (list 0.0 1.0 0.0)
   (list (- (sin phi)) 0.0 (cos phi))))

(define (rot-z-m3 phi)
  (list
   (list (cos phi) (- (sin phi)) 0.0)
   (list (sin phi)    (cos phi) 0.0)
   (list 0.0 0.0 1.0)))

(define (rot-matrix rot-v)
  (m*m (rot-z-m3 (@ rot-v 2))
       (m*m (rot-y-m3 (@ rot-v 1)) (rot-x-m3 (@ rot-v 0)))))

(define (rotate-angles angles v)
  (m*v (rot-matrix angles) v))

;Simple stat. procs
(define (AVG lsts)
  (s*v (/ 1.0 (length lsts)) (apply v+ lsts)))

;Rodriguez formula
(define (rotate-axis-angle unit-axis phi v)
  (let ((k unit-axis))
    (v+ (s*v (cos phi) v)
        (s*v (sin phi) (cross* k v))
        (s*v (* (v.v k v) (- 1.0 (cos phi))) k))))

(define projection-xy
  (list '(1.0 0.0 0.0)
        '(0.0 1.0 0.0)
        '(0.0 0.0 0.0)))

;Quaternions
(define (quaternion+ A B)
  (map + A B))

(define (quaternion* A B)
  (let ((s1 (car A)) (s2 (car B))
                     (v1 (cdr A)) (v2 (cdr B)))
    (cons (- (* s1 s2) (v.v v1 v2))
          (v+ (s*v s1 v2)
              (s*v s2 v1)
              (cross* v1 v2)))))

(define (quaternion-conjugate A)
  (cons (car A) (map - (cdr A))))

(define (quaternion-inverse A)
  (s*v (/ 1.0 (norm A)) (quaternion-conjugate A)))

(define (make-quaternion-rotation unit-axis phi)
  (cons (cos (/ phi 2)) (s*v (sin (/ phi 2)) unit-axis)))

(define (apply-quaternion-rotation q v)
  (cdr (quaternion* (quaternion* q (cons 0.0 v))
                    (quaternion-conjugate q))))

(define (quaternion->rot-matrix q)
  (let* ((s (car q)) (v (cdr q))
                     (vx (.x v))
                     (vy (.y v))
                     (vz (.z v))
                     (S (lambda (x) (* x x))))
    (list (list (- 1.0 (* 2.0 (+ (S vy) (S vz)))) (* 2.0 (- (* vx vy) (* s vz))) (* 2.0 (+ (* vx vz) (* s vy))))
          (list (* 2.0 (+ (* vx vy) (* s vz))) (- 1.0 (* 2.0 (+ (S vx) (S vz)))) (* 2.0 (- (* vy vz) (* s vx))))
          (list (* 2.0 (- (* vx vz) (* s vy))) (* 2.0 (+ (* vy vz) (* s vx))) (- 1.0 (* 2.0 (+ (S vx) (S vy))))))))

(define (quaternion->axis/angle q)
  (let ((phi (* 2.0 (acos (first q)))))
  (cons (normalize (cdr q))
        phi)))




