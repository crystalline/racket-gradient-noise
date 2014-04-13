#lang racket

(require racket/gui/base)
(require racket/vector)
(require file/convertible)
(require racket/flonum)
(require racket/fixnum)
(require racket/unsafe/ops)

(provide make-matrix matrix-width matrix-height matrix-data matrix-ref
         matrix-set! matrix-iter-bounded matrix-iter for/matrix build/matrix
         matrix-sum vector-max vector-min matrix-max matrix-min
         conv2d zeros matrix-scale linear-map matrix-show random-matrix
         matrix-dim-eq? image-load-grayscale image-save-grayscale)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-matrix w h)
  (vector w h (make-vector (* w h))))

(define (matrix-width matrix) (unsafe-vector-ref matrix 0))
(define (matrix-height matrix) (unsafe-vector-ref matrix 1))
(define (matrix-data matrix) (unsafe-vector-ref matrix 2))

(define (row-major-index matrix i j)
  (unsafe-fx+ (unsafe-fx* i (matrix-width matrix)) j))

(define (matrix-ref matrix i j)
  (let ((data (matrix-data matrix)))
    (unsafe-vector-ref data (row-major-index matrix i j))))

(define (matrix-set! matrix i j value)
  (let ((data (matrix-data matrix)))
    (unsafe-vector-set! data (row-major-index matrix i j) value)))

(define (matrix-iter-bounded matrix proc i1 i2 j1 j2)
  (let ((result (make-matrix j2 i2)))
    (do ([i i1 (+ i 1)]) ((>= i i2))
      (do ([j j1 (+ j 1)]) ((>= j j2))
        (matrix-set! result i j (proc matrix i j))))
    result))

(define (matrix-iter matrix proc)
  (matrix-iter-bounded matrix proc 0 (matrix-height matrix) 0 (matrix-width matrix)))

;Evaluate (proc matrix i j) for each i and j
(define (for/matrix proc matrix)
  (do ([i 0 (+ i 1)]) ((>= i (matrix-height matrix)))
      (do ([j 0 (+ j 1)]) ((>= j (matrix-width matrix)))
        (proc matrix i j))))

(define (build/matrix proc w h)
  (let ((res (make-matrix w h)))
    (for/matrix (λ (m i j) (matrix-set! m i j (proc i j))) res)
    res))

(define (matrix-sum m)
  (let ((w (matrix-width m))
        (h (matrix-height m))
        (result 0.0))
    (do ([i 0 (+ i 1)]) ((>= i h))
      (do ([j 0 (+ j 1)]) ((>= j w))
        (set! result (fl+ (matrix-ref m i j) result))))
    result))

(define (vector-max v) (vector-argmax (lambda (x) x) v))
(define (vector-min v) (vector-argmin (lambda (x) x) v))

(define (matrix-max m)
    (vector-max (matrix-data m)))

(define (matrix-min m)
    (vector-min (matrix-data m)))

;Quick'n'dirty
(define (conv2d m k)
  (let* ((wm (matrix-width m))
         (hm (matrix-height m))
         (wk/2 (/ (sub1 (matrix-width k)) 2))
         (hk/2 (/ (sub1 (matrix-height k)) 2))
         (ker*point (lambda (m i j)
                      (matrix-sum
                       (matrix-iter k
                                    (lambda (l u v)
                                      (fl* (matrix-ref l u v) (matrix-ref m
                                                                          (+ i (- u hk/2))
                                                                          (+ j (- v wk/2))))))))))
    (matrix-iter-bounded m ker*point hk/2 (- hm hk/2) wk/2 (- wm wk/2))))

(define (zeros w h)
  (matrix-iter (make-matrix w h) (lambda (m i j) 0.0)))

(define (matrix-scale m x)
  (matrix-iter m (lambda (m i j) (fl* x (matrix-ref m i j)))))

(define (linear-map x a b alpha beta)
  (let* ((k (/ (- beta alpha) (- b a)))
         (offset (- alpha (* k a))))
    (+ (* k x) offset)))

(define (fl->int x) (inexact->exact (round x)))

(define (coloring mmin mmax value)
  (let* ((blue (fl->int (linear-map value mmin mmax 0.0 255.0)))
         (red (- 255 (fl->int (linear-map value mmin mmax 0.0 255.0)))))
    ;(print (list value blue))
    (make-object color% red 0 blue)))

(define (matrix-dim-eq? a b)
  (and (eq? (matrix-height a) (matrix-height b))
       (eq? (matrix-width a) (matrix-width b))))

(define (matrix-dim-eq?* . matrices)
  (andmap (lambda (m)
            (matrix-dim-eq? (first matrices) m))
          (rest matrices)))

(define (matrix-map proc . args)
  (if (matrix-dim-eq?* args)
      (build/matrix
       (lambda (i j)
         (apply proc (map (lambda (m) (matrix-ref m i j)) args)))
       (matrix-width (first args))
       (matrix-height (first args)))
      #f))

(define (matrix->positive matrix)
  (let* ((minimum (matrix-min matrix))
         (maximum (matrix-max matrix))
         (offset (- minimum)))
    (matrix-map (lambda (x) (+ x offset)) matrix)))

(define (image-load-grayscale filename)
  (let* ((imbitmap (read-bitmap filename))
         (w (send imbitmap get-width))
         (h (send imbitmap get-height))  
         (image-pixels (make-bytes (* w h 4))))
    (send imbitmap get-argb-pixels 0 0 w h image-pixels)
    (printf "[Image:~s w:~s h:~s]\n" filename w h)
    (build/matrix (λ (i j) (let*((byte-index (* 4 (+ (* i w) j)))
                                          (red (bytes-ref image-pixels (+ byte-index   1)))
                                          (green (bytes-ref image-pixels (+ byte-index 2)))
                                          (blue (bytes-ref image-pixels (+ byte-index  3))))
                                      (fl/ (fl* (exact->inexact (+ red green blue)) (exact->inexact 1/3)) 255.0))) w h)))

(define (image-save-grayscale matrix filename)
  (let* ((w (matrix-width matrix))
         (h (matrix-height matrix))
         (minimum (matrix-min matrix))
         (maximum (matrix-max matrix))
         (imbitmap (make-object bitmap% w h #f #t))
         (image-pixels (make-bytes (* w h 4))))
    (for/matrix
     (λ (matrix i j) (let* ((byte-index (* 4 (+ (* i w) j)))
                            (mval  (linear-map (matrix-ref matrix i j) minimum maximum 0.0 1.0))
                            (imval (inexact->exact (round (* 255.0 mval))))
                            (red	 1)
                            (green  2)
                            (blue	 3))
                       (bytes-set! image-pixels (+ byte-index red)   imval)
                       (bytes-set! image-pixels (+ byte-index green) imval)
                       (bytes-set! image-pixels (+ byte-index blue)  imval)
                       (bytes-set! image-pixels byte-index 255)))
     matrix)
    (send imbitmap set-argb-pixels 0 0 w h image-pixels)
    (send imbitmap save-file filename 'png)))

(define (matrix-show matrix)
  (let* ((w (matrix-width matrix))
         (h (matrix-height matrix))
         (minimum (matrix-min matrix))
         (maximum (matrix-max matrix))
         (bitmap (make-bitmap w h #f))
         (bitmap-dc (new bitmap-dc% [bitmap bitmap]))
         (main-frame (new frame%
                          [label (format "Matrix: ~sx~s" w h)]
                          [width w]
                          [height h]))
         (save-frame (new frame%
                          [parent main-frame]
                          [label "Save to file:"]))
         (input0 (new text-field%
                      [label "Enter filename.png"]
                      [parent save-frame]))
         (save-b1 (new button%
                       [parent save-frame]
                       [label "Write to file"]
                       (callback
                        (lambda (button event)
                          (image-save-grayscale matrix (send input0 get-value))
                          (send save-frame show #f)))))
         (canvas (new canvas%
                      [parent main-frame]
                      [paint-callback
                       (lambda (canvas dc)
                         (send dc set-text-foreground "blue")
                         (send dc draw-bitmap bitmap 0 0)
                         ;(send dc flush)
                         ;(flush-display)
                         )]
                      [min-width w]	 
                      [min-height h]))
         (save-b0 (new button%
                       [parent main-frame]
                       [label "Save..."]
                       ; Callback procedure for a button click:
                       (callback
                        (lambda (button event)
                          (send save-frame show #t))))))
    
    ;Note that i,j of matrix corresponds to y,x of image
    (for/matrix
     (λ (m i j)
       (send bitmap-dc set-pixel j i (coloring minimum maximum (matrix-ref matrix i j))))
     matrix)
    
    (send main-frame show #t)))

(define (random-matrix m n)
  (matrix-iter (make-matrix m n) (lambda (matrix i j) (exact->inexact (random 100)))))

;(matrix-show (random-matrix 200 200))