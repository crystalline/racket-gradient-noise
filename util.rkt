#lang racket

(require racket/require)
(require compatibility/defmacro)

(provide @ tree-subst tree-match-transform o I every?
         random-bitvector)

(provide pushf
         popf
         incf
         +=
         *=
         -=
         /=)

(provide vector-index
         in-list?
         dlog2)

(provide while
         loop
         for-loop        
         is
         iso
         pr)

(define I identity)

(define @ sequence-ref)

(define (o F G)
  (lambda x (F (G x))))

(define (tree-subst tree old new)
  (cond ((equal? tree old) new)
        ((list? tree) (map (lambda (sub-tree) (tree-subst sub-tree old new)) tree))
        (else tree)))

(define (tree-subst* tree plist)
  (if (null? plist)
      tree
      (tree-subst (tree-subst* tree (cdr plist))
                  (caar plist)
                  (cadar plist))))

(define-for-syntax (tree-subst tree old new)
  (cond ((equal? tree old) new)
        ((list? tree) (map (lambda (sub-tree) (tree-subst sub-tree old new)) tree))
        (else tree)))

(define-for-syntax (tree-subst* tree plist)
  (if (null? plist)
      tree
      (tree-subst (tree-subst* tree (cdr plist))
                  (caar plist)
                  (cadar plist))))

(define (tree-match-transform tree predicate transform)
  (cond ((predicate tree) (transform tree))
        ((list? tree) (map (lambda (sub-tree)
                             (tree-match-transform sub-tree predicate transform))
                           tree))
        (else tree)))

(define (every? pred lst)
  (if (null? lst)
      #t
      (and (pred (car lst))
           (every? pred (cdr lst)))))

(define (random-bitvector N)
  (if (eq? N 0)
      '()
      (cons (random 2)
            (random-bitvector (- N 1)))))

(define-macro (pushf dst value)
  (list 'set! dst (list 'cons value dst)))

(define-macro (popf dst)
  (let ((t (gensym)))
    (list 'let (list (list t (list 'car dst)))
          (list 'set! dst (list 'cdr dst))
          t)))

(define-macro (incf dst value)
  (list 'set! dst (list '+ value dst)))

(define-macro (+= dst value)
  `(set! ,dst (+ ,value ,dst)))
(define-macro (*= dst value)
  `(set! ,dst (* ,value ,dst)))
(define-macro (-= dst value)
  `(set! ,dst (- ,dst ,value)))
(define-macro (/= dst value)
  `(set! ,dst (/ ,dst ,value)))

(define (vector-index proc v)
  (define (search v i)
    (if (eq? (vector-length v) i)
        -1
        (if (proc (vector-ref v i))
            i
            (search v (+ i 1)))))
  (search v 0))


(define (in-list? elem lst)
  (if (null? lst)
      #f
      (if (equal? elem (first lst))
          #t
          (in-list? elem (rest lst)))))

(define (dlog2 n)
  (inexact->exact
   (round
    (/ (log n)
       (log 2.0)))))


;Arc-like primitives

(define-macro (pr expr)
  `(print ,expr))

(define-macro (is A B)
  `(eq? ,A ,B))

(define-macro (iso A B)
  `(equal? ,A ,B))

;(define-macro (while expr . body)
;  (let ((fname (gensym))
;        (break-flag (gensym)))
;    `(letrec ((,break-flag #f)
;              (,fname
;               (lambda ()
;                 (if (and ,expr (not ,break-flag))
;                     (begin
;                       ,@body
;                       (,fname))
;                     #t)))
;              (break (lambda ()
;                       (set! ,break-flag #t)
;                       (,fname))))
;       (,fname))))

;C-like  while operator
;Complete with break and continue operators
(define-macro (while expr . body)
  (let ((fname (gensym))
        (break-flag (gensym)))
    `(letrec ((,break-flag #t)
              (,fname
               (lambda ()
                 (if (and ,expr ,break-flag)
                     (begin
                       ,@(tree-subst* body
                                      `(((break)
                                         (begin
                                           (set! ,break-flag #f)
                                           (,fname)))
                                       ((continue)
                                        (,fname))))
                       (,fname))
                     (if (not ,break-flag)
                         #f #t)))))
       (,fname))))

;C-like for with params = (index-name start-value iter-predicate increment-expr)
(define-macro (for-loop params . body)
  `(let ((,(list-ref params 0) ,(list-ref params 1)))
     (while ,(list-ref params 2)
            (begin
              ,@body
              ,(list-ref params 3)))))

;Equivalent to arc's for

;Old definition via racket's built in for
;(define-macro (loop name lower upper . expr)
;  `(for ((,name (in-range ,lower (+ 1 ,upper))))
;     ,@expr))

(define-macro (loop name lower upper . expr)
  `(for-loop ,(list name lower `(<= ,name ,upper) `(incf ,name 1))
             ,@expr))

;(loop i 0 10 (print i))
;=>012345678910#t

;(loop i 0 10
;      (when (> i 5) (break))
;      (print i))
;=>0123456#f

;Future: macro for declaring variables in groups
;for imperative programming
;(define-macro (vars varlist . body)

;Tests

;(define a '())
;(pushf a 10)
;(pushf a 20)
;a
;
;(popf a)
;(popf a)
;
;(define b 11)
;(+= b 5)
;b
;(*= b 10)
;b
;(-= b 60)
;b
;(/= b 10)
;b
