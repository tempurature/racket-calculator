#lang racket/gui

;
;   util
;

(define (slice list indices)
  (map (lambda (e) (list-ref list e)) indices))
(define (repeat obj n) (map (const obj) (range n)))
(define (pos lst sym)
  (define (listcons elem obj) (if (list? obj) (cons elem obj) obj))
  (define (req child)
    (cond ((list? child)
           (ormap (lambda (x y) (listcons y (req x)))
                  child
                  (range (length child))))
          ((symbol=? child sym) '())
          (#t #f)))
  (req lst))
(define (deep-reverse lst)
  (if (list? lst) (reverse (map deep-reverse lst)) lst))
(define (pos-from-lower-right lst sym)
  (map - (list (length lst) (length (first lst)))
         (pos (deep-reverse lst) sym)
         (list 1 1)))
(define (transpose lst)
  (foldr (lambda (l ll) (map cons l ll))
         (repeat '() (length (first lst)))
         lst))
(define (findindex elem lst)
  (let ((latter (member elem lst)))
    (if (false? latter) 0
        (- (length lst) (length latter)))))

;
;   calc logic
;

(define calc-num null)
(define calc-former-num null)
(define calc-op null)
(define calc-sign null)
(set! calc-num '())
(set! calc-former-num 0)
(set! calc-sign 1)
(define (calc-append n)
  (lambda () (and (not (and (eq? n 0) (null? calc-num)))
                  (< (length calc-num) 10)
                  (set! calc-num (cons n calc-num)))))
(define (calc-eval)
  (define (eval-op op)
    (set! calc-former-num (op calc-former-num (calc-number)))
    (set! calc-op null)
    (set! calc-num '())
    (set! calc-sign 1))
  (cond ((null? calc-num) #f)
        ((eq? calc-op '+) (eval-op +))
        ((eq? calc-op '-) (eval-op -))
        ((eq? calc-op '*) (eval-op *))
        ((eq? calc-op '/) (eval-op /))
        (#t (eval-op (lambda (x y) y)))))
(define (calc-set-op op)
  (lambda () (calc-eval) (set! calc-op op)))
(define (calc-inv)
  (set! calc-sign (* calc-sign -1)))
(define (calc-add-point)
  (and (< (length calc-num) 10)
       (not (member 'point calc-num))
       (set! calc-num (cons 'point calc-num))))
(define (calc-backspace)
  (unless (null? calc-num) (set! calc-num (rest calc-num))))
(define (calc-clear)
  (set! calc-num '())
  (set! calc-former-num 0)
  (set! calc-op null)
  (set! calc-sign 1))
(define (calc-number)
  (define (point-rejected)
    (filter (lambda (x) (not (eq? x 'point))) calc-num))
  (define (magnitude-by-point)
    (let ((i (findindex 'point calc-num)))
      (if (eq? i 0) 1 (expt 0.1 i))))
  (if (null? calc-num)
      calc-former-num
      (* (foldr (lambda (x y) (+ x (* 10 y))) 0 (point-rejected))
         (magnitude-by-point)
         calc-sign)))
(define (print-calc)
  (number->string (calc-number)))

;
;   window design
;

(define frame-margin 13)
(define vertical-margin 5)
(define horizontal-margin 8)
(define text-height 50)
(define button-height 30)
(define button-width 50)

(define window-layout
  (transpose
    '(( text  text  text  text  )
      ( bs    +-    clear /     )
      ( n7    n8    n9    *     )
      ( n4    n5    n6    -     )
      ( n1    n2    n3    +     )
      ( n0    n0    point =     ))))

(struct app-rect (tag color text callback))
(define window-design
  (append
   (list (app-rect 'text 'white print-calc (const #f))
         (app-rect 'bs 'pink (const "←") calc-backspace)
         (app-rect '+- 'pink (const "±") calc-inv)
         (app-rect 'clear 'pink (const "C") calc-clear)
         (app-rect 'point 'pink (const ".") calc-add-point)
         (app-rect '= 'pink (const "=") calc-eval))
   (map app-rect
        (list 'n0 'n1 'n2 'n3 'n4 'n5 'n6 'n7 'n8 'n9)
        (repeat 'pink 10)
        (map const (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
        (map calc-append (range 10)))
   (map app-rect
        (list '/ '* '- '+)
        (repeat 'pink 4)
        (map const (list "/" "*" "-" "+"))
        (map calc-set-op (list '/ '* '- '+)))))

;
;   window logic
;

(define row-heights
  (append (list text-height)
          (repeat button-height (- (length (first window-layout)) 1))))
(define col-widths
  (repeat button-width (length (first window-layout))))

(define (sp-x i)
  (+ frame-margin
     (apply + (slice col-widths (range i)))
     (* horizontal-margin i)))
(define (sp-y j)
  (+ frame-margin
     (apply + (slice row-heights (range j)))
     (* vertical-margin j)))
(define (ep-x i) (+ (sp-x i) (list-ref col-widths i)))
(define (ep-y j) (+ (sp-y j) (list-ref row-heights j)))

(define (sp app-rect-inst)
  (map (lambda (x y) (x y))
       (list sp-x sp-y)
       (pos window-layout (app-rect-tag app-rect-inst))))
(define (ep app-rect-inst)
  (map (lambda (x y) (x y))
       (list ep-x ep-y)
       (pos-from-lower-right window-layout
                             (app-rect-tag app-rect-inst))))

(define frame-size
  (map + (list (ep-x (sub1 (length window-layout)))
               (ep-y (sub1 (length (first window-layout)))))
         (repeat frame-margin 2)))

(define (pallet tag)
  (define (? sym) (symbol=? sym tag))
  (cond ((? 'white) "white")
        ((? 'pink) "pink")))

;
;   event-callback
;

(define (event-callback event)
  (define (in? x a b) (and (<= a x) (>= b x)))
  (define (in-rect? p r) (andmap in? p (sp r) (ep r)))
  (define (when-left-up x y)
    (let ((r (ormap (lambda (r) (if (in-rect? (list x y) r) r #f))
                    window-design)))
      (unless (null? r) ((app-rect-callback r))))
    (send canvas on-paint))
  (when (symbol=? (send event get-event-type) 'left-up)
    (when-left-up (send event get-x) (send event get-y))))

;
;   gui
;

(define (make-frame x y)
  (new frame% [label "Calcurator"]
              [width x] [height y]
              [style (list 'no-resize-border)]))

(define non-client-size
  (local [(define tmpsize 300)
          (define-values (x y) (send (make-frame tmpsize tmpsize)
                                     get-client-size))]
    (map - (repeat tmpsize 2) (list x y))))

(define (make-receiver)
  (let ((p null))
    (case-lambda ((proc) (set! p proc))
                 (() p))))
(define draw (make-receiver))
(define frame
  (let ((size (map + frame-size non-client-size)))
    (make-frame (first size) (second size))))
(define app-canvas%
  (class canvas%
    (define/override (on-event event)
      (event-callback event))
    (super-new)))
(define canvas
  (local ((define (callback c d) (map draw-app-rect window-design)))
  (new app-canvas% [parent frame] [paint-callback callback])))
(define dc (send canvas get-dc))

(define (draw-app-rect r)
  (send dc set-brush (pallet (app-rect-color r)) 'solid)
  (define-values (s e) (values (sp r) (ep r)))
  (define size (map - e s))
  (send dc draw-rectangle
        (first s) (second s) (first size) (second size))
  (define-values (w h d a)
    (send dc get-text-extent ((app-rect-text r))))
  (define (div-by-2 x) (/ x 2.0))
  (define upper-left-of-text
    (map - (map div-by-2 (map + (sp r) (ep r)))
           (map div-by-2 (list w h))))
  (send dc draw-text ((app-rect-text r))
        (first upper-left-of-text) (second upper-left-of-text)))

;
;   run
;

(send frame show #t)
