#lang planet dyoo/whalesong

;;;
;;; Whalesong version:
;;;    https://github.com/soegaard/whalesong/tree/patch-1
;;;
;;; Racket version:
;;;    5.2.1
;;;

(require (planet dyoo/whalesong/resource)
         (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/js))

;;;
;;;;; Triangle Solver
;;;

;;;
;;; Whalesong binding of Raphael
;;;

(define paper #f)

(define (raphael-init id width height)
  (set! paper
        (js-eval 
         (format "Raphael(~s, ~a, ~a)"
                 id width height))))

(define (raphael-rect x1 y1 x2 y2 . more)
  (case (length more)
    [(0) (call-method paper "rect" x1 y1 x2 y2)]
    [(1) (call-method paper "rect" x1 y1 x2 y2 (car more))]
    [else (error 'raphael-rect "too many arguments")]))

(define (raphael-circle x y r)
  (call-method paper "circle" x y r))

(define (raphael-ellipse x y rx ry)
  (call-method paper "ellipse" x y rx ry))

(define (raphael-image src-uri x y w h)
  (call-method paper "image" x y w h))

(define (raphael-set)
  (call-method paper "set"))

(define (raphael-push set . elems)
  (for-each (λ (e) (call-method paper "push" e)) elems))

(define (raphael-text x y str)
  (call-method paper "text" x y str))

(define (raphael-print x y text font #;size #;origin)
  (call-method paper x y text font))

(define (raphael-get-font family #;weight #;style #;strech)
  (call-method paper "getFont" family))

(define (raphael-path str) ; str in SVG path string format
  (call-method paper "path" str))

(define (raphael-line x1 y1 x2 y2)
  (raphael-path (format "M~a ~aL~a ~a" x1 y1 x2 y2)))

(define (raphael-clear) 
  (call-method paper "clear"))

(define (raphael-node c)
  (call-method c "node"))

(define (raphael-hide c)
  (call-method c "hide"))

(define (raphael-show c)
  (call-method c "show"))


(define (raphael-remove c)
  (call-method c "remove"))

(define (raphael-rotate c deg . more)
  (case (length more)
    [(0) (call-method c "rotate" deg)]
    [(1) (let ([is-absolute (car more)])
           (call-method c "rotate" deg is-absolute))]
    [(2) (let ([cx (car more)]
               [cy (cadr more)])
           ; (cx,xy) is the center
           (call-method c "rotate" deg cx cy))]))

(define (raphael-translate c dx dy)
  (call-method c "translate" dx dy))

(define (raphael-scale c xtimes ytimes . more)
  (case (length more)
    [(0) (call-method c "scale" xtimes ytimes)]
    [(2) (let ([centerx (car more)]
               [centery (cadr more)])
           (call-method c "scale" xtimes ytimes centerx centery))]
    [else (error 'raphael-scale "wrong number of arguments")]))

(define (raphael-attr c . more)
  (case (length more)
    [(2) (let* ([attribute-name (car more)]
                [attribute-value (cadr more)]
                [attribute-value (if (number? attribute-value)
                                     (number->string attribute-value)
                                     attribute-value)])
           (call-method c "attr" attribute-name attribute-value))]
    [(1) (cond
           [(string? (car more))
            ; return current attribute values
            (call-method c "attr" (car more))]
           [(list? (car more))
            (for-each (λ (p) (let ([name (car p)]
                                   [val (cadr p)])
                               (raphael-attr c name val)))
                      (car more))]
           [else (error 'raphael-attr "wrong argument type: string or list-of-two-element-lists expected")])]
    [else (error 'raphael-attr "expected 2 or 3 arguments")]))

;;;
;;; Triangle Calculations
;;;

(define-struct posn (x y) #:transparent)
(define-struct triangle (vertices sides angles names formulas))

(define (check-dependencies pname condition fmt . args)
  (unless condition
    (tp-error pname (apply format fmt args))))

(define (tp-error name fmt . args)
  (raise 
   (make-exn:fail:contract 
    (string-append (format "~a: " name) (apply format fmt args))
    (current-continuation-marks))))


; excess : R+ R+ -> R
;  compute the Euclidean excess
;  Note: If the excess is 0, then C is 90 deg.
;        If the excess is negative, then C is obtuse.
;        If the excess is positive, then C is acuse.
(define (excess a b c)
  (+ (sqr a) (sqr b) (- (sqr c))))

; polar->posn : R+ R -> (posn R R)
;  return a position with x and y coordinates
(define (polar->posn radius angle)
  (make-posn (* radius (cos angle))
             (* radius (sin angle))))

; cos-rel : R R R -> R+
;   return c^2 = a^2 + b^2 - 2ab cos(C)
(define (cos-rel a b C)
  (+ (sqr a) (sqr b) (* -2 a b (cos C))))

; sin-rel : R R R -> R
;  return the side b
(define (sin-rel A a B)
  (/ (* a (sin B)) (sin A)))

; last-angle : R R -> R
;   return pi-(A+B)
(define (last-angle A B)
  (- pi A B))

(define (radians degree)
  (* (/ degree 180.0) pi))

(define (degrees rads)
  (* (/ rads pi) 180.0))

(define (extra-formulas a b c A B C na nb nc nA nB nC)
  (append (list '(h3 "Medianer"))
          (medians a b c na nb nc)
          (list '(h3 "Højder"))
          (heights a b c A B C na nb nc nA nB nC)
          (list '(h3 "Vinkelhalveringslinjer"))
          (angle-bisectors a b c na nb nc)
          (angle-bisectors b a c nb na nc)
          (angle-bisectors c a b nc na nb)
          (list '(h3 "Areal"))
          (area a b c na nb nc)
          (list '(h3 "Omkreds"))
          (perimeter a b c na nb nc)))

(define (triangle/sss side-a side-b side-c na nb nc nA nB nC)
  ; three sides
  (define (triangle-vertices/sss a b c)
    (let ([A (acos (/ (excess b c a) (* 2 b c)))]
          [B (acos (/ (excess a c b) (* 2 a c)))]
          [C (acos (/ (excess a b c) (* 2 a b)))])
      (make-triangle
       (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))
       (list a b c)
       (list A B C)
       (list na nb nc nA nB nC)
       (append
        (list "Alle sider kendes, så cosinusrelationerne bruges til at udregne vinklerne."
              (format "$~a = ~a = ~a = ~a^{\\circ}$"
                      nA (cosrel-angle nb nc na) (cosrel-angle side-b side-c side-a) (degrees A))
              (format "$~a = ~a = ~a = ~a^{\\circ}$"
                      nB (cosrel-angle na nc nb) (cosrel-angle side-a side-c side-b) (degrees B))
              (format "$~a = ~a = ~a = ~a^{\\circ}$"
                      nC (cosrel-angle na nb nc) (cosrel-angle side-a side-b side-c) (degrees C)))
        (extra-formulas  a b c A B C na nb nc nA nB nC)))))
  (check-dependencies 'triangle/sss
                      (and (>= (+ side-a side-b) side-c) 
                           (>= (+ side-a side-c) side-b) 
                           (>= (+ side-b side-c) side-a))
                      "the given side lengths will not form a triangle ~a, ~a, and, ~a."
                      side-a side-b side-c)
  (triangle-vertices/sss side-a side-b side-c))

(define (triangle/ass angle-a side-b side-c na nb nc nA nB nC)
  ; two sides, included angle
  (define (triangle-vertices/ass A b c)
    (let* ([a^2 (cos-rel b c A)]
           [a   (sqrt a^2)]
           [B   (acos (/ (excess a c b) (* 2 a c)))]
           [C   (acos (/ (excess a b c) (* 2 a b)))])
      (make-triangle
       (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))
       (list a b c)
       (list A B C)
       (list na nb nc nA nB nC)
       (append
        (list "Der kendes to sider og mellemliggende vinkel. Først udregnes den sidste side ved hjælp af en cosinusrelation."             
              (format "$~a = ~a = ~a = ~a$"
                      na (cosrel-side nb nc nA) (cosrel-side b c (format "~a^{\\circ}" angle-a)) a)
              "Dernæst udregnes de to sidste vinkler ved hjælp af cosinusrelationer (så vinkelsummen kan bruges til kontrol)."
              (format "$~a = ~a = ~a = ~a^{\\circ}$"
                      nB (cosrel-angle na nc nb) (cosrel-angle a c b) (degrees B))
              (format "$~a = ~a = ~a = ~a^{\\circ}$"
                      nC (cosrel-angle na nb nc) (cosrel-angle a b c) (degrees C)))
        (extra-formulas  a b c A B C na nb nc nA nB nC)))))
  (triangle-vertices/ass (radians angle-a) side-b side-c))

(define (triangle/sas side-a angle-b side-c)
  ; two sides, included angle
  (define (triangle-vertices/sas a B c)
    (let ([b^2 (cos-rel a c B)])
      (check-dependencies 'triangle/sas
                          "the given side, angle, and, side will not form a triangle ~a, ~a, and, ~a."
                          side-a angle-b side-c)
      (let* ([b (sqrt b^2)]
             [A (acos (/ (excess b c a) (* 2 b c)))]
             [C (acos (/ (excess b a c) (* 2 b a)))])
        (make-triangle
         (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))
         (list a b c)
         (list A B C)
         (list "TODO")))))
  (triangle-vertices/sas side-a (radians angle-b) side-c))

(define (triangle/ssa side-a side-b angle-c)
  ; two sides, included angle
  (define (triangle-vertices/ssa a b C)
    (let ([c^2 (cos-rel a b C)])
      (check-dependencies 'triangle/ssa 
                          (positive? c^2)
                          "the given side, side, and, angle will not form a triangle ~a, ~a, and, ~a."
                          side-a side-b angle-c)
      (let*([c (sqrt c^2)]
            [A (acos (/ (excess b c a) (* 2 b c)))]
            [B (acos (/ (excess a c b) (* 2 a c)))])
        (make-triangle
         (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))
         (list a b c)
         (list A B C)
         (list "TODO")))))
  (triangle-vertices/ssa side-a side-b (radians angle-c)))


(define (triangle/aas angle-a angle-b side-c  na nb nc nA nB nC)
  ; two angles and shared side
  (define (triangle-vertices/aas A B c)
    (let* ([C (last-angle A B)]
           [b (sin-rel C c B)]
           [a (sin-rel C c A)])
      (make-triangle
       (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))
       (list a b c)
       (list A B C)
       (list na nb nc nA nB nC)
       (append
        (list "Der kendes to vinkler og deres fælles side i trekanten." 
              "Vinkelsumsformlen bruges til at udregne den sidste vinkel i trekanten."
              (format "$~a = ~a = ~a = ~a$" 
                      nC (angle-sum nA nB) (angle-sum ($degrees angle-a) ($degrees angle-b)) ($degrees (degrees C)))
              "Nu udregnes de to sidste sider ved hjælp af to sinusrelationer."
              (format "$~a = ~a = ~a = ~a$"
                      nb (sinrel-side nB nc nC) (sinrel-side ($degrees angle-b) c ($degrees (degrees C))) b)
              (format "$~a = ~a = ~a = ~a$"
                      na (sinrel-side nA nc nC) (sinrel-side ($degrees angle-a) c ($degrees (degrees C))) a))
        (extra-formulas  a b c A B C na nb nc nA nB nC)))))
  (triangle-vertices/aas (radians angle-a) (radians angle-b) side-c))


(define (triangle/aas-non-shared angle-a angle-b side-b na nb nc nA nB nC)
  ; two angles and non-shared side
  (define (triangle-vertices A B b)
    (let* ([C (last-angle A B)]
           [c (sin-rel B b C)]
           [a (sin-rel C c A)])
      (make-triangle
       (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))
       (list a b c)
       (list A B C)
       (list na nb nc nA nB nC)
       (append
        (list "Der kendes to vinkler og en ikke-fælles side i trekanten." 
              "Vinkelsumsformlen bruges til at udregne den sidste vinkel i trekanten."
              (format "$~a = ~a = ~a = ~a$" 
                      nC (angle-sum nA nB) (angle-sum ($degrees angle-a) ($degrees angle-b)) ($degrees (degrees C)))
              "Nu udregnes de to sidste sider ved hjælp af to sinusrelationer."
              (format "$~a = ~a = ~a = ~a$"
                      nc (sinrel-side nC nb nB) (sinrel-side ($degrees (degrees C)) b ($degrees (degrees B))) c)
              (format "$~a = ~a = ~a = ~a$"
                      na (sinrel-side nA nc nC) (sinrel-side ($degrees angle-a) c ($degrees (degrees C))) a))
        (extra-formulas  a b c A B C na nb nc nA nB nC)))))
  (triangle-vertices (radians angle-a) (radians angle-b) side-b))

(define (triangle/two-sides-non-included-angle angle-a side-a side-c na nb nc nA nB nC)
  ; two sides and non-included angle
  (define (triangle-vertices A a c)
    (let* ([C1 (asin (/ (* c (sin A)) a))]
           [C2 (- pi C1)]
           [B1 (- pi A C1)]
           [B2 (- pi A C2)]
           [b1 (/ (* c (sin B1)) (sin C1))]
           [b2 (/ (* c (sin B2)) (sin C2))])
      (make-triangle
       (list (make-posn 0 0) (make-posn c 0) (polar->posn b1 A))
       (list a b1 c)
       (list A B1 C1)
       (list na nb nc nA nB nC)
       (append
        (list "Der kendes to sider og en ikke-mellemliggende vinkel i trekanten. Dermed er vi i det dobbelttydige tilfælde."
              (format "Først udregnes de to mulige vinkler for $~a$. Den spidse mulighed benævnes $~a_1$ og den stumpe $~a_2$."
                      nC nC nC)
              '(em (string-append "Kun den ene trekant tegnes i resultat-feltet. For at tegne den sidste mulighed skal du indtaste de 3 sider"
                                  " i mulighed to og genberegne."))
              (format "$~a_1 = ~a = ~a = ~a$" 
                      nC (sinrel-angle-1 na nC nc) (sinrel-angle-1 c ($degrees (degrees A)) a) ($degrees (degrees C1)))
              (format "$~a_2 = 180^{\\circ}-~a_2 = 180^{\\circ}-~a = ~a$" 
                      nC nC ($degrees (degrees C1)) ($degrees (degrees C2)))
              '(h2 "Resultater for første mulighed")
              (format "Vi ser på tilfældet, hvor vinkel $~a$ er spids." nC)
              "Nu udregnes den sidste vinkel ved hjælp af vinkelsumsformlen."
              (format "$~a_1 = 180^{\\circ} - ~a - ~a_1 = 180^{\\circ} - ~a - ~a_1 = ~a$"
                      nB nA nC ($degrees angle-a) ($degrees (degrees C1)) ($degrees (degrees B1)))
              "Nu udregnes den sidste side."
              (format "$~a_1 = ~a = ~a = ~a$"
                      nb 
                      (sinrel-side (format "~a_1" nB) nc (format "~a_1" nC))
                      (sinrel-side ($degrees (degrees B1)) c ($degrees (degrees C1)))
                      b1)
              '(h2 "Resultater for anden mulighed")
              (format "Vi ser på tilfældet, hvor vinkel $~a$ er stump." nC)
              (format "$~a_2 = 180^{\\circ}-~a_2 = 180^{\\circ}-~a = ~a$" 
                      nC nC ($degrees (degrees C1)) ($degrees (degrees C2)))
              "Nu udregnes den sidste vinkel ved hjælp af vinkelsumsformlen."
              (format "$~a_2 = 180^{\\circ} - ~a - ~a_2 = 180^{\\circ} - ~a - ~a_1 = ~a$"
                      nB nA nC ($degrees angle-a) ($degrees (degrees C2)) ($degrees (degrees B2)))
              "Nu udregnes den sidste side."              
              (format "$~a_2 = ~a = ~a = ~a$"
                      nb 
                      (sinrel-side (format "~a_2" nB) nc (format "~a_2" nC))
                      (sinrel-side ($degrees (degrees B2)) c ($degrees (degrees C2)))
                      b2))
        (list '(h2 "Første mulighed"))
        (let ([na (format "{~a_1}" na)]
              [nb (format "{~a_1}" nb)]
              [nc (format "{~a_1}" nc)]
              [nA (format "{~a_1}" nA)]
              [nB (format "{~a_1}" nB)]
              [nC (format "{~a_1}" nC)])
          (extra-formulas  a b1 c A B1 C1 na nb nc nA nB nC))
        (list '(h2 "Anden mulighed"))
        (let ([na (format "{~a_2}" na)]
              [nb (format "{~a_2}" nb)]
              [nc (format "{~a_2}" nc)]
              [nA (format "{~a_2}" nA)]
              [nB (format "{~a_2}" nB)]
              [nC (format "{~a_2}" nC)])
          (extra-formulas  a b2 c A B2 C2 na nb nc nA nB nC))))))
  (triangle-vertices (radians angle-a) side-a side-c))

(define (triangle/asa angle-a side-b angle-c)
  ; two angles and shared side
  (define (triangle-vertices/asa A b C)
    (let* ([B (last-angle A C)]
           [c (sin-rel B b C)]
           [a (sin-rel B b A)])
      (make-triangle
       (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))
       (list a b c)
       (list A B C)
       (list "TODO"))))
  (triangle-vertices/asa (radians angle-a) side-b (radians angle-c)))

(define (triangle/saa side-a angle-b angle-c)
  ; two angles and shared side
  (define (triangle-vertices/saa a B C)
    (let* ([A (last-angle B C)]
           [b (sin-rel A a B)]
           [c (sin-rel A a C)])
      (make-triangle
       (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))
       (list a b c)
       (list A B C)
       (list "TODO"))))
  (triangle-vertices/saa side-a (radians angle-b) (radians angle-c)))

;;;
;;; GUI
;;;

(define WIDTH 400)
(define HEIGHT 400)

(define XMIN  -1.0)
(define XMAX   1.0)
(define YMIN  -1.0)
(define YMAX   1.0)

(define screen-x
  (let ([dx (- XMAX XMIN)])
    (lambda (x)
      (let* ([x (* 0.75 x)]
             [x (max x XMIN)]
             [x (min x XMAX)])
        (/ (* (- x XMIN) WIDTH) dx)))))

(define screen-y
  (let ([dy (- YMAX YMIN)])
    (lambda (y)
      (let* ([y (* 0.75 y)]
             [y (max y YMIN)]
             [y (min y XMAX)])
        (/ (* (- (- y) YMIN) HEIGHT) dy)))))


(define (midpoint-posn p1 p2)
  (make-posn (/ (+ (posn-x p1) (posn-x p2)) 2)
             (/ (+ (posn-y p1) (posn-y p2)) 2)))

(require (for-syntax racket))

(define-syntax (with-paper stx)
  (syntax-case stx ()
    [(_ paper-expr expr ...)
     #'(let ([new-paper paper-expr]
             [old-paper paper])
         (set! paper new-paper)
         (let ([v (begin expr ...)])
           (set! paper old-paper)
           v))]))

(define model-paper #f)

(define raphael-focus
  (let ([old-focus #f])
    (lambda (n)
      (let* ([vs (triangle-vertices model-triangle)]
             [p0 (first vs)]
             [p1 (second vs)]
             [p2 (third vs)]
             [p (cond
                  [(<= 0 n 2) (list-ref vs n)]
                  [(= n 3) (midpoint-posn p1 p2)]
                  [(= n 4) (midpoint-posn p0 p2)]
                  [(= n 5) (midpoint-posn p0 p1)])]
             [x (screen-x (posn-x p))]
             [y (screen-y (posn-y p))])
        (when old-focus
          (raphael-remove old-focus))
        (with-paper 
         model-paper
         (let ([c (raphael-circle x y 40)])
           (raphael-attr c "stroke" color-triangle-stroke)
           (raphael-attr c "stroke-opacity" "0.50")
           (raphael-attr c "fill" "white")
           (raphael-attr c "fill-opacity" "0.50")
           (set! old-focus c)))))))

(define color-triangle-fill   "#F7CE9E") ; salmon
(define color-triangle-stroke "#CE2D6A") ; dark salmon 

; result colors
(define color-result-fill "#BBE1EC") ; light blue
(define color-result-stroke "#3294D2") ; darker blue


  
(define stroke-width 3)

(define (raphael-open-sans-text x y text)
  (let* ([t (raphael-text x y text)]
         [t (raphael-attr t "font-size" "20")]
         [t (raphael-attr t "font-family" "Open Sans")])
    t))
  
(define (raphael-triangle x1 y1 x2 y2 x3 y3 stroke fill na nb nc nA nB nC)
  (print-message) ; clear errors
  (let* ([cx (/ (+ x1 x2 x3) 3.0)]
         [cy (/ (+ y1 y2 y3) 3.0)]
         ; adjust-x and adjust-y
         [ax (λ (x) (+ x (* 0.1 (- x cx))))]
         [ay (λ (y) (+ y (* 0.1 (- y cy))))]
         
         [ax1 (screen-x (ax x1))]
         [ax2 (screen-x (ax x2))]
         [ax3 (screen-x (ax x3))]
         [ay1 (screen-y (ay y1))]
         [ay2 (screen-y (ay y2))]
         [ay3 (screen-y (ay y3))]
         
         [x1 (screen-x x1)]
         [x2 (screen-x x2)]
         [x3 (screen-x x3)]
         [y1 (screen-y y1)]
         [y2 (screen-y y2)]
         [y3 (screen-y y3)])
    (let ([outline 
           (raphael-path 
            (format "M~a ~aL~a ~aL~a ~aZ" 
                    x1 y1 x2 y2 x3 y3))])
      (raphael-attr outline "stroke" stroke)
      (raphael-attr outline "fill" fill)
      (raphael-attr outline "stroke-width" stroke-width)
      (cons outline
            (list 
             (raphael-open-sans-text ax1 ay1 nA)
             (raphael-open-sans-text ax2 ay2 nB)
             (raphael-open-sans-text ax3 ay3 nC)
             (raphael-open-sans-text (/ (+ ax2 ax3) 2) (/ (+ ay2 ay3) 2) na)
             (raphael-open-sans-text (/ (+ ax1 ax3) 2) (/ (+ ay1 ay3) 2) nb)
             (raphael-open-sans-text (/ (+ x1 x2) 2) (+ 25 (/ (+ ay1 ay2) 2)) nc))))))

(define print-message
  (let ([old #f] [n 0])
    (lambda texts
      (when old
        (for-each raphael-remove old))
      (set! n 0)
      (set! old (map (lambda (text)
                       (set! n (add1 n))
                       (raphael-attr
                        (raphael-attr (raphael-open-sans-text 20 (+ (* 20 n) 100) text)
                                      "text-anchor" "start")
                        "color" "red"))
                     texts))
      old)))

(define color-background "white")

(define (start-raphael)
  ; model
  (raphael-init "raphael_model" WIDTH HEIGHT)
  (set! model-paper paper)
  (let ([r (raphael-rect 0 0 WIDTH HEIGHT)])
    (raphael-attr r "stroke" "black")
    (raphael-attr r "fill" color-background))
  (raphael-open-sans-text 50 50 "Skitse")
  (draw-triangle model-triangle 'model)
  ; result
  (raphael-init "raphael_result" WIDTH HEIGHT)
  (let ([r (raphael-rect 0 0 WIDTH HEIGHT)])
    (raphael-attr r "stroke" "black")
    (raphael-attr r "fill" color-background)
    (raphael-open-sans-text 50 50 "Resultat")
    r))

(define (scale-triangle triangle)
  (let* ([vs (triangle-vertices triangle)]
         [x1 (posn-x (first vs))]
         [x2 (posn-x (second vs))]
         [x3 (posn-x (third vs))]
         [y1 (posn-y (first vs))]
         [y2 (posn-y (second vs))]
         [y3 (posn-y (third vs))]
         [cx (/ (+ x1 x2 x3) 3)]
         [cy (/ (+ y1 y2 y3) 3)]
         [x1 (- x1 cx)]
         [x2 (- x2 cx)]
         [x3 (- x3 cx)]
         [y1 (- y1 cy)] 
         [y2 (- y2 cy)]
         [y3 (- y3 cy)]
         [x-max (max x1 x2 x3)]
         [y-max (max y1 y2 y3)]
         [m     (max x-max y-max)]
         [x1 (/ x1 m)]
         [x2 (/ x2 m)]
         [x3 (/ x3 m)]
         [y1 (/ y1 m)]
         [y2 (/ y2 m)]
         [y3 (/ y3 m)])
    (make-triangle 
     (list (make-posn x1 y1) (make-posn x2 y2) (make-posn x3 y3))
     (triangle-sides triangle)
     (triangle-angles triangle)
     (triangle-names triangle)
     (triangle-formulas triangle))))

(define (draw-triangle triangle type)
  (let* ([vs (triangle-vertices triangle)]
         [x1 (posn-x (first vs))]
         [x2 (posn-x (second vs))]
         [x3 (posn-x (third vs))]
         [y1 (posn-y (first vs))]
         [y2 (posn-y (second vs))]
         [y3 (posn-y (third vs))]
         [ns (triangle-names triangle)])
    (let-values ([(stroke fill)
                  (case type
                    [(model) (values color-triangle-stroke color-triangle-fill)]
                    [(result) (values color-result-stroke color-result-fill)])])
      (raphael-triangle x1 y1 x2 y2 x3 y3 stroke fill
                        (list-ref ns 0) (list-ref ns 1) (list-ref ns 2)
                        (list-ref ns 3) (list-ref ns 4) (list-ref ns 5)))))

(define-struct world (initialized? triangle result-formulas old-triangle-representation) #:mutable)

(define (update-result-formulas v formulas)
  (let* ([v (view-focus v "result")]
         [v (update-view-text v "")])    
    (foldl (λ (f v)              
             (view-append-child v (xexp->dom `(p ,f))))
           v formulas)))

;; tick: world view -> world
(define (tick w v)
  (let ([initialized?       (world-initialized? w)]
        [triangle           (world-triangle w)]
        [old-representation (world-old-triangle-representation w)])
    (cond
      [(not initialized?)
       (start-raphael)     
       (make-world #t #f #f #f)]
      [triangle
       (when old-representation
         (for-each raphael-remove old-representation))
       (make-world initialized? #f (triangle-formulas triangle) (draw-triangle triangle 'result))]
      [else
       w])))

;; draw: world view -> view
(define (draw w v)
  (let ([r (world-result-formulas w)])
    (if r
        (begin0
          (update-result-formulas v r)
          (set-world-result-formulas! w #f))
        v)))

(define ($degrees d)
  (format "~a^{\\circ}" d))

(define (cosrel-side a b C)
  (let ([dot (if (and (number? a) (number? b))
                 "\\cdot" "")])
    (format "\\sqrt{~a^2 + ~a^2 - 2~a~a~a~a\\cos(~a)}"
            a b dot a dot b C )))

(define ($cosrel-side$ c a b C)
  (format "$~a = ~a$" c (cosrel-side a b C)))

(define (cosrel-angle a b c)
  (let ([dot (if (and (number? b) (number? c))
                 "\\cdot" "")])
    (format "\\cos^{-1}\\left(\\frac{~a^2+~a^2-~a^2}{2~a~a~a~a}\\right)"
            a b c dot a dot b)))

(define ($cosrel-angle$ C a b c)
  (format "$~a = ~a$" C (cosrel-angle a b c)))

(define (sinrel-side A b B)
  (format "\\frac{~a}{\\sin(~a)}\\cdot\\sin(~a)"
          b B A))

(define ($sinrel-side$ a A b B)
  (format "$~a = ~a$" a (sinrel-side b B A)))

(define (sinrel-angle-1 a B b)
  (format "\\sin^{-1}\\left(\\frac{\\sin(~a)}{~a}\\cdot ~a\\right)" B b a ))

(define (sinrel-angle-2 a B b)
  (format "180^{\\circ} - \\sin^{-1}\\left(\\frac{\\sin(~a)}{~a}\\cdot ~a\\right)" B b a ))

(define ($sinrel-angle-1$ A a B b)
  (format  "$~a = ~a$" A (sinrel-angle-1 B b a)))

(define ($sinrel-angle-2$ A a B b)
  (format  "$~a = ~a$" A (sinrel-angle-2 B b a)))


(define (calculate-median c a b)
  (sqrt (/ (+ (* 2 a a) (* 2 b b) (- (* c c))) 4)))

(define (median a b c)
  (let ([dot (if (or (number? a) (number? b) (number? c))
                 "\\cdot" "")])
    (format "\\sqrt{\\frac{2~a~a^2+2~a~a^2-~a^2}{4}}" dot b dot c a)))

(define ($median$ a b c)
  (format "$m_~a = ~a$" a (median a b c)))

(define (medians a b c na nb nc)
  (list (format "$m_~a = ~a = ~a = ~a$"
                na (median na nb nc) (median a b c) (calculate-median a b c))
        (format "$m_~a = ~a = ~a = ~a$"
                nb (median nb na nc) (median b a c) (calculate-median b a c))
        (format "$m_~a = ~a = ~a = ~a$"
                nc (median nc na nb) (median c a b) (calculate-median c a b))))

(define (calculate-height b C)
  (* b (sin C)))

(define (heights a b c A B C na nb nc nA nB nC)
  (list (format "$h_~a = ~a = ~a = ~a$"
                na (first (height nb nc nB nC)) (first (height b c B ($degrees (degrees C)))) (calculate-height b C))
        (format "$h_~a = ~a = ~a = ~a$"
                nb (first (height na nc nA nC)) (first (height a c A ($degrees (degrees C)))) (calculate-height a C))
        (format "$h_~a = ~a = ~a = ~a$"
                nc (first (height na nb nA nB)) (first (height a b A ($degrees (degrees B)))) (calculate-height a B))))


(define (angle-sum B C)
  (format "180^{\\circ} - ~a - ~a" B C))

(define ($angle-sum$ A B C)
  (format "~a = ~a" A (angle-sum B C)))

(define (height b c B C)
  (list (format "~a\\cdot\\sin(~a)" b C)
        (format "~a\\cdot\\sin(~a)" c B)))

(define ($height$ a b c B C)
  (let* ([hs (height b c B C)]
         [h1 (format "h_~a = ~a" a (first hs))]
         [h2 (format "h_~a = ~a" a (second hs))])
    (format "$~a \\textrm{ eller } ~a$" h1 h2)))

(define (area a b c na nb nc)
  (let ([s (/ (+ a b c) 2)])
    (list "Herons formel anvendes til at udregne arealet."
          (format "$s = \\textrm{den halve omkreds} = \\frac{~a+~a+~a}{2} = \\frac{~a+~a+~a}{2} = ~a$"
                  na nb nc a b c (/ (+ a b c) 2))
          (format "$T = \\textrm{Areal} = \\sqrt{s(s-~a)(s-~a)(s-~a)} = \\sqrt{s\\cdot(s-~a)\\cdot(s-~a)\\cdot(s-~a)}=~a$"
                  na nb nc a b c (sqrt (* s (- s a) (- s b) (- s c)))))))

(define (perimeter a b c na nb nc)
  (list (format "$\\textrm{Omkreds} = ~a+~a+~a = ~a+~a+~a = ~a$"
                na nb nc a b c (+ a b c))))

; ta = sqrt[bc(1-a2/[b+c]2)]
(define (angle-bisectors a b c na nb nc)
  (list (format (string-append"$v_~a = \\sqrt{~a~a\\left(1-\\frac{~a^2}{(~a+~a)^2}\\right)} = "
                              "\\sqrt{~a\\cdot ~a\\cdot\\left(1-\\frac{~a^2}{(~a+~a)^2}\\right)} = ~a$")
                na nb nc na nb nc   b c  a b c   (sqrt (* b c (- 1 (/ (* a a) (sqr (+ b c)))))))))


(define (make-initial-view)
  (let* ([view (->view
                (xexp->dom 
                 `(html 
                   (head (title "Trekantsberegner"))
                   (body 
                    (h1 "Trekantsberegner")
                    (h3 "Jens Axel Søgaard, 2012")
                    (h2 "Indtast 3 sider/vinkler")
                    (table 
                     (tr (td (div
                              (h3 "Vinkler")
                              (div (@ (id "angles") (style "display: inline"))
                                   (div "$A=$" (input (@ (type "text") (id "angleA"))))
                                   (div "$B=$" (input (@ (type "text") (id "angleB"))))
                                   (div "$C=$" (input (@ (type "text") (id "angleC")))))
                              (h3 "Sider")
                              (div (@ (id "sides") (style "display: inline"))
                                   (div "$a=$" (input (@ (type "text") (id "sideA"))))
                                   (div "$b=$" (input (@ (type "text") (id "sideB"))))
                                   (div "$c=$" (input (@ (type "text") (id "sideC")))))
                              (div (input (@ (type "button") (id "calculate") (value "Beregn"))))))
                         (td "")
                         (td (div (@ (id "raphael_model"))))
                         (td (div (@ (id "raphael_result"))))))
                    (h2 "Resultater")
                    (div (@ (id "result")) "Ingen resultater endnu. Indtast 3 stykker og klik beregn.")
                    (hr)
                    (h2 "Formelsamling")
                    (h3 "Cosinusrelationer")
                    (table 
                     (tr (td ,($cosrel-side$ "a" "b" "c" "A")) (td 'nbsp 'nbsp ) (td ,($cosrel-angle$ "A" "b" "c" "a")))
                     (tr (td ,($cosrel-side$ "b" "a" "c" "B")) (td 'nbsp 'nbsp ) (td ,($cosrel-angle$ "B" "a" "c" "b")))
                     (tr (td ,($cosrel-side$ "c" "a" "b" "C")) (td 'nbsp 'nbsp ) (td ,($cosrel-angle$ "C" "a" "b" "c"))))
                    (h3 "Sinusrelationer")
                    (table 
                     ,@(apply append
                              (apply append
                                     (let* ([perms (list (list "a" "A" "b" "B" "c" "C") 
                                                         (list "b" "B" "a" "A" "c" "C")
                                                         (list "c" "C" "a" "A" "b" "B"))]
                                            [sides
                                             (map (lambda (l)
                                                    (apply (lambda (a A b B c C)
                                                             `((tr (td ,($sinrel-side$ a A b B)))
                                                               (tr (td ,($sinrel-side$ a A c C)))
                                                               (tr (td 'nbsp))))
                                                           l))
                                                  perms)]                                         
                                            [angles
                                             (map (lambda (l)
                                                    (apply (lambda (a A b B c C)
                                                             `((tr (td ,($sinrel-angle-1$ A a B b)) 
                                                                   (td "eller")
                                                                   (td ,($sinrel-angle-2$ A a B b)))
                                                               (tr (td ,($sinrel-angle-1$ A a C c))
                                                                   (td "eller")
                                                                   (td ,($sinrel-angle-2$ A a C c)))                                                      
                                                               (tr (td 'nbsp) (td 'nbsp) (td 'nbsp))))
                                                           l))
                                                  perms)])
                                       (list sides angles)))))                  
                    (h3 "Medianer")
                    (table 
                     (tr (td ,($median$ "a" "b" "c")) )
                     (tr (td ,($median$ "b" "a" "c")) )
                     (tr (td ,($median$ "c" "a" "c")) ))
                    (h3 "Højder")
                    (table 
                     (tr (td ,($height$ "a" "b" "c" "B" "C")) )
                     (tr (td ,($height$ "b" "a" "c" "A" "C")) )
                     (tr (td ,($height$ "c" "a" "b" "A" "B")) ))
                    (hr)
                    (h2 "Mail jensaxel@soegaard.net med kommentarer, spørgsmål, fejlmeldinger eller andet.")))))]
         [view (view-bind (view-focus view "calculate") "click" on-click)]
         [view (view-bind (view-focus view "angleA") "focusin" (make-on-focusin 0))]
         [view (view-bind (view-focus view "angleB") "focusin" (make-on-focusin 1))]
         [view (view-bind (view-focus view "angleC") "focusin" (make-on-focusin 2))]
         [view (view-bind (view-focus view "sideA")  "focusin" (make-on-focusin 3))]
         [view (view-bind (view-focus view "sideB")  "focusin" (make-on-focusin 4))]
         [view (view-bind (view-focus view "sideC")  "focusin" (make-on-focusin 5))])
    view))

(define (values-map f l)
  (apply values (map f l)))

(define (maybe-make-triangle A B C a b c)
  (let-values ([(A B C a b c)
                (values-map (lambda (s) (string->number (string-replace-char s #\, #\.)))
                            (list A B C a b c))])
    (cond
      [(< (length (filter (λ (x) x) (list A B C a b c))) 3)
       (print-message "Ikke nok oplysninger")
       #f]      
      [(> (length (filter (λ (x) x) (list A B C a b c))) 3)
       (print-message "Du har indtastet for" 
                      "mange oplysninger. " 
                      "Slet nogle igen.")
       #f]
      [(or (and A (not (positive? A))) 
           (and B (not (positive? B)))
           (and C (not (positive? C))))
       (print-message "Vinkler skal være positive.")
       #f]
      [(or (and a (not (positive? a))) 
           (and b (not (positive? b)))
           (and c (not (positive? c))))
       (print-message "Sidelængder skal være positive.")
       #f]
      [(and A B C)
       (print-message "Du har indtastet tre vinkler." 
                      "Sidelængderne kan ikke udregnes."
                      "Indtast mindst en side.")
       #f]
      [(and a b c (not (and (< c (+ a b)) (< a (+ b c)) (< b (+ a c)))))
       (print-message "Der findes ingen trekanter"  
                      "med de 3 indtastede sider.")
       #f]      
      ; 3 sides
      [(and a b c (not A) (not B) (not C)) (scale-triangle (triangle/sss a b c 'a 'b 'c 'A 'B 'C))]
      ; 2 sides and included angle
      [(and A b c (not a) (not B) (not C)) (scale-triangle (triangle/ass A b c 'a 'b 'c 'A 'B 'C))]
      [(and a B c (not A) (not b) (not C)) (scale-triangle (triangle/ass B a c 'b 'a 'c 'B 'A 'C))]
      [(and a b C (not A) (not B) (not c)) (scale-triangle (triangle/ass C a b 'c 'a 'b 'C 'A 'B))]
      ; 2 angles and shared side
      [(and B C a (not A) (not b) (not c)) (scale-triangle (triangle/aas B C a 'b 'c 'a 'B 'C 'A))] 
      [(and A C b (not a) (not B) (not c)) (scale-triangle (triangle/aas A C b 'a 'c 'b 'A 'C 'B))]
      [(and A B c (not a) (not b) (not C)) (scale-triangle (triangle/aas A B c 'a 'b 'c 'A 'B 'C))]
      ; 2 angles and non-shared side
      [(and A B a (not b) (not c) (not C)) (scale-triangle (triangle/aas-non-shared B A a 'b 'a 'c 'B 'A 'C))]
      [(and A B b (not a) (not c) (not C)) (scale-triangle (triangle/aas-non-shared A B b 'a 'b 'c 'A 'B 'C))]
      [(and A C a (not B) (not b) (not c)) (scale-triangle (triangle/aas-non-shared C A a 'c 'a 'b 'C 'A 'B))]
      [(and A C c (not a) (not B) (not b)) (scale-triangle (triangle/aas-non-shared A C c 'a 'c 'b 'A 'C 'B))]
      [(and B C b (not A) (not a) (not c)) (scale-triangle (triangle/aas-non-shared C B b 'c 'b 'a 'C 'B 'A))]
      [(and B C c (not b) (not A) (not a)) (scale-triangle (triangle/aas-non-shared B C c 'b 'c 'a 'B 'C 'A))]
      ; 2 sides, non-included angle
      [(and A a b (not C) (not c) (not B)) (scale-triangle (triangle/two-sides-non-included-angle A a b 'a 'c 'b 'A 'C 'B))]
      [(and B b c (not A) (not a) (not C)) (scale-triangle (triangle/two-sides-non-included-angle B b c 'b 'a 'c 'B 'A 'C))]
      [(and C c a (not B) (not b) (not A)) (scale-triangle (triangle/two-sides-non-included-angle C c a 'c 'b 'a 'C 'B 'A))]

      [(and A a c (not B) (not b) (not C)) (scale-triangle (triangle/two-sides-non-included-angle A a c 'a 'b 'c 'A 'B 'C))]
      [(and B b a (not C) (not c) (not A)) (scale-triangle (triangle/two-sides-non-included-angle B b a 'b 'c 'a 'B 'C 'A))]
      [(and C c b (not A) (not a) (not B)) (scale-triangle (triangle/two-sides-non-included-angle C c b 'c 'a 'b 'C 'A 'B))]
      
      [else #f])))

;; on-click: world view -> world
(define (on-click w v)
  (make-world (world-initialized? w)
              (maybe-make-triangle
               (view-form-value (view-focus v "angleA"))
               (view-form-value (view-focus v "angleB"))
               (view-form-value (view-focus v "angleC"))
               (view-form-value (view-focus v "sideA"))
               (view-form-value (view-focus v "sideB"))
               (view-form-value (view-focus v "sideC")))
              #f
              (world-old-triangle-representation w)))

;; on-focusin: world view -> world
(define (make-on-focusin n)
  (lambda (w v)
    (raphael-focus n)
    w))

(define (string-replace-char str c1 c2)
  (list->string
   (map (lambda (c) (if (eqv? c c1) c2 c))
        (string->list str))))

(define model-triangle (scale-triangle (triangle/sss 60 40 90 'a 'b 'c 'A 'B 'C)))

(big-bang 
 (make-world #f #f #f #f)
 (initial-view (make-initial-view))
 (on-tick tick 1)
 (to-draw draw))
