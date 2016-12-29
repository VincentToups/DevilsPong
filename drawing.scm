(define (draw-normal-sprite x y w0 h0 t x1 y1 x2 y2 r . colors)
  (apply glCoreTextureDraw 
	 (if (negative? w0)
	     (+ x (abs w0))
	     x)
	 (if (negative? h0)
	     (+ y (abs h0))
	     y)
	 w0 h0 t x1 y1 x2 y2 r colors))

(define (sprite! s)
  (append s (list 0.)))

(define (dynamic! f)
  (list f))

(define (dynamic-string! f)
  (dynamic!
   (let ((s (string! "")))
     (lambda ()
       (set-car! s (f))
       s))))

(define (string! s #!key 
		  (font ascii_18.fnt)
		  (color White))
  (list s font color))

(define (left-string! s  #!key 
		      (w #f) 
		      (h #f)
		      (font ascii_18.fnt)
		      (color White))
  (if (not w)
      (set! w (flo (glgui:stringwidth s font))))
  (if (not h)
      (set! h (car (map flo (glgui:stringheight (string-append s "|") font)))))
  (list left-aligned: w h s font color))

(define (right-string! s  #!key 
		       (w #f) 
		       (h #f)
		       (font ascii_18.fnt)
		       (color White))
  (if (not w)
      (set! w (flo (glgui:stringwidth s font))))
  (if (not h)
      (set! h (car (map flo (glgui:stringheight (string-append s "|") font)))))
  (list right-aligned: w h s font color))

(define (aligned-string-width as)
  (nth as 1))

(define (center-string! s  #!key 
			(w #f) 
			(h #f)
			(font ascii_18.fnt)
			(color White))
  (if (not w)
      (set! w (flo (glgui:stringwidth s font))))
  (if (not h)
      (set! h (car (map flo (glgui:stringheight (string-append s "|") font)))))
  (list center-aligned: w h s font color))

(define (flip-horizontally* w0 h0 t x1 y1 x2 y2 r #!rest colors)
  (apply list (- w0) h0 t x1 y1 x2 y2 r colors))

(define (flip-horizontally sprite)
  (apply flip-horizontally* sprite))

(define (flip-vertically* w0 h0 t x1 y1 x2 y2 r #!rest colors)
  (apply list w0 (- h0) t x1 y1 x2 y2 r colors))

(define (flip-vertically sprite)
  (apply flip-vertically* sprite))

(define (rotate* rd w0 h0 t x1 y1 x2 y2 r #!rest colors)
  (apply list w0 h0 t x1 y1 x2 y2 (+ r rd) colors))

(define (rotate sprite rd)
  (apply rotate* rd sprite))

(define (angle* rd w0 h0 t x1 y1 x2 y2 r #!rest colors)
  (apply list w0 h0 t x1 y1 x2 y2 rd colors))

(define (angle sprite rd)
  (apply angle* rd sprite))

(define (draw-sprite x y sigil #!rest rest)
  (let ((r glCore:red)
        (b glCore:blue)
        (g glCore:green)
        (a glCore:alpha))
    (cond 
    ((keyword? sigil) 
     (cond 
      ((eq? sigil left-aligned:)
       (apply glgui:draw-text-left (round x) (round y) rest))
      ((eq? sigil right-aligned:)
       (let ((w (car rest))) 
         (apply glgui:draw-text-right (round (- x w)) (round y) rest)))
      ((eq? sigil center-aligned:)
       (let ((w/2 (/ (car rest) 2.0))) 
         (apply glgui:draw-text-center (round (- x w/2)) (round y) rest)))))
    ((string? sigil)
     (apply glgui:renderstring (round x) (round y) sigil rest))
    ((procedure? sigil)
     (apply draw-sprite x y (sigil)))
    (else  
     (if (or (nan? y) (nan? x))
         (log-system `(list "draw-sprite draw at " ,x ,y))
         (apply draw-normal-sprite (round x) (round y) sigil rest))
     #f
     ))
    (set! glCore:red r)
    (set! glCore:blue b)
    (set! glCore:green g)
    (set! glCore:alpha a)))
