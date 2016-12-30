(define-component c-sprite (e s)
  s)

(define (set-sprite! entity s)
  (entity-component-set! entity c-sprite s))

(define (nth seq i)
  (cond
   ((and (list? seq) (= i 0)) (car seq))
   ((list? seq) (nth seq (- i 1)))
   ((vector? seq) (vector-ref seq i))))

(define-type pos x y
  constructor: -make-pos)
(define (make-pos x y)
  (-make-pos (exact->inexact x) (exact->inexact y)))
(define-pattern (pos x y)
  `(and (? pos?) (call pos-x ,x) (call pos-y ,y)))

(define (r-theta-x r theta)
  (* (cos theta) r))

(define (r-theta-y r theta)
  (* (sin theta) r))

(define (make-pos-r-theta r theta)
  (make-pos (r-theta-x r theta)
            (r-theta-y r theta)))

(define (norm p)
  (let* ((px (pos-x p))
         (py (pos-y p))
         (d (sqrt (+ (square px) (square py)))))
    (make-pos (/ px d) (/ py d))))

(define (pos-length p)
  (sqrt (+ (square (pos-x p)) (square (pos-y p)))))

(define (shorten-vector v p #!key (in-place #t))
  (set! p (exact->inexact p))
  (cond
   (in-place
    (pos-x-set! v (fl* p (pos-x v)))
    (pos-y-set! v (fl* p (pos-y v)))
    p)
   (else
    (make-pos (fl* p (pos-x v))
              (fl* p (pos-y v))))))

(define (shorten-vector-x v p #!key (in-place #t))
  (set! p (exact->inexact p))
  (cond
   (in-place
    (pos-x-set! v (fl* p (pos-x v)))
    p)
   (else
    (make-pos (fl* p (pos-x v))
              (pos-y v)))))

(define (shorten-vector-y v p #!key (in-place #t))
    (set! p (exact->inexact p))
  (cond
   (in-place
    (pos-y-set! v (fl* p (pos-y v)))
    p)
   (else
    (make-pos (pos-x v)
              (fl* p (pos-y v))))))

(define-component c-position (e x y)
  ;; (assert (number? x))
  ;; (assert (number? y))
  (make-pos x y))

(define (w/pos e b)
  (do-with-component
   e
   c-position
   (lambda (pos)
     (assert-match pos (pos (? number? x) (? number? y)))
     (b pos))))

(define (w/vel e b)
  (do-with-component
   e
   c-velocity
   (lambda (pos)
     (assert-match pos (pos (? number? x) (? number? y)))
     (b pos))))

(define (set-pos! e x y)
  (cond
   ((entity-has-component? e c-position)
    (let ((pos (entity-component e c-position)))
      (pos-x-set! pos (exact->inexact x))
      (pos-y-set! pos (exact->inexact y))))
   (else
    (add-component! e c-position x y))))

(define (set-vel! e x y)
  (cond
   ((entity-has-component? e c-velocity)
    (let ((pos (entity-component e c-velocity)))
      (pos-x-set! pos (exact->inexact x))
      (pos-y-set! pos (exact->inexact y))))
   (else
    (add-component! e c-velocity x y))))


(define (t/pos e b)
  (let ((r (do-with-component
            e
            b)))
    (assert-match pos (pos (? number? x) (? number? y)))
    (entity-component-set! e c-position r)))

(define-component c-velocity ( e x y)
  (make-pos x y))

(define-component c-force ( e x y)
  (make-pos x y))

(define (w/force e b)
  (do-with-component
   e
   c-force
   (lambda (force)
     (assert-match force (force (? number? x) (? number? y)))
     (b force))))

(define (add-force e x y)
  (if (not (entity-has-component? e c-force))
      (add-component! e c-force x y)
      (w/force e (lambda (f)
                   (pos-x-set! f (+ x (pos-x f)))
                   (pos-y-set! f (+ y (pos-y f)))))))

(define-type drawn order thunk)
(define-component c-drawn (e order thunk)
  (make-drawn order thunk))

(define (w/color c thunk)
  (let (;; (current-color (color-rgba
        ;;                 glCore:red
        ;;                 glCore:green
        ;;                 glCore:blue
        ;;                 glCore:alpha))
        (null (glCoreColor (if (procedure? c) (c) c)))
        (result (thunk)))
    ;;(glCoreColor current-color)
    result))

(define (rect color)
  (define (draw e)
    (let ((w (entity-component e c-width))
          (h (entity-component e c-height)))
      (w/pos e (lambda (p)
                 (let ((x (pos-x p))
                       (y (pos-y p)))
                   (w/color color
                            (lambda ()
                              (glCoreBegin GL_TRIANGLE_STRIP)
                              (glCoreVertex2f x y)
                              (glCoreVertex2f (fl+ x w) y)
                              (glCoreVertex2f x (fl+ y h))
                              (glCoreVertex2f (fl+ x w) (fl+ y h))
                              (glCoreEnd))))))))
  draw)

(define-component c-falling (e m)
  m)

(define-component c-width (e w) (exact->inexact w))
(define-component c-height (e h) (exact->inexact h))
(define-component c-restitution (e r) (exact->inexact r))

(define (bottom-of e)
  (w/pos e (lambda (p) (pos-y p))))

(define (top-of e)
  (w/pos e (lambda (p) (+ (pos-y p) (if (entity-has-component? e c-height)
                                        (entity-component e c-height)
                                        0)))))

(define (left-of e)
  (w/pos e (lambda (p) (pos-x p))))

(define (right-of e)
  (w/pos e (lambda (p) (+ (pos-x p)
                          (if (entity-has-component? e c-width)
                              (entity-component e c-width)
                              0)))))

(define (width-of e)
  (if (entity-has-component? e c-width) (entity-component e c-width) 0))

(define (height-of e)
  (if (entity-has-component? e c-height) (entity-component e c-height) 0))


(define-type bounding-box x y w h constructor: -make-bounding-box)
(define (make-bounding-box x y w h)
  (-make-bounding-box
   (exact->inexact x)
   (exact->inexact y)
   (exact->inexact w)
   (exact->inexact h)))

(define-component c-bounded-within (e x y w h)
  (make-bounding-box x y w h))

(define (entity-bottom-below-bounding-box? e bb)
  (let ((b (bottom-of e)))
    (< b (bounding-box-y bb))))

(define (entity-top-above-bounding-box? e bb)
  (let ((t (top-of e)))
    (> t (+ (bounding-box-y bb) (bounding-box-h bb)))))

(define (entity-right-right-of-bounding-box? e bb)
  (let ((r (right-of e)))
    (> r (+ (bounding-box-x bb) (bounding-box-w bb)))))

(define (entity-left-left-of-bounding-box? e bb)
  (let ((l (left-of e)))
    (< l (bounding-box-x bb))))




