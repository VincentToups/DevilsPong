(define (make-drawn-system c-drawn)
  (define jobs (list))
  (define (clear-jobs)
    (set! jobs (list)))
  (define (every e drawn)
    (let ((order (drawn-order drawn))
          (thunk (drawn-thunk drawn)))
      (set! jobs
            (cons
             (list (if (procedure? order)
                       (order)
                       order)
                   thunk
                   e)
             jobs))))
  (define (pre)
    (clear-jobs))
  (define (by-car p1 p2)
    (< (car p1) (car p2)))
  (define (draw-one p)    
    ((cadr p) (caddr p)))
  (define (post)
    (for-each draw-one (fast-sort jobs by-car)))

  (system! (list c-drawn)
   pre: pre
   post: post
   every: every))

(define system-drawn
  (make-drawn-system c-drawn))

(define (make-dynamical-system c-position c-velocity c-force)
  (define (every e pos vel)
    (let ((f (entity-component-or e c-force (make-pos 0 0))))
      (pos-x-set! pos (+ (pos-x pos) (* *dt* (pos-x vel))))
      (pos-y-set! pos (+ (pos-y pos) (* *dt* (pos-y vel))))
      
      (pos-x-set! vel (+ (pos-x vel) (* *dt* (pos-x f))))
      (pos-y-set! vel (+ (pos-y vel) (* *dt* (pos-y f))))
      
      (pos-x-set! f 0)
      (pos-y-set! f 0)))
  (system!
   (list c-position c-velocity)
   every: every))

(define system-dynamical
  (make-dynamical-system c-position c-velocity c-force))

(define (make-system-falling c-position c-falling)
  (system! (list c-position c-falling)
           every: (lambda (e p m)
                    (if (not (= 0.0 (pos-y p)))
                        (add-force e 0 m)))))

(define system-falling (make-system-falling c-position c-falling))

(define (left-callback) #f)
(define (right-callback) #f)

(define (make-system-bounded-within c-position
                                    c-velocity
                                    c-bounded-within
                                    c-restitution)
  (define (every e pos vel bb restitution)
    (cond
     ((entity-bottom-below-bounding-box? e bb)
      (pos-y-set! pos (bounding-box-y bb))
      (pos-y-set! vel (- (pos-y vel)))
      (shorten-vector-y vel restitution)
      (if (< (pos-y vel) .03)
          (pos-y-set! vel 0))
      (audiofile-play *pong*))
     ((entity-left-left-of-bounding-box? e bb)
      (pos-x-set! pos (bounding-box-x bb))
      (pos-x-set! vel (- (pos-x vel)))
                                        ;(shorten-vector-x vel restitution)
      (audiofile-play *pong*)
      (left-callback))
     ((entity-top-above-bounding-box? e bb)
      (pos-y-set! pos (- (+ (bounding-box-h bb) (bounding-box-y bb))
                          (height-of e)))
      (pos-y-set! vel (- (pos-y vel)))
      (shorten-vector-y vel restitution)
      (audiofile-play *pong*))
     ((entity-right-right-of-bounding-box? e bb)
      (pos-x-set! pos (- (+ (bounding-box-w bb)
                            (bounding-box-x bb))
                         (width-of e)))
      (pos-x-set! vel (- (pos-x vel)))
                                        ;(shorten-vector-x vel restitution)
      (audiofile-play *pong*)
      (right-callback))))
  (system! (list c-position c-velocity c-bounded-within c-restitution)
           every: every))

(define system-bounded-within (make-system-bounded-within c-position
                                                          c-velocity
                                                          c-bounded-within
                                                          c-restitution))
