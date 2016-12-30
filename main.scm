#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2013, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#
;; helloworld example

(define gui #f)

(define *platform* (string->symbol (system-platform)))

(define (->string expr)
  (with-output-to-string "" (lambda ()
                           (display expr))))

(##define-syntax assert
  (lambda (stx)
    (define (ensure-code o)
      (if (##source? o) (##source-code o) o))
    (let* ((code (ensure-code stx))
           (expr (ensure-code (car code)))
           (str (if (cadr code) (ensure-code (cadr code)) #f)))
      (##sourcify
       `(if (not ,expr) (error (string-append "Assertion error: " ,(if str str `(->string ',expr)))) #t)
       stx))))

(##define-syntax assert-match
  (lambda (stx)
    (define (ensure-code o)
      (if (##source? o) (##source-code o) o))
    (let* ((code (ensure-code stx))
           (expr (ensure-code (car code)))
           (pat (ensure-code (cadr code)))
           (str (if (caddr code) (ensure-code (caddr code)) #f)))
      (##source-code
       `(assert (match ,expr (,pat #t) (anything-else #f)) ,@(if str (list str) (list)))))))

(define *last-time* 0)
(define *dt* 30)
(define (update-time-info)
  (let ((now (fl* 1000.0 (exact->inexact (time->seconds (current-time))))))
    (set! *dt* (min 30.0 (- now *last-time*)))
    (set! *last-time* now)
    *dt*))

(define *screen-w* 480)
(define *screen-h* 320)

(include "sounds.scm")
(include "sorting.scm")

(include "drawing.scm")

(include "components.scm")
(include "systems.scm")

(log-system "past systems and components.")

(define (random-ball-x-velocity)
  (* (if (< (random-real) 0.5) -1.0 1.0)
     (+ (* 0.1 (random-real)) 0.1)))

(define (random-ball-y-velocity)
  (* (random-real) 0.1))

(define (make-ball #!key
                   (x0 (/ *screen-w* 2))
                   (y0 (- *screen-h* (/ *screen-h* 10)))
                   (vx0 (random-ball-x-velocity))
                   (vy0 (random-ball-y-velocity))
                   (color White))
  (log-system (list "Creating ball" x0 y0 vx0 vy0))
  (entity!
   (list c-falling -0.001)
   (list c-position x0 y0)
   (list c-velocity
         vx0
         vy0)
   (list c-drawn 0 (rect color))
   (list c-width 10)
   (list c-height 10)
   (list c-bounded-within 0 0 *screen-w* *screen-h*)
   (list c-restitution 0.95)))

(define (reset-paddle paddle)
  (let ((h (entity-component paddle c-height)))
    (w/pos paddle (lambda (pos)
                    (pos-y-set! pos (exact->inexact (- *screen-h* h)))))
    (w/vel paddle (lambda (pos)
                    (pos-x-set! pos 0.0)
                    (pos-y-set! pos 0.0)))))

(define (reset-ball ball)
  (log-system (list "reset-ball" ball))
  (w/pos ball
         (lambda (pos)
           (pos-x-set! pos (exact->inexact (/ *screen-w* 2)))
           (pos-y-set! pos (exact->inexact (- *screen-h* (/ *screen-h* 10))))))
  (log-system (list "reset-ball 2" ball))
  (w/vel ball
         (lambda (pos)
           (pos-x-set! pos (random-ball-x-velocity))
           (pos-y-set! pos (random-ball-y-velocity))))
  (log-system (list "reset-ball 3" ball))
  ball
  ;; (if (entity-has-component? ball c-force)
  ;;     (remove-component! ball c-force))
  )

(define (make-paddle side
                     #!key
                     (w 10)
                     (h 100)
                     (y0 (- *screen-h* h))
                     (color Azure))
  (entity!
   (list c-falling -0.001)
   (list c-position (match side
                           (left: 5)
                           (right: (- *screen-w* (+ 5 w))))
         y0)
   (list c-velocity
         0
         0)
   (list c-drawn 0 (rect color))
   (list c-width w)
   (list c-height h)
   (list c-bounded-within 0 0 *screen-w* *screen-h*)
   (list c-restitution 0.3)))

(define (make-balls n)
  (cond
   ((= n 0) done:)
   (else
    (make-ball)
    (make-balls (- n 1)))))

(define *p1-score* 0)
(define *p2-score* 0)
(define *volley* 0)

(define (make-score-1-display #!key (x 20) (y 10))
  (entity!
   (list c-position x y)
   (list c-drawn -1
         (lambda (e)
           (apply draw-sprite x y (left-string! (->string *p1-score*)))))))

(define (make-instructions-p1 #!key (x 20) (y 30))
  (let ((s (case *platform*
             ((ios android) (left-string! "TAP"))
             (else (left-string! "Press Z")))))
    (entity!
     (list c-position x y)
     (list c-drawn -1
           (lambda (e)
             (apply draw-sprite x y s))))))

(define (make-score-2-display #!key (x (- *screen-w* 20)) (y 10))
  (entity!
   (list c-position x y)
   (list c-drawn -1
         (lambda (e)
           (apply draw-sprite x y (right-string! (->string *p2-score*)))))))

(define (make-instructions-p2 #!key (x (- *screen-w* 20)) (y 30))
  (let ((s (case *platform*
             ((ios android) (left-string! "TAP"))
             (else (right-string! "Press M")))))
    (entity!
     (list c-position x y)
     (list c-drawn -1
           (lambda (e)
             (apply draw-sprite x y s))))))

(define (make-volley-display #!key
                             (x (/ *screen-w* 2))
                             (y (- *screen-h* 30)))
  (entity!
   (list c-position x y)
   (list c-drawn -1
         (lambda (e)
           (apply draw-sprite x y (center-string! (string-append "Volley "
                                                                 (->string *volley*))))))))

(define (make-reset-instructions #!key
                                 (x (/ *screen-w* 2))
                                 (y (- *screen-h* 60)))
  (let ((s (case *platform*
             ((ios android) (left-string! "TAP TO RESET"))
             (else (center-string! "Press R to Reset")))))
    (entity!
     (list c-position x y)
     (list c-drawn -1
           (lambda (e)
             (apply draw-sprite x y s))))))

(define (paddle-ball-interaction paddle ball)
  (let ((ball-top (top-of ball))
        (ball-bottom (bottom-of ball))
        (ball-left (left-of ball))
        (ball-right (right-of ball))
        (ball-position (entity-component ball c-position))
        (ball-velocity (entity-component ball c-velocity))
        (paddle-top (top-of paddle))
        (paddle-bottom (bottom-of paddle))
        (paddle-left (left-of paddle))
        (paddle-right (right-of paddle))
        (paddle-pos (entity-component paddle c-position))
        (paddle-velocity (entity-component paddle c-velocity)))
    (if (and
         (< ball-bottom paddle-top)
         (> ball-top paddle-bottom))
        (cond
          ((and
            (> ball-left paddle-left)
            (< ball-left paddle-right))
           (pos-x-set! ball-position paddle-right)
           (pos-x-set! ball-velocity (*
                                      (if (< *volley* 12) 1.1 1.0)
                                      (- (pos-x ball-velocity))))
           (pos-y-set! ball-velocity (+ (pos-y ball-velocity)
                                        (* 0.1 (pos-y paddle-velocity))))
           (set! *volley* (+ 1 *volley*))
           (audiofile-play *pong*))
          ((and
            (< ball-right paddle-right)
            (> ball-right paddle-left))
           (pos-x-set! ball-velocity (*
                                      (if (< *volley* 12) 1.1 1.0)
                                      (- (pos-x ball-velocity))))
           (pos-y-set! ball-velocity (+ (pos-y ball-velocity)
                                        (* 0.1 (pos-y paddle-velocity))))
           (pos-x-set! ball-position (- paddle-left (entity-component ball c-width)))
           (set! *volley* (+ 1 *volley*))
           (audiofile-play *pong*))))))

(define *p1* #f)
(define *p2* #f)
(define *ball* #f)

(define *show-fps* #f)
(define make-frame-rate-entity
  (let ((measurement 30.0))
    (lambda (#!key (x (/ *screen-w* 2))
                   (y 10)
                   (smoothing 0.99))
      (entity!
       (list c-position x y)
       (list c-drawn -1
             (lambda (e)
               (set! measurement
                     (fl+
                      (fl* measurement smoothing)
                      (fl* *dt* (- 1.0 smoothing))))
               (if *show-fps*
                   (apply draw-sprite
                          x y
                          (center-string!
                           (string-append "FPS: "
                                          (number->string (inexact->exact (round (fl/ 1.0 (fl/ measurement 1000.0)))))))))))))))

(main
;; initialization
  (lambda (w h)
    (make-window *screen-w* *screen-h*)
    (glgui-orientation-set! GUI_LANDSCAPE)
    (let ((p1 (make-paddle left:))
          (p2 (make-paddle right:))
          (b (make-ball)))
      (make-score-1-display)
      (make-score-2-display)
      (make-instructions-p1)
      (make-instructions-p2)
      (make-volley-display)
      (make-reset-instructions)
      (make-frame-rate-entity)
      (set! left-callback
            (lambda ()
              (set! *p2-score* (+ 1 *p2-score*))
              (set! *volley* 0)
              (log-system "About to call reset-ball")
              (reset-ball b)))
      (set! right-callback
            (lambda ()
              (set! *p1-score* (+ 1 *p1-score*))
              (set! *volley* 0)
              (log-system "About to call reset-ball")
              (reset-ball b)))
      (set! *p1* p1)
      (set! *p2* p2)
      (set! *ball* b)
      (log-system (list "Entity count" (system-count-entities system-drawn)))
      (glCore-registerhook (lambda ()
                             (glClearColor 0. 0. 0. 0.)
                             (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                             (glEnable GL_BLEND)
                             (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
                             
                             (update-time-info)
                             (system-execute system-falling)
                             (system-execute system-dynamical)
                             (system-execute system-bounded-within)
                             (paddle-ball-interaction *p1* *ball*)
                             (paddle-ball-interaction *p2* *ball*)
                             (system-execute system-drawn))))
    (set! gui (make-glgui)))
;; events
  (lambda (t x y) 
    (if (= t EVENT_KEYPRESS)
        (begin 
          (if (= x EVENT_KEYESCAPE) (terminate))
          (case (integer->char x)
            ((#\z)
             (w/vel *p1* (lambda (v) (pos-y-set! v 0.0)))
                                        ;             (add-force *p1* 0 0.01)
             (set-vel! *p1* 0 (* 0.1 5))
             (audiofile-play *bump*))
            ((#\m)
             (w/vel *p2* (lambda (v) (pos-y-set! v 0.0)))
                                        ;             (add-force *p2* 0 0.01)
             (set-vel! *p2* 0 (* 0.1 5))
             (audiofile-play *bump*))
            ((#\f)
             (set! *show-fps* (not *show-fps*)))
            ((#\r)
             (log-system "reset key")
             (set! *p1-score* 0)
             (set! *p2-score* 0)
             (set! *volley* 0)
             (reset-paddle *p1*)
             (log-system "post p1")
             (reset-paddle *p2*)
             (log-system "post p2")
             (reset-ball *ball*)
             (log-system "post ball")))))
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
