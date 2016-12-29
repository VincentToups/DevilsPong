Devil's Pong
============

Devil's pong is a one or two player (in person) game which combines
the classic mechanics of pong with the popular mechanics of flappy
bird.

It is written in Lambdanative/Scheme and should run on most modern
platforms, though there seems to be a bug in Lambdanative which is
preventing the ios version from using portrait mode correctly. 

Notes on Game Design
====================

The rise of touch based devices posed problems for game developers. A
screen can be subdivided into an infinite combination of "buttons" but
the lack of tactile feedback, combined with the fact that fingers
obscure the screen, makes transposition of classical, button based
controllers onto screens difficult.

Touch games, then, break down into two broad categories:

1. Games where input is precise but not much time pressure is placed
   on the player, so that they can make sure that the device
   recognizes their intentions. In this group of games I place Desert
   Golf, Angry Birds, and my own upcoming game The Death of The Corpse Wizard.
2. Games where time pressure is high but inputs are simplified or
   carefully designed with the touch pad in mind. Flappy Bird and
   other "endless" style games are in this category. Devil's Pong is
   as well, since each player has only one verb: "bounce" which
   applies an impulse to their paddle.
   
I like to think about this in terms of symmetry. In regular pong we
have two "actions" and two "outcomes": press the up button, move up
and press the down button and move down. If we restrict the player to
a single action, as is often desireable in touch based, real-time
games, then we have a problem: how do we enable the player to reach
the full space of possible positions? 

By providing a mechanism which breaks the symmetry of the state space:
where in pong our paddle is stationary if no input is provided, in
Devil's Pong the paddle falls by accelerating downward. The single
leftover input gives the paddle an upward impulse. The whole state
space is available to the player, but they must plan how they apply
impulses so that their paddle is in the right place at the right
time. Alternatives would be to have the paddle oscillate in
a sinusoidal pattern and let the player "freeze" the paddle in place
or to have the paddle move up and down at a fixed speed, but to let
the player reverse that speed with a button tap. 
   
Falling seems to be the more entertaining solution. Objects fall in a
gravitational field, tracing out parabolas. Flappy Bird (and Devil's
Pong) then, are games about calculating the intersection of parabolas
with other geometries. In Devil's Pong's case, each player wants the
parabola of their paddle to intersect that traced out by the
ball. This is fun because its a non-linear problem and because we have
to do it in nature a lot (since at the surfacce of the earth, any
thrown projectile traces out a parabola). 

Notes on the Implementation
===========================

Devil's Pong is a simple game which uses the same basic abstractions
as my much more complex upcoming game "The Death of The Corpse
Wizard." It is written in Scheme, and uses the Lambdanative
Development environment to target multiple platforms. 

Like all games I write, its based on an entity-component-system
framework. Almost all game behavior is formally described by "systems"
which group "entities" by what "components" they have. For instance,
there is a simple physics engine:

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

This indicates that any entity with components for position, velocity,
and force will have its position and velocity updated in this
way. Forces are set to zero on each update under the assumption that
some other system or logic will apply them appropriately on each
frame, before the dynamics are updated. 

We create entities like so:

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

There is a system specifically for falling objects which applies the
gravitational force on each frame. 

Because Devil's Pong is a small, simple game, many best practices are
violated. I often refer to global variables representing paddles and
the ball, and I fiddle with components in places which probably should
be solved more correctly elsewhere. For instance, a few hard coded
callbacks are in place in the system that manages collisions with the
edge of the playfield which should probably be more abstract. 

Anyway, I hope the code is pretty readable. Start in "main.scm".
