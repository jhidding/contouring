(import (rnrs (6))
        (lib)
        (cairo) (cairo-object)
        (numeric) (colour)
        (contouring))

(define (to-svg filename width height cb)
  (let* ((surface (cairo-svg-surface-create width height filename))
         (context (cairo-create surface)))
    (cb context)
    (cairo-surface-finish surface)))

(define (to-png filename width height cb)
  (let* ((surface (cairo-image-surface-create 'argb32 width height))
         (context (cairo-create surface)))
    (format #t "Rendering '~a' ...~%" filename)
    (cb context)
    (cairo-surface-write-to-png surface filename)
    (cairo-surface-finish surface)))

(define config
  (dict (window
          (dict (default-size    800 1800)
                (title           "Zeeman's Catastrophe Machine")
                (subtitle        "how to get discontinious behaviour out of continuous physics")))
        (machine
          (dict (disc-radius     1.0)
                (elastic-length  1.5)
                (spring-constant 2.0)
                (fixed-end      -3.0)
                (init-state      3.0 0.0 0.0)))
        (view
          (dict (geometry        8.0 18.0 0.5 0.0)))))

(define (potential l k r ax)
  (lambda (x y φ)
    (let ((ab (sqrt (+ (expt (- ax (* r (cos φ))) 2)
                       (expt (* r (sin φ)) 2))))
          (bc (sqrt (+ (expt (- x (* r (cos φ))) 2)
                       (expt (- y (* r (sin φ))) 2)))))
      (* 1/2 l (+ (expt (- k ab) 2)
                  (expt (- k bc) 2))))))
      ;(* 1/2 l (+ (expt (- k (max ab k)) 2)
      ;            (expt (- k (max bc k)) 2))))))

(define U (potential (config 'machine 'spring-constant)
                     (config 'machine 'elastic-length)
                     (config 'machine 'disc-radius)
                     (config 'machine 'fixed-end)))

(define (cairo-set-source-colour cr c)
  (receive (r g b a) (colour-rgba c)
    (cairo-set-source-rgba cr r g b a)))

(define colour-scheme
  (colour-hsv-gradient
    (make-colour 'hsva 0.0 1.0 1.0 0.2)
    (make-colour 'hsva 1/3 1.0 0.5 1.0)))

(define colour-scheme-2
  (colour-hsv-gradient
    (make-colour 'hsva 0.2 0.8 1.0 0.3)
    (make-colour 'hsva -0.4 1.0 0.3 1.0)))

(define (sorted-contour-indices n)
  (let ((phi (lambda (i) (+ (* (/ 1/2 n) turn)
                            (/ turn -4)
                            (* i (/ 1 n) turn)))))
  (list-sort (lambda (a b)
               (> (cos (phi a)) (cos (phi b))))
             (range n))))

(define qn 500)

(to-png
  "zeeman-colourmap.png" 1600. 3600.
  (cairo ()
    (define set-source-colour (bind cairo-set-source-colour))
    (define contour (bind cairo-contour))

    (set-operator 'add)
    (set-source-rgb 0.5 0.5 0.5)

    (for-each (lambda (i)
                (let* ((phi0 (+ (/ turn (* 2 qn)) (/ turn -2) (* (/ i qn) turn)))
                       (phi1 (+ (/ turn (* 2 qn)) (/ turn -2) (* (/ (inc i) qn) turn)))
                       (g0   (lambda (x y) ((D (lambda (phi*) (U x y phi*))) phi0)))
                       (g1   (lambda (x y) ((D (lambda (phi*) (U x y phi*))) phi1)))
                       (surf (cairo-surface-create-similar (get-target) 'color-alpha 1600 3600))
                       (sctx (cairo-create surf)))

                  ((cairo ()
                     (define contour (bind cairo-contour))

                     (scale 200. 200.)
                     (translate 3.5 9.0)
                     (set-operator 'xor)
                     (set-source-rgba 1 1 1 1)
                     (contour g1 0.0 -3.5 -9 4.5 9 100 200)
                     (fill)
                     (contour g0 0.0 -3.5 -9 4.5 9 100 200)
                     (fill)) sctx)

                  (set-source-colour (colour-scheme
                                       (/ (+ 1 (cos (+ phi0 (/ turn (* 2 qn))))) 2)))
                  (mask (cairo-pattern-create-for-surface surf))))
              (sorted-contour-indices qn))))

(to-png
  "zeeman-checker.png" 1600. 3600.
  (cairo ()
    (define set-source-colour (bind cairo-set-source-colour))
    (define contour (bind cairo-contour))
    (scale 200. 200.)
    ;(translate 3.5 2.6)
    (translate 3.5 9.0)

    (set-operator 'xor)
    (set-source-rgb 1 1 1)
    (for-each (lambda (i)
                (let* ((phi (+ (* 1/112 turn) (/ turn -2) (* i 1/56 turn)))
                       (g   (lambda (x y) ((D (lambda (phi*) (U x y phi*))) phi))))
                  (contour g 0.0 -3.5 -9 4.5 9 20 40)
                  (fill)))
              (range 56))))

(to-png
  "zeeman-lines.png" 1600. 3600.
  (cairo ()
    (define set-source-colour (bind cairo-set-source-colour))
    (define contour (bind cairo-contour))
    (scale 200. 200.)
    ;(translate 3.5 2.6)
    (translate 3.5 9.0)
    (set-line-width 0.005)

    (set-operator 'over)
    (set-source-rgb 0. 0. 0.)
    (for-each (lambda (i)
                (let* ((phi (+ (* 1/112 turn) (/ turn -2) (* i 1/56 turn)))
                       (g   (lambda (x y) ((D (lambda (phi*) (U x y phi*))) phi))))
                  (contour g 0.0 -3.5 -9 4.5 9 20 40)
                  (stroke)))
              (range 56))))
