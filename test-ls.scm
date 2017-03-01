(import
  (rnrs (6))
  (cairo) (cairo-object)
  (contouring)
  (numeric))

(define (himmelblau x y)
    (+ (expt (+ (* x x) y -11) 2)
       (expt (+ x (* y y) -7)  2)))

(define (to-svg filename width height cb)
  (let* ((surface (cairo-svg-surface-create width height filename))
         (context (cairo-create surface)))
    (cb context)
    (cairo-surface-finish surface)))

(define (echo f)
  (lambda X
    (format #t "~s ~a~%" f X)
    (apply f X)))

(to-svg
  "test.svg" 1100.0 1100.0
  (cairo ()
    (scale 100. 100.)
    (translate 4.0 5.5)
    (set-line-width 0.05)

    ((bind cairo-contour) himmelblau 10. -3.5 -5.0 6.5 5.0 10 10)

    (set-source-rgb 0.0 0.0 0.3)
    (fill-preserve)
    (set-source-rgb 0. 0. 0.)
    (stroke)

    ((bind cairo-contour) himmelblau 50. -3.5 -5.0 6.5 5.0 10 10)
    (set-source-rgb 0.5 0.0 0.2)
    (fill-preserve)
    (set-source-rgb 0. 0. 0.)
    (stroke)

    ((bind cairo-contour) himmelblau 100. -3.5 -5.0 6.5 5.0 10 10)
    (set-source-rgb 0.8 0.4 0.3)
    (fill-preserve)
    (set-source-rgb 0. 0. 0.)
    (stroke)

    ((bind cairo-contour) himmelblau 300. -3.5 -5.0 6.5 5.0 10 10)
    (set-source-rgb 0.9 0.8 0.5)
    (fill-preserve)
    (set-source-rgb 0. 0. 0.)
    (stroke)))

