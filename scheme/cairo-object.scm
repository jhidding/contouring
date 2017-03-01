(library (cairo-object)
  (export cairo)
  (import (contexts)
          (cairo))

  (define-object cairo
    (save) (restore) (paint)
    (fill) (fill-preserve) (stroke) (stroke-preserve)
    (set-source-rgb r g b) (set-source-rgba r g b a)
    (set-source-surface s x y)
    (set-source pattern)

    (set-line-width w) (set-line-cap c) (set-line-join j)
    (set-operator o)

    (mask-surface surf x y)
    (mask pattern)
    (get-target)

    ; paths
    (new-path)
    (close-path)
    (arc x y r t0 t1) (arc-negative x y r t0 t1)
    (line-to x y)
    (move-to x y)
    (curve-to sx0 sy0 sx1 sy1 x1 y1)
    (rectangle x y width height)

    ; transformations
    (translate tx ty)
    (scale sx sy)
    (rotate angle)
    (transform m)
    (set-matrix m)
    (get-matrix m)
    (identity-matrix)))
