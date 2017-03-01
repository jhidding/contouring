#| Compute level set of function. Input is a function, output a list of curves
 | with positive orientation with respect to the positive areas of the
 | function; meaning that walking these curves will always have the function
 | being negative on the right hand side and positive ond the left-hand side.
 |
 | First we do a pre-scan, sampling the function values on a coarse grid. This
 | grid determines the minimum size of the features found. A cell on this grid
 | is spanned by four corner points. If any of the corner points have a
 | different sign of the function value, the level-set passes through the
 | cell.
 |
 | In the second stage we trace the level-set in the marked cells.
 |
 | Finally the different segments are combined into closed curves, including
 | if necessary the border of the field.
 |#

(library (level-set)
  (export level-set cairo-level-set)
  (import (chezscheme)
          (lib))

  (define lib-level-set (load-shared-object "ls.so"))

  (define level-set*
    (foreign-procedure "level_set"
      (int double double double double double unsigned unsigned int int int int)
      void))

  (define cairo-level-set
    (foreign-procedure "cairo_level_set"
      (iptr iptr double double double double double unsigned unsigned)
      void))

  (define (level-set f s x0 y0 x1 y1 n m move_to line_to curve_to close_path)
    (let ((f*     (callback f (double double) double))
          (move*  (callback move_to (double double) void))
          (line*  (callback line_to (double double) void))
          (curve* (callback curve_to (double double double double double double) void))
          (close* (callback close_path () void)))

      (level-set* f*
                  s x0 y0 x1 y1 n m
                  move* line* curve* close*)

      ;(unlock-object f)
      ;(unlock-object move_to)
      ;(unlock-object line_to)
      ;(unlock-object curve_to)
      ;(unlock-object close_path)

      (unlock-object f*)
      (unlock-object move*)
      (unlock-object line*)
      (unlock-object curve*)
      (unlock-object close*)))
)
