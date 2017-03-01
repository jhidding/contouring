(library (numeric)
  (export D find-root-ridders sign ε turn minimise range)
  (import (rnrs (6))
          (only (ice-9 format) format))

  (define turn 6.28318530718)

  (define ε 1e-3)

  (define (sign x)
    (cond
      ((< x 0) -1)
      ((> x 0) +1)
      (else     0)))

  (define range
    (case-lambda
      [(b)      (range 0 b 1)]
      [(a b)    (range a b 1)]
      [(a b dx) (let loop ([lst '()]
                           [x a])
                  (if (>= x b)
                    (reverse lst)
                    (loop (cons x lst) (+ x dx))))]))


  #| Derivative of a function, by taking the difference on a small
   | interval 2ε. The derivative is approximated by the formula:
   |
   |               f(x+ε) - f(x-ε)
   |     f'(x) ≈  ─────────────────
   |                     2ε
   |#
  (define (D f)
    (lambda (x)
      (/ (- (f (+ x ε)) (f (- x ε))) (* 2 ε))))

  #| Finds the root of a function `f` between values `a` and `b`,
   | using Ridders' algorithm.  DOI:10.1109/TCS.1979.1084580
   |#
  (define (find-root-ridders f a b)
    (let* ((d   (/ (- b a) 2)) (m   (+ a d))
           (f-a (f a)) (f-m (f m)) (f-b (f b))
           (p   (/ f-b f-a))
           (q   (/ f-m f-a))
           (x   (+ m (/ (* d q) (sqrt (- (* q q) p)))))
           (f-x (f x)))

      (cond
        ((> ε (abs f-x)) x)
        ((> 0 (* f-x f-m)) (if (> m x)
                             (find-root-ridders f x m)
                             (find-root-ridders f m x)))
        ((> 0 (* f-x f-a)) (find-root-ridders f a x))
        (else              (find-root-ridders f x b)))))

  #| Find the minimum of function `f`. First walk in steps of `dx`
   | until the signs f' flip, then proceed with Ridders' algorithm
   | on that interval to find the root of f'.
   |#
  (define (minimise f x dx)
    (let* ((df   (D f))
           (df-x (df x))
           (dx   (* -1 (sign df-x) dx)))
      (let loop ((a    x)
                 (b    (+ x dx))
                 (df-b (df (+ x dx))))
        (if (< (* df-x df-b) 0)
          (find-root-ridders df a b)
          (loop b (+ b dx) (df (+ b dx)))))))

  (define (trace-line-step f x y a r)
    (find-root-ridders (lambda (t)
                 (f (+ x (* r (cos t)))
                    (+ y (* r (sin t)))))
               (- a 3) (+ a 3)))

  (define (trace-line f x0 y0 a0 step stop?)
    (let loop ((lst '())
               (x x0)
               (y y0)
               (a a0) (n 0))
      (let* ((a* (trace-line-step f x y a step))
             (x* (+ x (* step (cos a*))))
             (y* (+ y (* step (sin a*)))))
        ; (format #t "~a ~a~%" x* y*)
        (if (or (stop? x* y* a*) (> n 1000))
          (reverse lst)
          (loop (cons (list x* y*) lst) x* y* a* (+ n 1))))))
)
