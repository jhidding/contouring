(library (lib)

  (export make-dict dict assuming receive
          define-ftype-helpers take
          compose $ thunk dec inc sqr)

  (import (rnrs (6))
          (ice-9 format)
          (rename (cut) (cut $)))

  #| compose functions
   |   @(param f . rest) variadic list of functions
   |   @(returns) functional composite of arguments
   |#
  (define (compose f . rest)
    (if (null? rest)
      f
      (let ((g (apply compose rest)))
        (lambda args
          (call-with-values ($ apply g args) f)))))

  (define (thunk x) (lambda () x))
  (define (dec x) (- x 1))
  (define (inc x) (+ x 1))
  (define (sqr x) (* x x))

  (define take
    (lambda (lst n)
      (let loop ((l lst)
                 (result '())
                 (m n))
        (if (or (zero? m) (null? l))
          (reverse result)
          (loop (cdr l) (cons (car l) result) (- m 1))))))

  (define (make-dict table)
    (lambda (entry . args)
      (let ((value (assuming (pair) (assq entry table)
                             (cdr pair))))
        (cond
          ((not value) #f)
          ((null? args) (apply values value))
          (else (apply (car value) args))))))

  (define-syntax dict
    (syntax-rules ()
      ((_ (<symbol> <values> ...) ...)
       (let ((table `((<symbol> (unquote <values>) ...) ...)))
         (make-dict table)))))

  (define-syntax assuming
    (syntax-rules ()
      ((_ (<formal>) <call> <expr> ...)
       (let ((<formal> <call>))
         (if <formal>
           (begin <expr> ...)
           #f)))

      ((_ (<formals> ...) <call> <expr> ...)
       (call-with-values
         (lambda () <call>)
         (case-lambda
           ((x) #f)
           ((<formals> ...) <expr> ...))))))

  (define-syntax receive
    (syntax-rules ()
      ((_ <formals> <expr> <body> ...)
       (call-with-values
         (lambda () <expr>)
         (lambda <formals> <body> ...)))))
)
