(library (contexts)
  (export define-context define-object)
  (import (rnrs (6))
          (gen-id))

  (define-syntax define-object
    (lambda (x)
      (syntax-case x ()
        ((define-object <name> (<m> <args> ...) ...)
         (with-syntax (((<call> ...) (map (lambda (x)
                                            (gen-id x #'<name> "-" x))
                                          #'(<m> ...))))
           #'(define-syntax <name>
               (lambda (x)
                 (syntax-case x ()
                   ((<name> (<formals> (... ...)) <expr> (... ...))
                    (with-syntax ((<bind> (datum->syntax #'<name> 'bind))
                                  (<m>    (datum->syntax #'<name> '<m>)) ...)
                      #'(lambda (obj <formals> (... ...))
                          (let ((<bind> (lambda (f) (lambda X (apply f obj X))))
                                (<m>    (lambda (<args> ...) (<call> obj <args> ...))) ...)
                            <expr> (... ...)))))))))))))


  (define-syntax define-context
    (lambda (x)
      (syntax-case x (fields)
        [(define-context <name> (fields <f> ...) <args> ...)
         (with-syntax ([with-record     (gen-id #'<name> "with-" #'<name>)]
                       [update-record   (gen-id #'<name> "update-" #'<name>)]
                       [make-record     (gen-id #'<name> "make-" #'<name>)]
                       ; Define the names of member access functions.
                       [(access ...)    (map (lambda (x)
                                               (gen-id x #'<name> "-" x))
                                             #'(<f> ...))])
           #'(begin
               (define-record-type <name> (fields <f> ...) <args> ...)

               (define-syntax with-record
                 (lambda (x)
                   (syntax-case x ()
                     [(with-record <r> <expr> (... ...))
                      (with-syntax ([<f> (datum->syntax #'with-record '<f>)]
                                    ...)
                        #'(let ([<f> (access <r>)] ...)
                            <expr> (... ...)))])))

               (define-syntax update-record
                 (lambda (x)
                   (syntax-case x ()
                     [(update-record <r> <bindings> (... ...))
                      (with-syntax ([<f> (datum->syntax #'update-record '<f>)]
                                    ...)
                        #'(let ([<f> (access <r>)] ...)
                            (let (<bindings> (... ...))
                              (make-record <f> ...))))])))
               ))])))
)
