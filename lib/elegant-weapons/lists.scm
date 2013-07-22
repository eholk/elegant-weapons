;; A collection of macros and procedures for manipulating lists.
(library (elegant-weapons lists)
  (export fold-right-values)
  (import (rnrs))

  (define-syntax fold-right-values
    (syntax-rules ()
      ((_ (binding ...) body)
       (split-bindings () (binding ...) body))))

  (define-syntax split-bindings
    (syntax-rules (<-)
      ((_ (left ...) ((x e) rest ...) body)
       (split-bindings (left ... (x e)) (rest ...) body))
      ((_ (left ...) (<- rest ...) body)
       (fold-right-values-inner
        ((left ...)
         (rest ...))
        body))))
  
  (define-syntax fold-right-values-inner
    (syntax-rules ()
      ((_ (((out init) ...) ((in lists) ...)) body)
       (let loop ((out init) ...
                  (in lists) ...)
         (if (and (null? in) ...)
             (values out ...)
             (let-values (((out ...)
                           (loop out ... (cdr in) ...)))
               (let ((in (car in)) ...)
                 body))))))))
