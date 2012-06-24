(library
    (elegant-weapons compat)
  (export add1 sub1 syntax-error make-parameter parameterize
    last-pair make-list void)
  (import (rnrs))
  
  ;; This file provides least-common-denominator implementations of
  ;; common functions used throughout the framework. Most Schemes
  ;; provide their own version, however, so these should be used if
  ;; possible.
  
  (define (add1 x) (+ x 1))
  (define (sub1 x) (- x 1))
  
  (define syntax-error error)

  (define make-parameter
    (let ((filter (lambda (x) x)))
      (case-lambda
        ((value)
         (case-lambda
           (() value)
           ((new-value)
            (set! value (filter new-value)))))
        ((value filter)
         (case-lambda
           (() value)
           ((new-value)
            (set! value (filter new-value))))))))

  ;; This is from the Chez Scheme Version 8 User Guide
  ;; http://www.scheme.com/csug8/system.html#./system:s167
  (define-syntax parameterize
    (lambda (x)
      (syntax-case x ()
        [(_ () e1 e2 ...) (syntax (begin e1 e2 ...))]
        [(_ ([x v] ...) e1 e2 ...)
         (with-syntax ([(p ...) (generate-temporaries (syntax (x ...)))]
                       [(y ...) (generate-temporaries (syntax (x ...)))])
           (syntax
             (let ([p x] ... [y v] ...)
               (let ([swap (lambda ()
                             (let ([t (p)]) (p y) (set! y t)) ...)])
                 (dynamic-wind swap (lambda () e1 e2 ...) swap)))))])))

  (define (last-pair ls)
    (if (null? (cdr ls))
        ls
        (last-pair (cdr ls))))

  (define (void) (if #f #t))
  
  (define make-list
    (case-lambda
      ((n)
       (make-list n (void)))
      ((n obj)
       (let loop ((n n)
                  (ls '()))
         (if (zero? n)
             ls
             (loop (sub1 n) (cons obj ls)))))))

  ;; end-library
  )
