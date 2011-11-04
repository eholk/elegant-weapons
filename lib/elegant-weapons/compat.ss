(library
    (elegant-weapons compat)
  (export add1 sub1 syntax-error make-parameter last-pair make-list void)
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
