(library (elegant-weapons record-case)
  (export record-case)
  (import (rnrs))
  
  (define-syntax bind-record
    (syntax-rules ()
      ((_ rtd t i (x x* ...) b ...)
       (let ((access (record-accessor rtd i)))
         (let ((x (access t)))
           (bind-record rtd t (+ 1 i) (x* ...) b ...))))
      ((_ rtd t i () b ...)
       (begin b ...))))
  
  (define-syntax match-record
    (lambda (x)
      (syntax-case x ()
        ((_ t ((name x ...) b ...) rest)
         #`(if (#,(datum->syntax #'name (string->symbol
                                         (string-append
                                          (symbol->string
                                           (syntax->datum #'name))
                                           "?"))) t)
               (let ((rtd (record-rtd t)))
                 (bind-record rtd t 0 (x ...) b ...))
               rest)))))
  
  (define-syntax record-case
    (syntax-rules (else)
      ((_ e ((name x ...) b ...) rest ...)
       (let ((t e))
         (match-record t ((name x ...) b ...)
                       (record-case t rest ...))))
      ((_ e (else b ...))
       (begin b ...))
      ((_ e) (if #f 5))))
  
  )
