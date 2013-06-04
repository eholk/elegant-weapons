(library (elegant-weapons record-case-tests)
  (export record-case-tests)
  (import (rnrs)
          (elegant-weapons tester)
          (elegant-weapons record-case))

  (define-record-type foo (fields a b))
  (define-record-type bar (fields c))
  
  (define-test-suite record-case-tests
    (record-case-1 (lambda (fail)
                     (let ((a (make-foo 1 2)))
                       (record-case a
                         ((bar x) (fail))
                         ((foo a b) (+ a b)))))))
  )
