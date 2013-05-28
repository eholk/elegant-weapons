(library
    (elegant-weapons sets-tests)
  (export sets)
  (import (rnrs)
          (elegant-weapons sets)
          (elegant-weapons tester))

  (define-test-suite sets
    (union1 (lambda (fail)
              (let ((s1 '(a b c))
                    (s2 '(c d e)))
                (unless (set-equal? (union s1 s2)
                                    '(a b c d e))
                  (fail))))))
  )
