(library
    (elegant-weapons helpers-tests)
  (export helpers)
  (import (rnrs)
          (elegant-weapons helpers)
          (elegant-weapons tester))

  (define-test-suite helpers
    (andmap (lambda (fail)
              (or (andmap (lambda (x) x) '(1 2 3 4))
                  (fail))))
    (andmap2 (lambda (fail)
               (or (andmap + '(1 2 3 4) '(5 6 7 8))
                   (fail)))))
  )
