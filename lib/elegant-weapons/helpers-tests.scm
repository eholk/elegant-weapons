(library
    (elegant-weapons helpers-tests)
  (export helpers)
  (import (rnrs)
          (elegant-weapons helpers)
          (elegant-weapons tester))

  (define-test-suite helpers
    (map-values-1
     (lambda (fail)
       (let-values (((a b c)
                     (map-values (lambda (x y) (values x y (+ x y)))
                                 '(1) '(2))))
         (or (equal? a '(1)) (fail))
         (or (equal? b '(2)) (fail))
         (or (equal? c '(3)) (fail)))))
    (map-values-2
     (lambda (fail)
       (let-values (((a b) (map-values (lambda (x) (values x (+ 1 x)))
                                       '(1 2 3 4))))
         (or (equal? a '(1 2 3 4)) (fail))
         (or (equal? b '(2 3 4 5)) (fail)))))
    (andmap (lambda (fail)
              (or (andmap (lambda (x) x) '(1 2 3 4))
                  (fail))))
    (andmap2 (lambda (fail)
               (or (andmap + '(1 2 3 4) '(5 6 7 8))
                   (fail)))))
  )
