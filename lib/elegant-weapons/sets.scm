(library
    (elegant-weapons sets)
  (export union set-add difference intersection)
  (import (rnrs))

  (define (set-add s x)
    (if (member x s)
        s
        (cons x s)))
  
  (define union
    (lambda  s*
      (cond
        ((null? s*) `())
        (else
          (let ((s (car s*)) (s* (cdr s*)))
            (fold-left (lambda (s1 s2) (fold-left set-add s1 s2)) s s*))))))

  (define (intersection s . s*)
    (fold-left (lambda (s1 s2)
                 (filter (lambda (x)
                           (member x s2))
                         s1))
               s s*))

  (define (difference s1 s2)
    (filter (lambda (x) (not (member x s2))) s1))

  (define (subset? s1 s2)
    (andmap (lambda (x) (member x s2)) s1))
  
  ;; end library
  )
