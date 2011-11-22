(library
    (elegant-weapons sets)
  (export union set-add difference intersection member?)
  (import (rnrs))

  (define member? memq)

  (define (set-add s x)
    (if (member? x s)
        s
        (cons x s)))
  
  (define (union s . s*)
    (fold-left (lambda (s1 s2) (fold-left set-add s1 s2)) s s*))

  (define (intersection s . s*)
    (fold-left (lambda (s1 s2)
                 (filter (lambda (x)
                           (member? x s2))
                         s1))
               s s*))

  (define (difference s1 s2)
    (filter (lambda (x) (not (member? x s2))) s1))
  
  ;; end library
  )