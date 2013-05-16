(library
    (elegant-weapons sets)
  (export union set-add difference intersection subset? set-equal?
          disjoint-union)
  (import (rnrs)
          (elegant-weapons compat))

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

  (define disjoint-union
    (lambda (s1 s2)
      (difference (union s1 s2)
                  (intersection s1 s2))))
  
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

  (define (set-equal? s1 s2)
    (and (subset? s1 s2) (subset? s2 s1)))
  
  (define (andmap p . ls*)
    (if (null? (car ls*))
        #t
        (and (apply p (map car ls*))
             (apply andmap (cons p (map cdr ls*))))))
  
  ;; end library
  )
