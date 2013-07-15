(library (elegant-weapons graphs)
  (export strongly-connected-components)
  (import (rnrs))

  ;; Uses Tarjan's algorithm to find the strongly connected components
  ;; in a graph.
  (define (strongly-connected-components graph)
    (let* ((stack '())
           (push! (lambda (x)
                    (set! stack (cons x stack))))
           (pop! (lambda () (let ((x (car stack)))
                              (set! stack (cdr stack))
                              x)))
           (index 0)
           (bump-index! (lambda () (set! index (+ 1 index))))
           (indices (make-eq-hashtable))
           (lowlink (make-eq-hashtable))
           (sccs '()))
      (letrec ((strong-connect
                (lambda (v)
                  (hashtable-set! indices v index)
                  (hashtable-set! lowlink v index)
                  (bump-index!)
                  (push! v)
                  (for-each
                    (lambda (w)
                      (if (hashtable-contains? indices w)
                          (if (memq w stack)
                              (hashtable-set!
                               lowlink v
                               (min (hashtable-ref lowlink v #f)
                                    (hashtable-ref indices w #f))))
                          (begin
                            (strong-connect w)
                            (hashtable-set!
                             lowlink v
                             (min (hashtable-ref lowlink v #f)
                                  (hashtable-ref lowlink w #f))))))
                    (let ((edges (assq v graph)))
                      (if edges (cdr edges) '())))
                  (if (eq? (hashtable-ref lowlink v #f)
                           (hashtable-ref indices v #f))
                      (let loop ((scc '())
                                 (w (pop!)))
                        (set! sccs (cons (cons w scc) sccs))
                        (unless (eq? w v)
                          (loop (cons w scc) (pop!))))))))
        (for-each
          (lambda (node)
            (let ((v (car node)))
              (unless (hashtable-contains? indices v)
                (strong-connect v))))
          graph))
      sccs))
)
