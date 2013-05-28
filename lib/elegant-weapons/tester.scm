(library
    (elegant-weapons tester)
  (export register-tests run-tests)
  (import (rnrs))

  (define-record-type box (fields (mutable content)))
  
  (define tests (make-box '()))
  
  (define-syntax register-tests
    (syntax-rules ()
      ((_ (name test) ...)
       (begin
         (box-content-set! tests (append (box-content tests)
                                         (list (cons name test)))) ...))))


  (define run-tests
    (lambda ()
      (let loop ((tests (box-content tests))
                 (successes 0)
                 (failures 0))
        (if (pair? tests)
            (call/cc
             (lambda (k)
               (let ((name (caar tests))
                     (test (cdar tests)))
                 (display "Running test ") (display name) (display "...")
                 (test (lambda ()
                         (display "failed") (newline)
                         (k (loop (cdr tests)
                                  successes
                                  (+ 1 failures)))))
                 (display "success") (newline)
                 (loop (cdr tests)
                       (+ 1 successes)
                       failures))))
            (begin
              (display "All tests completed.") (newline)
              (display successes) (display " successes, ")
              (display failures) (display " failures") (newline)
              (if (> failures 0)
                  (error 'run-tests "Some tests failed")))))))
  )
