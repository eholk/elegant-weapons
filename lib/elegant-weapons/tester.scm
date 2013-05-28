(library
    (elegant-weapons tester)
  (export run-tests define-test-suite)
  (import (rnrs)
          (elegant-weapons compat))

  (define tests (make-parameter '()))

  (define (register-test name test)
    (display "registering ") (display name) (newline)
    (tests (append (tests) (list (cons name test)))))
  
  (define-syntax define-test-suite
    (syntax-rules ()
      ((_ suite-name (name test) ...)
       (define suite-name
         (let ((tests
                `((name . ,test) ...)))
           (lambda ()
             (run-suite 'suite-name tests)))))))

  (define run-suite
    (lambda (name tests)
      (display "Running tests for ") (display name) (newline)
      (let loop ((tests tests)
                 (successes 0)
                 (failures 0))
        (if (pair? tests)
            (call/cc
             (lambda (k)
               (let ((name (caar tests))
                     (test (cdar tests)))
                 (display "  Running test ") (display name) (display "...")
                 (test (lambda ()
                         (display "failed") (newline)
                         (k (loop (cdr tests)
                                  successes
                                  (+ 1 failures)))))
                 (display "success") (newline)
                 (loop (cdr tests)
                       (+ 1 successes)
                       failures))))
            (values successes failures)))))

  (define run-tests
    (lambda suites
      (let loop ((suites suites)
                 (successes 0)
                 (failures 0))
        (if (pair? suites)
            (let-values (((s f) ((car suites))))
              (loop (cdr suites) (+ successes s) (+ failures f)))
            (begin
              (display "All tests completed.") (newline)
              (display successes) (display " successes, ")
              (display failures) (display " failures") (newline)
              (if (> failures 0)
                  (error 'run-tests "Some tests failed")))))))
  )
