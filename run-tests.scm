#!/usr/bin/env petite --libdirs lib --script 

(import
 (elegant-weapons print-c)
 (elegant-weapons parse-c))

(define (print-parse-roundtrip e)
  (display "Round trip testing for...\n")
  (display e)(newline)(newline)
  (let ((s (format-c e)))
    (display "Formatted C...\n")
    (display s)(newline)(newline)
    (let ((tokens (tokenize-string s)))
      (display "Tokens...\n")
      (display tokens)(newline)(newline)
      (let ((parsed (parse-c tokens)))
        (display "Parsed...\n")
        (display parsed)(newline)(newline)
        (if (equal? e parsed)
            (display "Success!\n")
            (error 'print-parse-roundtrip
                   "Parser round trip test failed."))))))
    
(print-parse-roundtrip
 '((func int main () (return (int 0)))))

(print-parse-roundtrip
 '((func int main ()
         (let x int (int 0))
         (return (var x)))))
