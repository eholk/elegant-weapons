(library
    (elegant-weapons parse-c)
  (export
   tokenize-string
   read-token
   parse-c)
  (import (rnrs))

  (define keywords
    '("int"
      "return"))

  (define (punctuation? c)
    (member c '(#\{ #\} #\( #\) #\;)))

  (define (word-char? c)
    (char-alphabetic? c))
  
  (define (read-word port word)
    (if (word-char? (peek-char port))
        (read-word port (append word (list (get-char port))))
        (list->string word)))

  (define (read-number port num)
    (if (char-numeric? (peek-char port))
        (read-number port (append num (list (get-char port))))
        (string->number (list->string num))))
  
  (define read-token
    (case-lambda
      (()
       (read-token (current-input-port)))
      ((port)
       (let ((c (get-char port)))
         (cond
           ((eof-object? c) #f)
           ((char-whitespace? c) (read-token port))
           ((punctuation? c) c)
           ((char-numeric? c) (read-number port (list c)))
           ((word-char? c) (read-word port (list c))))))))

  (define (tokenize-string s)
    (let ((port (open-string-input-port s)))
      (let loop ((tokens '())
                 (token (read-token port)))
        (if token
            (loop (append tokens (list token)) (read-token port))
            tokens))))

  
  (define parse-c
    #f)
  
  ;; end library
  )
          