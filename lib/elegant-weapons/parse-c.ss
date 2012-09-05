(library
    (elegant-weapons parse-c)
  (export
   tokenize-string
   read-token
   parse-c)
  (import (rnrs)
          (elegant-weapons helpers))

  (define keywords
    '("int"
      "return"))

  (define (punctuation? c)
    (member c '(#\{ #\} #\( #\) #\; #\=)))

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

  
  (define (parse-c tokens)
    (call/cc (lambda (k)
               (parse-decls tokens '() k)
               (error 'parse-c "Failed to parse tokens."))))

  (define (parse-decls tokens decls k)
    (if (null? tokens)
        (k decls)
        (parse-decl tokens
                    (lambda (tokens decl)
                      (parse-decls tokens (append decls (list decl)) k)))))
  
  (define (parse-decl tokens k)
    (parse-type tokens
                (lambda (tokens type)
                  (let ((name (string->symbol (car tokens))))
                    ((parse-arguments
                      (lambda (tokens args)
                       (parse-block tokens
                                    (lambda (tokens block)
                                      (k tokens
                                         `(func ,type ,name ,args
                                                . ,block))))))
                     (cdr tokens))))))

  (define (parse-type tokens k)
    (let ((t (car tokens))
          (tokens (cdr tokens)))
      (if (equal? "int" t)
          (k tokens 'int))))

  (define-match (parse-arguments k)
    ((#\( #\) . ,rest)
     (k rest '())))

  (define (parse-block tokens k)
    (if (equal? (car tokens) #\{)
        (parse-statements (cdr tokens)
                          '()
                          (lambda (tokens stmts)
                            (if (equal? (car tokens) #\})
                                (k (cdr tokens) stmts))))))

  (define (parse-statements tokens stmts k)
    (parse-statement tokens
                     (lambda (tokens stmt)
                       (parse-statements tokens
                                         (append stmts (list stmt))
                                         k)))
    (k tokens stmts))

  (define (parse-statement tokens k)
    ;; try to parse a return statement
    (let ((t (car tokens))
          (tokens (cdr tokens)))
      (if (equal? t "return")
          (parse-expr tokens
                      (lambda (tokens expr)
                        (if (equal? (car tokens) #\;)
                            (k (cdr tokens) `(return ,expr)))))))
    ;; try to parse a declaration
    (parse-type tokens
                (lambda (tokens type)
                  (let ((name (string->symbol (car tokens)))
                        (eq (cadr tokens))
                        (tokens (cddr tokens)))
                    (parse-expr tokens
                                (lambda (tokens expr)
                                  (if (equal? (car tokens) #\;)
                                      (k (cdr tokens)
                                         `(let ,name ,type ,expr)))))))))

  (define (parse-expr tokens k)
    (let ((t (car tokens))
          (tokens (cdr tokens)))
      (cond
        ((integer? t)
         (k tokens `(int ,t)))
        ;; Identifier
        ((string? t)
         (k tokens `(var ,(string->symbol t)))))))
         
  ;; end library
  )
