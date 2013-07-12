(library (elegant-weapons graphviz)
  (export dot->string write-dot)
  (import
   (rnrs)
   (elegant-weapons helpers))


  (define (dot->string g)
    (string-append
     "digraph G {\n"
     (join ""
           (map (lambda (n)
                  (let ((s (car n)))
                    (join ""
                          (map (lambda (n)
                                 (string-append
                                  "    \""
                                  (symbol->string s)
                                  "\" -> \""
                                  (symbol->string n)
                                  "\";\n"))
                               (cdr n)))))
                g))
     "}\n"))

  (define write-dot
    (case-lambda
      ((g) (write-dot g (current-output-port)))
      ((g p) (display (dot->string g) p)))))
