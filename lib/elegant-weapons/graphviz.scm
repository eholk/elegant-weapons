(library (elegant-weapons graphviz)
  (export dot->string write-dot)
  (import
   (rnrs)
   (elegant-weapons helpers))


  (define dot->string
    (case-lambda
      ((g) (dot->string g '()))
      ((g c)
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
        (join
         "\n"
         (map (let ((index 0))
                (lambda (c)
                  (set! index (+ 1 index))
                  (string-append "subgraph cluster" (number->string index)
                                 " {\n"
                                 (join " "
                                       (map (lambda (n)
                                              (string-append
                                               "\"" (symbol->string n) "\""
                                               "; "))
                                            c))
                                 "\n}\n")))
              c))
        "}\n"))))

  (define write-dot
    (case-lambda
      ((g) (write-dot g (current-output-port)))
      ((g c p) (display (dot->string g c) p))
      ((g p) (display (dot->string g) p)))))
