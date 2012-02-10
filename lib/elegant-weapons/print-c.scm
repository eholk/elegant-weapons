(library
  (elegant-weapons print-c)
  (export
    format-c
    print-c
    join
    format-arg
    format-ident
    format-stmt)
  (import
    (rnrs)
    (elegant-weapons match)
    (elegant-weapons compat)
    (elegant-weapons helpers))

  (define indent (make-parameter 0))
  (define (push-indent) (indent (+ (indent) 1)))
  (define (pop-indent) (indent (- (indent) 1)))

  (define-syntax indent-more
    (syntax-rules ()
      ((_ expr)
       (begin
         (push-indent)
         (let ((ans expr)) (begin (pop-indent) ans))))))

  (define (indent-before str)
    (let loop ((i (indent)))
      (if (zero? i) str (string-append "    " (loop (- i 1))))))

  (define format-ident
    (lambda (ident)
      (unless (symbol? ident)
        (error 'format-ident "invalid symbol" ident))
      (symbol->string ident)))
    
  (define-match format-type
    (u64 "uint64_t")
    ((ptr ,[t])
     (string-append t " __global *"))
    ((const-ptr ,[t])
     (string-append t " __global const *"))
    ((,[t] ,[t*] ...)
     (if (null? t*)
         t
         (string-append t "< " (join ", " t*) " >")))
    (,x (guard (symbol? x))
      (symbol->string x)))

  (define-match format-arg
    ((,[format-ident -> x] ,[format-type -> t])
     (string-append t " " x)))

  (define format-args
    (lambda (args)
      (join ", " (map format-arg args))))

  (define format-call-args
    (lambda (args)
      (join ", " (map format-expr args))))
  
  (define binop->string
    (lambda (op)
      (case op
        ((bitwise-or) "|")
        ((+) "+")
        ((*) "*")
        ((-) "-")
        ((/) "/")
        ((mod) "%")
        (else (error 'binop->string "unknown binop" op)))))
  
  (define relop->string
    (lambda (op)
      (case op
        ((== =) "==")
        ((<) "<")
        ((>) ">")
        ((<=) "<=")
        ((>=) ">=")
        (else (error 'relop->string "unknown relop" op)))))

  (define escape-string-literal
    (lambda (s)
      (if (zero? (string-length s))
          ""
          (string-append
            (case (string-ref s 0)
              ((#\newline) "\\n")
              ((#\") "\\\"")
              (else (string (string-ref s 0))))
            (escape-string-literal
              (substring s 1 (string-length s)))))))
  
  (define-match format-expr
    ((field ,[format-ident -> obj] ,x)
     (string-append obj "." (symbol->string x)))
    ((field ,[format-ident -> obj] ,x ,[format-type -> t])
     (string-append obj "." (symbol->string x) "<" t ">"))
    ((if ,[test] ,[conseq] ,[alt])
     (string-append "(" test ") ? (" conseq ") : (" alt ")"))
    ((vector-ref ,[v] ,[i])
     (string-append v "[" i "]"))
    ((sizeof ,[format-type -> t])
     (string-append "sizeof(" t ")"))
    ((deref ,[p])
     (string-append "*" p))
    ((cast ,[format-type -> t] ,[e])
     (string-append "((" t ")(" e "))"))
    ((addressof ,[e])
     (string-append "(&(" e "))"))
    ((,op ,[lhs] ,[rhs])
     (guard (binop? op))
     (string-append "(" lhs ") " (binop->string op) " (" rhs ")"))
    ((,op ,[lhs] ,[rhs])
     (guard (relop? op))
     (string-append "(" lhs ") " (relop->string op) " (" rhs ")"))
    ((assert ,[expr])
     (string-append "assert(" expr ")"))
    ((var ,var) (symbol->string var))
    ((int ,n) (number->string n))
    ((u64 ,n) (number->string n))
    ((str ,s) (string-append "\"" (escape-string-literal s) "\""))
    ((float ,f) (number->string f))
    ((c-expr ,t ,x) (symbol->string x))
    ((call ,[f] . ,[format-call-args -> args])
     (string-append f "(" args ")")))
  
  (define-match format-stmt
    ((begin ,stmt* ...)
     (string-append
       (indent-before "{\n")
       (join "\n" (indent-more (map format-stmt stmt*)))
       "\n"
       (indent-before "}")))
    ((let ,[format-ident -> ident] ,[format-type -> type]
          ,[format-expr -> expr])
     (indent-before
       (string-append type " " ident " = " expr ";")))
    ((if ,[format-expr -> test] ,conseq)
     (string-append
       (indent-before (string-append "if(" test ")\n"))
       (indent-more (format-stmt conseq))))
    ((if ,[format-expr -> test] ,conseq ,alt)
     (string-append
       (indent-before (string-append "if(" test ")\n"))
       (indent-more (format-stmt conseq))
       "\n"
       (indent-before "else\n")
       (indent-more (format-stmt alt))))
    ((return)
     (indent-before (string-append "return;")))
    ((return ,[format-expr -> expr])
     (indent-before (string-append "return " expr ";")))
    ((print ,[format-expr -> expr])
     (indent-before (string-append "print(" expr ");")))
    ((set! ,[format-expr -> x] ,[format-expr -> v])
     (indent-before
       (string-append x " = " v ";")))
    ((vector-set! ,[format-expr -> vec-expr]
       ,[format-expr -> i-expr] ,[format-expr -> val-expr])
     (indent-before
       (string-append vec-expr "[" i-expr "] = " val-expr ";")))
    ((while ,[format-expr -> expr] ,stmt)
     (string-append
       (indent-before (string-append "while(" expr ")\n"))
       (indent-more (format-stmt stmt))))
    ((for (,[format-ident -> i]
           ,[format-expr -> start]
           ,[format-expr -> end])
       ,stmt)
     (string-append
       (indent-before
         (string-append
           "for(int " i " = " start "; " i " < " end "; ++" i ")\n"))
       (indent-more (format-stmt stmt))))
    ((do ,[format-expr -> e])
     (indent-before (string-append e ";"))))

  (define-match format-decl
    ((include ,header)
     (string-append "#include \"" header "\"\n\n"))
    ((global ,type ,name ,args ...)
     (string-append
       (format-type type) " " (format-ident name)
       (if (null? args)
           ""
           (string-append "(" (format-call-args args) ")"))
       ";"))
    ((func ,[format-type -> type] ,[format-ident -> name]
       ,[format-args -> args] ,stmt* ...)
     (string-append type " " name "(" args ")\n"
       (format-stmt `(begin . ,stmt*))))
    ((extern ,[format-type -> type] ,[format-ident -> name]
       (,[format-type -> args] ...))
     (string-append type " " name "(" (join ", " args) ");\n")))
  
  (define format-c
    (lambda (decls)
      (string-append
        (join "\n\n" (map format-decl decls))
        "\n")))
  
  (define print-c
    (lambda (decls)
      (display (format-c decls)))))

