(library
  (elegant-weapons print-c)
  (export
    indent-more
    format-c
    print-c
    format-decl
    format-stmt
    format-expr
    format-ident
    format-type
    format-args
    format-call-args
    format-arg
    format-binop
    format-relop
    decl-fns
    stmt-fns
    expr-fns
    ident-fns
    type-fns
    arg-fns
    binop-fns
    relop-fns
    call-with-fns)
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
       (parameterize ((indent (+ 1 (indent)))) expr))))

  (define (indent-before str)
    (let loop ((i (indent)))
      (if (zero? i) str (string-append "    " (loop (- i 1))))))

  (define (format-sexp param)
    (lambda (x)
      (let loop ((x x) (fns (param)))
        ((car fns) x (lambda (ft) (loop ft (cdr fns)))))))

  (define-syntax call-with-fns
    (syntax-rules ()
      ((_ ((param fn) ...) body ...)
       (parameterize ((param (cons fn (param))) ...)
         body ...))))

  (define (print-c decl*)
    (display (format-c decl*)))

  (define (format-c decl*)
    (let ((decl* (map format-decl decl*)))
      (string-append (join "\n\n" decl*) "\n")))
  
  (define (format-decl-default decl _)
    (match decl
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
       (string-append type " " name "(" args ")"
                      (if (null? stmt*)
                          ";\n"
                          (string-append
                           " {\n"
                           (join "\n" (indent-more (map format-stmt stmt*)))
                           "\n}\n"))))
      ((extern ,[format-type -> type] ,[format-ident -> name]
         (,[format-type -> args] ...))
       (string-append type " " name "(" (join ", " args) ");\n"))
      ((typedef ,[format-ident -> name] ,[format-type -> type])
       (string-append "typedef " type " " name " ;\n"))
      (,else (error 'format-decl "could not format" else))))
  (define decl-fns (make-parameter `(,format-decl-default)))
  (define format-decl (format-sexp decl-fns))
  
  (define (format-stmt-default stmt _)
    (match stmt
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
      ((let ,[format-ident -> ident] ,[format-type -> type])
       (indent-before
        (string-append type " " ident ";")))
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
      ((print ,[format-expr -> e] ,[format-expr -> op])
       (indent-before (string-append "print(" e ", " op ");")))
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
      ((for (,[format-ident -> i]
             ,[format-expr -> start]
             ,[format-expr -> end]
             ,[format-expr -> step])
         ,stmt)
       (string-append
         (indent-before
           (string-append
             "for(int " i " = " start "; " i " < " end "; " i "= (" i " + " step "))\n"))
         (indent-more (format-stmt stmt))))
      ((do ,[format-expr -> e])
       (indent-before (string-append e ";")))
      (,else (error 'format-stmt "could not format" else))))
  (define stmt-fns (make-parameter `(,format-stmt-default)))
  (define format-stmt (format-sexp stmt-fns))

  (define (format-expr-default expr _)
    (match expr
      ((empty-struct) "{0}")
      ((field ,[obj] ,x)
       (string-append obj "." (format-ident x)))
      ((field ,[obj] ,x ,[format-type -> t])
       (string-append obj "." (format-ident x) "<" t ">"))
      ((if ,[format-expr -> test]
           ,[format-expr -> conseq]
           ,[format-expr -> alt])
       (string-append "(" test ") ? (" conseq ") : (" alt ")"))
      ((vector-ref ,[format-expr -> v]
         ,[format-expr -> i])
       (string-append v "[" i "]"))
      ((sizeof ,[format-type -> t])
       (string-append "sizeof(" t ")"))
      ((deref ,[format-expr -> p])
       (string-append "*" p))
      ((cast ,[format-type -> t] ,[e])
       (string-append "((" t ")(" e "))"))
      ((addressof ,[format-expr -> e])
       (string-append "(&(" e "))"))
      ((,op ,[format-expr -> lhs] ,[format-expr -> rhs])
       (guard (binop? op))
       (string-append "(" lhs ") " (format-binop op) " (" rhs ")"))
      ((,op ,[format-expr -> lhs] ,[format-expr -> rhs])
       (guard (relop? op))
       (string-append "(" lhs ") " (format-relop op) " (" rhs ")"))
      ((not ,[format-expr -> lhs])
       (string-append "!(" lhs ")"))
      ((assert ,[format-expr -> expr])
       (string-append "assert(" expr ")"))
      ((bool ,b) (if (not b) "false" "true"))
      ((var ,var) (format-ident var))
      ((char ,c) (format-char-literal c))
      ((int ,n) (number->string n))
      ((u64 ,n) (number->string n))
      ((str ,s) (string-append "\"" (escape-string-literal s) "\""))
      ((float ,f) (number->string f))
      ((c-expr ,x) (symbol->string x))
      ((call ,[format-expr -> f] . ,[format-call-args -> args])
       (string-append f "(" args ")"))
      (,else (error 'format-expr "could not format" else))))
  (define expr-fns (make-parameter `(,format-expr-default)))
  (define format-expr (format-sexp expr-fns))

  (define (mangle-ident x)
    (let ((y ""))
      (define (push c)
        (set! y (string-append y c)))
      (string-for-each
       (lambda (c)
         (case c
           ((#\-) (push "$"))
           ((#\$) (push "$$"))
           ((#\.) (push "$_"))
           ((#\$) (push "$$$"))
           (else (push (string c)))))
       x)
      y))
  
  (define (format-ident-default ident _)
    (unless (symbol? ident)
      (error 'format-ident "could not format" ident))
    (let ((reserved-words '(complex)))
      (if (memq ident reserved-words)
          (string-append "$$" (symbol->string ident))
          (mangle-ident (symbol->string ident)))))
  
  (define ident-fns (make-parameter `(,format-ident-default)))
  (define format-ident (format-sexp ident-fns))

  (define (format-type-default t _)
    (match t
      (u64 "uint64_t")
      ((ptr ,[t])
       (string-append t " __global *"))
      ((ref ,[t])
       (string-append t " &"))
      ((const-ptr ,[t])
       (string-append t " __global const *"))
      ((struct (,[format-ident -> x] ,[t]) ...)
       (string-append "struct {\n"
                      (indent-more
                       (join "" (map (lambda (x t)
                                       (indent-before
                                        (string-append t " " x ";\n")))
                                     x t)))
                      "}"))
      ((union (,[format-ident -> x] ,[t]) ...)
       (string-append "union {\n"
                      (indent-more
                       (join "" (map (lambda (x t)
                                       (indent-before
                                        (string-append t " " x ";\n")))
                                     x t)))
                      "}"))
      ((,[t] ,[t*] ...)
       (if (null? t*)
           t
           (string-append t "< " (join ", " t*) " >")))
      (,x (guard (symbol? x))
          (format-ident x))
      (,else (error 'format-type "could not format" else))))
  (define type-fns (make-parameter `(,format-type-default)))
  (define format-type (format-sexp type-fns))

  (define (format-args args)
    (join ", " (map format-arg args)))

  (define (format-call-args args)
    (join ", " (map format-expr args)))
  
  (define (format-arg-default arg _)
    (match arg
      ((,[format-ident -> x] ,[format-type -> t])
       (string-append t " " x))
      (,else (error 'format-arg "could not format" else))))
  (define arg-fns (make-parameter `(,format-arg-default)))
  (define format-arg (format-sexp arg-fns))

  (define (format-binop-default op _)
    (case op
      ((bitwise-or) "|")
      ((+) "+")
      ((*) "*")
      ((-) "-")
      ((/) "/")
      ((mod) "%")
      (else (error 'format-binop "could not format" op))))
  (define binop-fns (make-parameter `(,format-binop-default)))
  (define format-binop (format-sexp binop-fns))
  
  (define (format-relop-default op _)
    (case op
      ((== =) "==")
      ((<) "<")
      ((>) ">")
      ((<=) "<=")
      ((>=) ">=")
      (else (error 'format-relop "could not format" op))))
  (define relop-fns (make-parameter `(,format-relop-default)))
  (define format-relop (format-sexp relop-fns))

  (define escape-string-literal
    (lambda (s)
      (if (zero? (string-length s))
          ""
          (string-append
            (case (string-ref s 0)
              ((#\newline) "\\n\"\n\"")
              ((#\") "\\\"")
              (else (string (string-ref s 0))))
            (escape-string-literal
              (substring s 1 (string-length s)))))))

  (define (format-char-literal c)
    (string-append "'"
                   (case c
                     ((#\nul) "\\0")
                     (else (string c)))
                   "'"))
  
)

