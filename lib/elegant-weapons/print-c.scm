(library
  (elegant-weapons print-c)
  (export
    indent-more
    format-c
    print-c
    decl-fns
    run-format
    format-append
    format-join
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
    mangle-ident
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
      (if (zero? i) str (format-append "    " (loop (- i 1))))))

  (define (format-sexp param)
    (lambda (x)
      (let loop ((x x) (fns (param)))
        ((car fns) x (lambda (ft) (loop ft (cdr fns)))))))

  (define-syntax call-with-fns
    (syntax-rules ()
      ((_ ((param fn) ...) body ...)
       (parameterize ((param (cons fn (param))) ...)
         body ...))))

  (define (put-m p s)
    (cond
      ((string? s) (put-string p s))
      ((procedure? s) (s p))
      (else (error 'format-append "can't format" s))))
  
  (define format-append
    (lambda s*
      (lambda (p)
        (unless (null? s*)
          (let ((s (car s*))
                (s* (cdr s*)))
            (put-m p s)
            ((apply format-append s*) p))))))

  (define format-join
    (lambda (j s*)
      (lambda (p)
        (unless (null? s*)
          (put-m p (car s*))
          (let loop ((s* (cdr s*)))
            (unless (null? s*)
              (put-m p j)
              (put-m p (car s*))
              (loop (cdr s*))))))))

  (define (run-format f)
    (let-values (((p s) (open-string-output-port)))
      (f p)
      (s)))
  
  (define (print-c decl*)
    (display (format-c decl*)))

  (define (format-c decl*)
    (run-format 
     (let ((decl* (map format-decl decl*)))
       (format-append (format-join "\n\n" decl*) "\n"))))
  
  (define (format-decl-default decl _)
    (match decl
      ((include ,header)
       (format-append "#include \"" header "\"\n\n"))
      ((global ,type ,name ,args ...)
       (format-append
         (format-type type) " " (format-ident name)
         (if (null? args)
             ""
             (format-append "(" (format-call-args args) ")"))
         ";"))
      ((func ,[format-type -> type] ,[format-ident -> name]
         ,[format-args -> args] ,stmt* ...)
       (format-append type " " name "(" args ")"
                      (if (null? stmt*)
                          ";\n"
                          (format-append
                           " {\n"
                           (format-join "\n" (indent-more (map format-stmt stmt*)))
                           "\n}\n"))))
      ((extern ,[format-type -> type] ,[format-ident -> name]
         (,[format-type -> args] ...))
       (format-append type " " name "(" (format-join ", " args) ");\n"))
      ((typedef ,[format-ident -> name] ,[format-type -> type])
       (format-append "typedef " type " " name " ;\n"))
      (,else (error 'format-decl "could not format" else))))
  (define decl-fns (make-parameter `(,format-decl-default)))
  (define format-decl (format-sexp decl-fns))
  
  (define (format-stmt-default stmt _)
    (match stmt
      ((begin ,stmt* ...)
       (format-append
         (indent-before "{\n")
         (format-join "\n" (indent-more (map format-stmt stmt*)))
         "\n"
         (indent-before "}")))
      ((let ,[format-ident -> ident] (fixed-array ,[format-type -> type] ,i)
            ,[format-expr -> expr])
       (indent-before
        (format-append type " " ident "[" (number->string i) "] = " expr ";")))
      ((let ,[format-ident -> ident] ,[format-type -> type]
            ,[format-expr -> expr])
       (indent-before
         (format-append type " " ident " = " expr ";")))
      ((let ,[format-ident -> ident] ,[format-type -> type])
       (indent-before
        (format-append type " " ident ";")))
      ((if ,[format-expr -> test] ,conseq)
       (format-append
         (indent-before (format-append "if(" test ")\n"))
         (indent-more (format-stmt conseq))))
      ((if ,[format-expr -> test] ,conseq ,alt)
       (format-append
         (indent-before (format-append "if(" test ")\n"))
         (indent-more (format-stmt conseq))
         "\n"
         (indent-before "else\n")
         (indent-more (format-stmt alt))))
      ((return)
       (indent-before (format-append "return;")))
      ((return ,[format-expr -> expr])
       (indent-before (format-append "return " expr ";")))
      ((print ,[format-expr -> expr])
       (indent-before (format-append "print(" expr ");")))
      ((print ,[format-expr -> e] ,[format-expr -> op])
       (indent-before (format-append "print(" e ", " op ");")))
      ((set! ,[format-expr -> x] ,[format-expr -> v])
       (indent-before
         (format-append x " = " v ";")))
      ((vector-set! ,[format-expr -> vec-expr]
         ,[format-expr -> i-expr] ,[format-expr -> val-expr])
       (indent-before
         (format-append vec-expr "[" i-expr "] = " val-expr ";")))
      ((goto ,name)
       (indent-before (format-append "goto " (format-ident name) ";")))
      ((label ,name)
       (indent-before (format-append (format-ident name) ":")))
      ((while ,[format-expr -> expr] ,stmt)
       (format-append
         (indent-before (format-append "while(" expr ")\n"))
         (indent-more (format-stmt stmt))))
      ((for (,[format-ident -> i]
             ,[format-expr -> start]
             ,[format-expr -> end])
         ,stmt)
       (format-append
         (indent-before
           (format-append
             "for(int " i " = " start "; " i " < " end "; ++" i ")\n"))
         (indent-more (format-stmt stmt))))
      ((for (,[format-ident -> i]
             ,[format-expr -> start]
             ,[format-expr -> end]
             ,[format-expr -> step])
         ,stmt)
       (format-append
         (indent-before
           (format-append
             "for(int " i " = " start "; " i " < " end "; " i "= (" i " + " step "))\n"))
         (indent-more (format-stmt stmt))))
      ((do ,[format-expr -> e])
       (indent-before (format-append e ";")))
      (,else (error 'format-stmt "could not format" else))))
  (define stmt-fns (make-parameter `(,format-stmt-default)))
  (define format-stmt (format-sexp stmt-fns))

  (define (format-expr-default expr _)
    (match expr
      ((empty-struct) "{0}")
      ((field ,[obj] ,x)
       (format-append obj "." (format-ident x)))
      ((field ,[obj] ,x ,[format-type -> t])
       (format-append obj "." (format-ident x) "<" t ">"))
      ((if ,[format-expr -> test]
           ,[format-expr -> conseq]
           ,[format-expr -> alt])
       (format-append "(" test ") ? (" conseq ") : (" alt ")"))
      ((vector-ref ,[format-expr -> v]
         ,[format-expr -> i])
       (format-append v "[" i "]"))
      ((sizeof ,[format-type -> t])
       (format-append "sizeof(" t ")"))
      ((deref ,[format-expr -> p])
       (format-append "*" p))
      ((cast ,[format-type -> t] ,[e])
       (format-append "((" t ")(" e "))"))
      ((addressof ,[format-expr -> e])
       (format-append "(&(" e "))"))
      ((,op ,[format-expr -> lhs] ,[format-expr -> rhs])
       (guard (binop? op))
       (format-append "(" lhs ") " (format-binop op) " (" rhs ")"))
      ((,op ,[format-expr -> lhs] ,[format-expr -> rhs])
       (guard (relop? op))
       (format-append "(" lhs ") " (format-relop op) " (" rhs ")"))
      ((not ,[format-expr -> lhs])
       (format-append "!(" lhs ")"))
      ((assert ,[format-expr -> expr])
       (format-append "assert(" expr ")"))
      ((bool ,b) (if (not b) "false" "true"))
      ((var ,var) (format-ident var))
      ((char ,c) (format-char-literal c))
      ((int ,n) (number->string n))
      ((u64 ,n) (format-append (number->string n) "u"))
      ((str ,s) (format-append "\"" (escape-string-literal s) "\""))
      ((float ,f) (number->string f))
      ((c-expr ,x) (symbol->string x))
      ((call ,[format-expr -> f] . ,[format-call-args -> args])
       (format-append f "(" args ")"))
      (,else (error 'format-expr "could not format" else))))
  (define expr-fns (make-parameter `(,format-expr-default)))
  (define format-expr (format-sexp expr-fns))

  (define (mangle-ident x)
    (let ((y ""))
      (define (push c)
        (set! y (format-append y c)))
      (string-for-each
       (lambda (c)
         (case c
           ((#\-) (push "$d"))
           ((#\$) (push "$$"))
           ((#\.) (push "$_"))
           ((#\$) (push "$s"))
           ((#\>) (push "$v"))
           ((#\!) (push "$b"))
           ((#\?) (push "$p"))
           ((#\*) (push "$x"))
           ((#\^) (push "$c"))
           (else (push (string c)))))
       x)
      y))
  
  (define (format-ident-default ident _)
    (unless (symbol? ident)
      (error 'format-ident "could not format" ident))
    (let ((reserved-words '(complex)))
      (if (memq ident reserved-words)
          (format-append "$$" (symbol->string ident))
          (mangle-ident (symbol->string ident)))))
  
  (define ident-fns (make-parameter `(,format-ident-default)))
  (define format-ident (format-sexp ident-fns))

  (define (format-type-default t _)
    (match t
      (u64 "uint64_t")
      ((ptr ,[t])
       (format-append "__global " t " *"))
      ((ptr local ,[t])
       (format-append "__local " t " *"))
      ((ptr private ,[t])
       (format-append "__private " t " *"))
      ((ref ,[t])
       (format-append t " &"))
      ((const-ptr ,[t])
       (format-append "__global const " t " *"))
      ((fixed-array ,t ,i)
       (error 'format-type-default
              "Directly formatting fixed-size arrays is a bad idea."
              `(fixed-array ,t ,i)))
      ((struct (,[format-ident -> x] ,[t]) ...)
       (format-append "struct {\n"
                      (indent-more
                       (format-join "" (map (lambda (x t)
                                       (indent-before
                                        (format-append t " " x ";\n")))
                                     x t)))
                      "}"))
      ((union (,[format-ident -> x] ,[t]) ...)
       (format-append "union {\n"
                      (indent-more
                       (format-join "" (map (lambda (x t)
                                       (indent-before
                                        (format-append t " " x ";\n")))
                                     x t)))
                      "}"))
      ((,[t] ,[t*] ...)
       (if (null? t*)
           t
           (format-append t "< " (format-join ", " t*) " >")))
      (,x (guard (symbol? x))
          (format-ident x))
      (,else (error 'format-type "could not format" else))))
  (define type-fns (make-parameter `(,format-type-default)))
  (define format-type (format-sexp type-fns))

  (define (format-args args)
    (format-join ", " (map format-arg args)))

  (define (format-call-args args)
    (format-join ", " (map format-expr args)))
  
  (define (format-arg-default arg _)
    (match arg
      ((,[format-ident -> x] ,[format-type -> t])
       (format-append t " " x))
      (,else (error 'format-arg "could not format" else))))
  (define arg-fns (make-parameter `(,format-arg-default)))
  (define format-arg (format-sexp arg-fns))

  (define (format-binop-default op _)
    (case op
      ((bitwise-or) "|")
      ((bitwise-xor) "^")
      ((+) "+")
      ((*) "*")
      ((-) "-")
      ((/) "/")
      ((<<) "<<")
      ((>>) ">>")
      ((mod) "%")
      ((or) "||")
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
          (format-append
            (case (string-ref s 0)
              ((#\newline) "\\n\"\n\"")
              ((#\") "\\\"")
              (else (string (string-ref s 0))))
            (escape-string-literal
              (substring s 1 (string-length s)))))))

  (define (format-char-literal c)
    (format-append "'"
                   (case c
                     ((#\nul) "\\0")
                     (else (string c)))
                   "'"))
  
)

