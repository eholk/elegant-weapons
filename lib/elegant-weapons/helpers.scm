(library
  (elegant-weapons helpers)
  (export
    gensym
    iota
    define-match
    andmap
    ormap
    map-values
    ident?
    binop?
    relop?
    float?
    scalar-type?
    c-type?
    cl-type?
    join
    make-begin
    symbol-append

    ;; match exports
    match trace-match 
    match+ trace-match+
    match/lexical-context trace-match/lexical-context
    match-equality-test
    guard ... quasiquote unquote unquote-splicing ->)
  (import
    (rnrs)
    (elegant-weapons match))

(define make-begin
  (lambda (exp*)
    (match
      (match `(begin . ,exp*)
        ((begin ,[x] ...)
         (apply append x))
        (,x `(,x)))
      ((,x) x)
      ((,x ...) `(begin . ,x)))))

(define join
  (lambda (sep strings)
    (match strings
      (() "")
      ((,a) a)
      ((,a ,b) (string-append a sep b))
      ((,a ,b* ...)
       (string-append a sep (join sep b*))))))

;; From http://scheme.com/csug8/syntax.html#./syntax:s11
(define-syntax with-implicit
  (syntax-rules ()
    [(_ (tid id ...) b1 b2 ...)
     (with-syntax ([id (datum->syntax #'tid 'id)] ...)
       b1 b2 ...)]))

;; This abstracts away most of the boilerplate for writing
;; match-based transformations. A little unsafe.
;; Stipulation: make sure the match macro is in the environment
;; everywhere this macro is used.
(define-syntax define-match
  (lambda (x)
    (syntax-case x ()
      ((k (name args ...) clauses ...)
       (with-implicit (k match)
                      #'(define (name args ...)
                          (lambda (arg)
                            (match arg
                              clauses ...
                              (,else
                               (error 'name "Unrecognized item" else)))))))
      ((k name clauses ...)
       (with-implicit (k match)
                      #'(define name
                          (lambda (arg)
                            (match arg
                              clauses ...
                              (,else
                               (error 'name "Unrecognized item" else))))))))))

(define gensym
  (let ((c 0))
    (lambda (x)
      (unless (symbol? x) (error 'gensym "invalid symbol" x))
      (set! c (+ 1 c))
      (string->symbol
        (string-append
          (symbol->string x) "_" (number->string c))))))

(define iota
  (lambda (n)
    (let loop ([i 0])
      (cond
        [(= i n) '()]
        [else (cons i (loop (+ i 1)))]))))

(define andmap for-all)

(define ormap
  (lambda (p ls)
    (and (not (null? ls))
         (or (p (car ls)) (ormap p (cdr ls))))))

(define map-values
  (lambda (p . ls)
    ;; We require at least one element
    (if (andmap (lambda (x) (null? (cdr x))) ls)
        (let-values ((v* (apply p (map car ls))))
          (apply values (map list v*)))
        (let-values ((rest* (apply map-values p (map cdr ls)))
                     (this* (apply p (map car ls))))
          (apply values (map cons this* rest*))))))

(define ident? symbol?)

(define binop?
  (lambda (op)
    (case op
      ((bitwise-or + * - mod /) #t)
      (else #f))))

(define relop?
  (lambda (op)
    (case op
      ((< <= = > >=) #t)
      (else #f))))

(define (float? n)
  (and (number? n) (inexact? n)))

(define (scalar-type? t)
  (case t
    ((int u64 void str float bool char) #t)
    (else #f)))

(define (c-type? t)
  (case t
    ((int uint64_t void float char bool cl_mem)
     #t)
    (else #f)))

(define (cl-type? t)
  (case t
    ((std::ofstream cl::queue cl::kernel cl::program) #t)
    (else #f)))

(define (symbol-append . syms)
  (string->symbol
   (apply string-append (map symbol->string syms))))
)
