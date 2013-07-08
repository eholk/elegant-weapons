(library
    (elegant-weapons insert-prototypes)
  (export insert-prototypes)
  (import
   (rnrs)
   (elegant-weapons helpers)
   (elegant-weapons match))

  ;; This pass takes a parenthesized C++ program and inserts the
  ;; necessary function prototypes to make sure all the names work out
  ;; right.
  
  (define-match insert-prototypes
    ((,[Decl -> typedefs prototypes funcs] ...)
     `(,@(apply append typedefs)
       ,@(apply append prototypes)
       . ,(apply append funcs))))

  (define-match Decl
    ((func ,t ,name ,args . ,stmt*)
     (values '() `((func ,t ,name ,args)) `((func ,t ,name ,args . ,stmt*))))
    ((kernel . ,_)
     (values '() '() `((kernel . ,_))))
    (,else (values (list else) '() '())))
    
  )
