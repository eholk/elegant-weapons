(library
    (elegant-weapons compat)
  (export add1 sub1 syntax-error make-parameter parameterize
    last-pair make-list void)
  (import (only (chezscheme) add1 sub1 syntax-error make-parameter
                parameterize last-pair make-list void))
  )
