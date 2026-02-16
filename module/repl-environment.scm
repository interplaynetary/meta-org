(library (repl-environment)
  (export repl-environment)
  (import (except (guile) - / = < <= = >= >)
          (prefix (only (guile) - / < <= = > >=) %)
          (only (hoot modules) current-module))

  ;; Some of the arithmetic and comparison operators are macros, which
  ;; don't work with Hoot's eval yet.  So, we use their identifier
  ;; syntax to residualize them to procedures here.
  (define - %-)
  (define / %/)
  (define < %<)
  (define <= %<=)
  (define = %=)
  (define >= %>=)
  (define > %>)

  (define (repl-environment)
    (current-module)))
