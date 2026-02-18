;;; play-repl.scm — interactive terminal REPL for (metaorg play)
;;; Run with:  guile -L . play-repl.scm
;;;        or: rlwrap guile -L . play-repl.scm   (for line-editing + history)

(add-to-load-path ".")
(use-modules (metaorg play))

;;; ── Base environment ────────────────────────────────────────────────────────

(define base-bindings
  `((+         . ,+)
    (-         . ,-)
    (*         . ,*)
    (/         . ,/)
    (>         . ,>)
    (<         . ,<)
    (=         . ,=)
    (>=        . ,>=)
    (<=        . ,<=)
    (not       . ,not)
    (equal?    . ,equal?)
    (null?     . ,null?)
    (cons      . ,cons)
    (car       . ,car)
    (cdr       . ,cdr)
    (list      . ,list)
    (number?   . ,number?)
    (string?   . ,string?)
    (symbol?   . ,symbol?)
    (display   . ,display)
    (newline   . ,newline)
    (env-history  . ,env-history)
    (env-bindings . ,env-bindings)))

;;; ── Mutable REPL state ──────────────────────────────────────────────────────

(define *env* (make-initial-env base-bindings))

;;; ── Core eval step ──────────────────────────────────────────────────────────
;;; Returns zero values when the result is an env (state update, no display),
;;; or the computed value(s) otherwise.

(define (play-eval! expr)
  (let ((result (evaluate expr *env*)))
    (if (env? result)
        (begin (set! *env* result) (values))
        result)))

;;; ── Prompt helpers ──────────────────────────────────────────────────────────

(define (current-identity)
  (or (expressor-name (env-expressor *env*)) '?))

(define (show-prompt)
  (display (string-append "play[" (symbol->string (current-identity)) "]> "))
  (force-output))

;;; ── Main loop ───────────────────────────────────────────────────────────────

(define (repl)
  (display "play.scm REPL  —  Ctrl-D or (quit) to exit\n\n")
  (let loop ()
    (show-prompt)
    (let ((expr (catch #t
                  (lambda () (read))
                  (lambda (key . args)
                    (display (string-append "\nread error: "
                                           (symbol->string key) "\n"))
                    'read-error))))
      (cond
       ((eof-object? expr)
        (display "\nBye.\n"))
       ((eq? expr 'read-error)
        (loop))
       ((equal? expr '(quit))
        (display "Bye.\n"))
       (else
        (catch #t
          (lambda ()
            (call-with-values
                (lambda () (play-eval! expr))
              (lambda vals
                (for-each (lambda (v)
                            (display "=> ")
                            (write v)
                            (newline))
                          vals))))
          (lambda (key . args)
            (display (string-append "error: " (symbol->string key) "\n"))
            (for-each (lambda (a)
                        (display "  ")
                        (display a)
                        (newline))
                      args)))
        (loop))))))

(repl)
