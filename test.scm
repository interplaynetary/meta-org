;;; test.scm — tests for (metaorg play)
;;; Run with:  guile -L . test.scm

(add-to-load-path ".")
(use-modules (metaorg play))

;;; ── Harness ─────────────────────────────────────────────────────────────────

(define pass-count 0)
(define fail-count 0)

(define (check label got expected)
  (if (equal? got expected)
      (begin (set! pass-count (+ pass-count 1))
             (display (string-append "  PASS  " label "\n")))
      (begin (set! fail-count (+ fail-count 1))
             (display (string-append "  FAIL  " label "\n"))
             (display "    expected: ") (write expected) (newline)
             (display "    got:      ") (write got)      (newline))))

(define-syntax check-error
  (syntax-rules ()
    ((_ label body ...)
     (check label
            (catch #t
              (lambda () body ... #f)
              (lambda (key . args) #t))
            #t))))

;;; Base env — Scheme primitives exposed to the meta-evaluator
(define base-env
  (make-initial-env `((+          . ,+)
                      (-          . ,-)
                      (*          . ,*)
                      (>          . ,>)
                      (equal?     . ,equal?)
                      (not        . ,not)
                      (length     . ,length)
                      (env-history  . ,env-history)
                      (env-bindings . ,env-bindings))))

;;; Helper: inject a Scheme value as a binding without going through evaluate
(define (inject-binding env name val)
  (env-modify env (env-expressor env) name val `(inject ,name)))

;;; ── 1. Expressor accessors ──────────────────────────────────────────────────

(display "\n── 1. expressor accessors ──\n")

(let ((id (make-expressor 'alice "alice-key")))
  (check "expressor? recognizes valid id"        (if (expressor? id) #t #f) #t)
  (check "expressor-name returns the name symbol" (expressor-name id) 'alice)
  (check "expressor-key returns the key string"   (expressor-key id) "alice-key"))

(check "expressor-name on non-expressor returns #f"
       (expressor-name 'not-an-expressor) #f)
(check "expressor-key on non-expressor returns #f"
       (expressor-key 'not-an-expressor) #f)

;;; ── 2. extend-env / lambda closures ─────────────────────────────────────────

(display "\n── 2. lambda closures (extend-env) ──\n")

(let* ((env1 (evaluate '(modify! 'outer 99) base-env))
       (fn   (evaluate '(lambda (x) (+ x outer)) env1)))
  (check "lambda closes over outer binding" (fn 1) 100))

(let* ((env1 (evaluate '(modify! 'a 10) base-env))
       (env2 (evaluate '(modify! 'b 20) env1))
       (fn   (evaluate '(lambda (x) (+ x a b)) env2)))
  (check "lambda closes over multiple outer bindings" (fn 5) 35))

;; Parameter binding shadows outer binding
(let* ((env1 (evaluate '(modify! 'x 100) base-env))
       (fn   (evaluate '(lambda (x) x) env1)))
  (check "parameter shadows outer binding of same name" (fn 7) 7))

;;; ── 3. modify! and lookup ───────────────────────────────────────────────────

(display "\n── 3. modify! and lookup ──\n")

(let* ((env1 (evaluate '(modify! 'x 42) base-env)))
  (check "modify! stores value"       (evaluate 'x env1) 42))

(let* ((env1 (evaluate '(modify! 'x 42) base-env))
       (env2 (evaluate '(modify! 'x 99) env1)))
  (check "later modify! shadows earlier" (evaluate 'x env2) 99))

(check-error "unbound variable signals error"
  (evaluate 'undefined-var base-env))

;;; ── 4. Validator: pre-env / post-env signature ──────────────────────────────

(display "\n── 4. validator pre-env / post-env ──\n")

;; Validator that verifies history grows on every transition
(let* ((env-v (evaluate '(enact! (lambda (expressor name value pre-env post-env)
                                   (> (length (env-history post-env))
                                      (length (env-history pre-env)))))
                        base-env)))
  (check "enact! itself passes the history-growth validator"
         (if env-v #t #f) #t)
  (let ((env-m (evaluate '(modify! 'y 7) env-v)))
    (check "modify! under history-growth validator succeeds"
           (evaluate 'y env-m) 7)))

;; Validator that rejects its own installation
(check-error "validator returning #f rejects its own enact!"
  (evaluate '(enact! (lambda (expressor name value pre-env post-env) #f))
            base-env))

;; Direct Scheme-level test: env-modify passes correct pre and post envs.
;; We can't easily test this through evaluate (would need or/set! in meta-env),
;; so we install a capturing validator at the Scheme level.
(let* ((pre-seen  #f)
       (post-seen #f)
       (capturing (lambda (expressor name value pre-env post-env)
                    (set! pre-seen  pre-env)
                    (set! post-seen post-env)
                    #t))
       (env0 (make-env '() capturing (default-history)
                       (make-expressor 'tester "k")))
       (env1 (env-modify env0 (env-expressor env0) 'x 42 '(test))))
  (check "validator receives pre-env with empty bindings"
         (null? (env-bindings pre-seen)) #t)
  (check "validator receives post-env with x=42"
         (assoc 'x (env-bindings post-seen)) '(x . 42))
  (check "pre-env and post-env are distinct"
         (equal? (env-bindings pre-seen) (env-bindings post-seen)) #f))

;;; ── 5. as: identity switching ───────────────────────────────────────────────
;;; Note: (defexpressor 'name key) has a known binding bug — it stores the key
;;; as the list (quote name) rather than the symbol name, so the env-lookup by
;;; symbol fails.  We inject expressors directly via env-modify to test 'as'.

(display "\n── 5. as / identity switching ──\n")

(let* ((alice (make-expressor 'alice "key-a"))
       (env1  (inject-binding base-env 'alice alice))
       (env2  (evaluate '(as alice (modify! 'greeting "hello")) env1))
       (entry (car (env-history env2))))
  (check "modify! value is stored under as"
         (evaluate 'greeting env2) "hello")
  (check "history entry records alice as the acting expressor"
         (expressor-name (history-expressor entry)) 'alice))

;; modify! on fresh base-env records genesis
(let* ((env1  (evaluate '(modify! 'a 1) base-env))
       (entry (car (env-history env1))))
  (check "modify! without as records genesis in fresh env"
         (expressor-name (history-expressor entry)) 'genesis))

;; The expressor set by 'as' persists in the returned env —
;; env-modify returns make-env with the acting expressor, so subsequent
;; modify! calls on that env continue under alice.
(let* ((alice (make-expressor 'alice "key-a"))
       (env1  (inject-binding base-env 'alice alice))
       (env2  (evaluate '(as alice (modify! 'a 1)) env1))
       (env3  (evaluate '(modify! 'b 2) env2))
       (entry (car (env-history env3))))
  (check "expressor set by as persists in returned env"
         (expressor-name (history-expressor entry)) 'alice))

;; as with unbound name signals error
(check-error "as with unbound name signals error"
  (evaluate '(as nobody (modify! 'x 1)) base-env))

;;; ── 6. show-history: names, not alists ──────────────────────────────────────
;;; The old accessor bug caused expressor-name to return the full alist rather
;;; than the name symbol.  Verify the accessor fix indirectly through history.

(display "\n── 6. expressor-name in history ──\n")

(let* ((bob   (make-expressor 'bob "key-b"))
       (env1  (inject-binding base-env 'bob bob))
       (env2  (evaluate '(as bob (modify! 'flag #t)) env1))
       (entry (car (env-history env2))))
  (check "history-expressor entry holds an expressor id (not a symbol)"
         (if (expressor? (history-expressor entry)) #t #f) #t)
  (check "expressor-name on that id returns the symbol 'bob"
         (expressor-name (history-expressor entry)) 'bob)
  (check "result is a symbol, not a list"
         (symbol? (expressor-name (history-expressor entry))) #t))

;;; ── 7. Lambda carries caller's authority, not author's ──────────────────────

(display "\n── 7. logic vs authority ──\n")

;; Alice defines a lambda that writes to 'target.
;; Bob calls it. History must record Bob, not Alice.
(let* ((alice (make-expressor 'alice "key-a"))
       (bob   (make-expressor 'bob   "key-b"))
       (env0  (inject-binding (inject-binding base-env 'alice alice) 'bob bob))
       ;; Alice defines the modifier
       (env1  (evaluate '(as alice (modify! 'modifier (lambda (x) (modify! 'target x)))) env0))
       ;; Bob calls it
       (env2  (evaluate '(as bob (modifier 42)) env1))
       ;; Most recent history entry is the (modify! 'target 42)
       (entry (car (env-history env2))))
  (check "value is stored correctly via cross-identity call"
         (evaluate 'target env2) 42)
  (check "lambda called by bob attributes modify! to bob, not alice"
         (expressor-name (history-expressor entry)) 'bob))

;;; ── Summary ─────────────────────────────────────────────────────────────────

(display "\n")
(display (string-append "Results: "
                        (number->string pass-count) " passed, "
                        (number->string fail-count) " failed\n"))
(when (> fail-count 0) (exit 1))
