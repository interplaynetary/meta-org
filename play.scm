(define-module (metaorg play)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (make-expressor
            expressor?
            expressor-name
            expressor-key
            make-env
            env-bindings
            env-validator
            env-history
            env-expressor
            make-history-entry
            history-version
            history-expressor
            history-expr
            history-parent
            next-version
            env-modify
            env-enact
            env-rollback
            default-validator
            default-history
            make-initial-env
            with-expressors
            evaluate
            authenticate?))

;;; Identity management - pure and elegant
(define *expressors* (make-weak-key-hash-table))

(define (env-lookup env name)
  (match (assoc name (env-bindings env))
    ((_key . val) val)
    (_ (error "Variable unbound:" name))))

(define (extend-env base-bindings names vals)
  (if (null? names)
      base-bindings
      (cons (cons (car names) (car vals))
            (extend-env base-bindings (cdr names) (cdr vals)))))

(define (make-expressor name public-key)
  (let ((id (gensym (string-append (symbol->string name) "-"))))
    (hashq-set! *expressors* id 
                `((id . ,id)
                  (name . ,name)
                  (public-key . ,public-key)
                  (created . ,(current-time))))
    id))

(define (expressor? x) (hashq-ref *expressors* x))
(define (expressor-name id)
  (let ((data (hashq-ref *expressors* id)))
    (and data (assq-ref data 'name))))
(define (expressor-key id)
  (let ((data (hashq-ref *expressors* id)))
    (and data (assq-ref data 'public-key))))

;;; Environment now tracks the current expressor
(define (make-env bindings validator history expressor)
  (list bindings validator history expressor))

(define (env-bindings env) (car env))
(define (env-validator env) (cadr env))
(define (env-history env) (caddr env))
(define (env-expressor env) (cadddr env))

;;; History entries: (version . (expressor . (expr . parent)))
(define (make-history-entry version expressor expr parent)
  (cons version (cons expressor (cons expr parent))))

(define (history-version entry) (car entry))
(define (history-expressor entry) (cadr entry))
(define (history-expr entry) (caddr entry))
(define (history-parent entry) (cdddr entry))

;;; Version management
(define *version-counter* 0)
(define (next-version)
  (set! *version-counter* (+ *version-counter* 1))
  *version-counter*)

;;; Core operations now take explicit expressor
(define (env-modify env expressor name value expr)
  (let* ((validator (env-validator env))
         (history (env-history env))
         (version (next-version))
         (new-bindings (cons (cons name value) (env-bindings env)))
         (new-entry (make-history-entry version expressor expr (caar history)))
         (new-history (cons new-entry history))
         (post-env (make-env new-bindings validator new-history expressor)))
    (if (validator expressor name value env post-env)
        post-env
        (error "Validation failed:" expressor name value))))

(define (env-enact env expressor validator-expr expr)
  (let* ((new-validator (evaluate validator-expr env))
         (version (next-version))
         (new-entry (make-history-entry version expressor expr
                                       (caar (env-history env))))
         (new-history (cons new-entry (env-history env)))
         (post-env (make-env (env-bindings env)
                             new-validator
                             new-history
                             expressor)))
    (if (new-validator expressor 'validator new-validator env post-env)
        post-env
        (error "Validator rejected by itself:" expressor new-validator))))

;;; Default validator - now takes expressor as first argument
(define (default-validator expressor name value pre-env post-env)
  #t)  ; Trust no one? Actually trust everyone by default

(define (default-history)
  (list (make-history-entry 0 'genesis '(quote initial) 0)))

(define (make-initial-env base-bindings)
  (let ((genesis (make-expressor 'genesis "genesis-key")))
    (make-env base-bindings default-validator 
              (default-history) genesis)))

;;; The elegant part: "with-expressors" wraps expressions with identity
(define-syntax with-expressors
  (syntax-rules ()
    ((_ (expressor-id expr ...) body ...)
     (let ((current-expressor expressor-id))
       (evaluate '(begin expr ...) 
                (make-env (env-bindings env)
                         (env-validator env)
                         (env-history env)
                         current-expressor))))))

(define (authenticate? expressor)
  #t) ; Trust everyone for now

;;; Rollback with expressor identity
(define (env-rollback env target-version expressor)
  (let* ((history (env-history env))
         (validator (env-validator env))
         (target-entry (find (lambda (e) 
                               (= (history-version e) target-version))
                            history)))
    (if (not target-entry)
        (error "Version not found:" target-version)
        (let replay ((vers (reverse history)) 
                    (current-env env))
          (if (null? vers)
              current-env
              (let* ((entry (car vers))
                     (ver (history-version entry))
                     (expr (history-expr entry))
                     (entry-expressor (history-expressor entry)))
                (if (= ver target-version)
                    current-env
                    (replay (cdr vers)
                           ;; Replay with original expressor!
                           (evaluate expr 
                                    (make-env (env-bindings current-env)
                                             (env-validator current-env)
                                             (env-history current-env)
                                             entry-expressor))))))))))

;;; The enhanced evaluator - minimal changes, maximum power
(define (evaluate expr env)
  (match expr
    ((or #t #f (? number?) (? string?)) expr) ; Added string? safety
    (('quote quoted-expr) quoted-expr)
    ((? symbol? name) (env-lookup env name))
    (('if test consequent alternate)
     (if (evaluate test env)
         (evaluate consequent env)
         (evaluate alternate env)))
    
    ;; Lambda captures current expressor
    (('lambda (args ...) body)
     (let ((current-validator (env-validator env))
           (current-history (env-history env))
           (current-expressor (env-expressor env)))
       (lambda vals
         (evaluate body
                   (make-env (extend-env (env-bindings env) args vals)
                            current-validator
                            current-history
                            current-expressor)))))
    
    ;; NEW: Create an expressor (identity)
    (('defexpressor name key)
     (let ((id (make-expressor (evaluate name env) 
                              (evaluate key env))))
       (env-modify env (env-expressor env) 
                  `,name id
                  `(defexpressor ,name ,key))))
    
    ;; NEW: Switch expressor (with authentication)
    (('as expressor-id expr)
     (let ((target-expressor (evaluate expressor-id env)))
       (if (and (expressor? target-expressor)
                (authenticate? target-expressor))
           (evaluate expr 
                    (make-env (env-bindings env)
                             (env-validator env)
                             (env-history env)
                             target-expressor))
           (error "Authentication failed:" expressor-id))))
    
    ;; Modify - now records expressor
    (('modify! name value)
     (env-modify env (env-expressor env)
                 (evaluate name env)
                 (evaluate value env)
                 `(modify! ,name ,value)))
    
    ;; Enact - now records expressor
    (('enact! validator-expr)
     (env-enact env (env-expressor env)
                validator-expr
                `(enact! ,validator-expr)))
    
    ;; Rollback - now requires expressor validation
    (('rollback! version)
     (let* ((target-version (evaluate version env))
            (expressor (env-expressor env))
            (validator (env-validator env))
            (rolled-back (env-rollback env target-version expressor)))
       (if (validator expressor 'rollback target-version
                     (env-bindings rolled-back) env)
           rolled-back
           (error "Rollback rejected by validator:" target-version))))
    
    ;; Show history with expressors
    (('show-history)
     (let ((history (env-history env)))
       (for-each (lambda (entry)
                   (display "Version ")
                   (display (history-version entry))
                   (display " [")
                   (display (expressor-name (history-expressor entry)))
                   (display "] : ")
                   (display (history-expr entry))
                   (display " (parent: ")
                   (display (history-parent entry))
                   (display ")")
                   (newline))
                 (reverse history))
       env))
    
    ;; Show expressors
    (('show-expressors)
     (hash-for-each (lambda (id data)
                      (display (assq-ref data 'name))
                      (display " (")
                      (display id)
                      (display ")\n"))
                    *expressors*)
     env)
    
    ;; Application
    ((proc-expr arg-exprs ...)
     (apply (evaluate proc-expr env)
            (map (lambda (arg) (evaluate arg env)) arg-exprs)))))
