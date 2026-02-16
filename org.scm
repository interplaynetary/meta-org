(use-modules (ice-9 match)
             (srfi srfi-1))  ; For list utilities

;;; A versioned environment: (bindings validator history)
;;; history: ((version-id . (expr . parent-version)) ...)
(define (make-env bindings validator history)
  (list bindings validator history))

(define (env-bindings env) (car env))
(define (env-validator env) (cadr env))
(define (env-history env) (caddr env))

(define (env-lookup env name)
  (match (assoc name (env-bindings env))
    ((_key . val) val)
    (_ (error "Variable unbound:" name))))

(define (extend-env env names vals)
  (if (null? names)
      (env-bindings env)
      (cons (cons (car names) (car vals))
            (extend-env env (cdr names) (cdr vals)))))

;;; Generate unique version identifiers
(define *version-counter* 0)
(define (next-version)
  (set! *version-counter* (+ *version-counter* 1))
  *version-counter*)

;;; Safe environment modification with versioning
(define (env-modify env name value expr)
  (let* ((bindings (env-bindings env))
         (validator (env-validator env))
         (history (env-history env))
         (version (next-version))
         (new-bindings (cons (cons name value) bindings))
         (new-history (cons (cons version (cons expr (caar history))) history)))
    (if (validator name value new-bindings env)
        (make-env new-bindings validator new-history)
        (error "Validation failed:" name value))))

;;; Enact new validator with versioning
(define (env-enact env validator-expr expr)
  (let* ((new-validator (evaluate validator-expr env))
         (bindings (env-bindings env))
         (history (env-history env))
         (version (next-version))
         (new-history (cons (cons version (cons expr (caar history))) history)))
    (if (new-validator 'validator new-validator bindings env)
        (make-env bindings new-validator new-history)
        (error "Validator rejected by itself:" new-validator))))

;;; Rollback to a previous version
(define (env-rollback env target-version)
  (let* ((history (env-history env))
         (validator (env-validator env))
         ;; Replay all expressions up to target version
         (target-entry (find (lambda (entry) (= (car entry) target-version))
                            history))
         (target-expr (cadr target-entry))
         (parent-version (cddr target-entry)))
    (if (not target-entry)
        (error "Version not found:" target-version)
        ;; Reconstruct environment by replaying from the beginning
        (let replay ((vers (reverse history)) (current-env env))
          (if (null? vers)
              current-env
              (let* ((entry (car vers))
                     (ver (car entry))
                     (expr (cadr entry)))
                (if (= ver target-version)
                    current-env
                    (replay (cdr vers) 
                           (evaluate expr current-env)))))))))

;;; Default validator (allows rollback by default)
(define (default-validator name value new-bindings old-env)
  (or (not (eq? name 'rollback))  ; Allow rollback operations
      #t))

(define (default-history)
  (list (cons 0 (cons '(quote initial) 0))))

(define (make-initial-env base-bindings)
  (make-env base-bindings default-validator (default-history)))

;;; The temporal evaluator with memory and rollback
(define (evaluate expr env)
  (match expr
    ((or #t #f (? number?)) expr)
    (('quote quoted-expr) quoted-expr)
    ((? symbol? name) (env-lookup env name))
    (('if test consequent alternate)
     (if (evaluate test env)
         (evaluate consequent env)
         (evaluate alternate env)))
    
    (('lambda (args ...) body)
     (let ((current-validator (env-validator env))
           (current-history (env-history env)))
       (lambda vals
         (evaluate body 
                   (make-env (extend-env env args vals)
                            current-validator
                            current-history)))))
    
    ;; Modify with expression recording
    (('modify! name value)
     (env-modify env 
                 (evaluate name env) 
                 (evaluate value env)
                 `(modify! ,name ,value)))
    
    ;; Enact with expression recording
    (('enact! validator-expr)
     (env-enact env 
                validator-expr
                `(enact! ,validator-expr)))
    
    ;; NEW: Rollback to a specific version
    (('rollback! version)
     (let* ((target-version (evaluate version env))
            (validator (env-validator env))
            (current-bindings (env-bindings env))
            (rolled-back (env-rollback env target-version)))
       ;; Even rollback must be validated!
       (if (validator 'rollback target-version 
                     (env-bindings rolled-back) env)
           rolled-back
           (error "Rollback rejected by validator:" target-version))))
    
    ;; NEW: Show the DAG of versions
    (('show-history)
     (let ((history (env-history env)))
       (for-each (lambda (entry)
                   (display "Version ")
                   (display (car entry))
                   (display ": ")
                   (display (cadr entry))
                   (display " (parent: ")
                   (display (cddr entry))
                   (display ")")
                   (newline))
                 (reverse history))
       env))
    
    ;; Procedure application
    ((proc-expr arg-exprs ...)
     (apply (evaluate proc-expr env)
            (map (lambda (arg) (evaluate arg env)) arg-exprs)))))

;;; Now let's build a temporal organization with full history DAG
(define temporal-society
  (evaluate
   '(begin
      ;; Version 1: Enact constitution with rollback rights
      (display "=== Version 1: Initial Constitution ===\n")
      (enact! (lambda (name value new-bindings old-env)
                (display "Validator: Can we change ") (display name) 
                (display "? ") 
                (let ((allowed (or (eq? name 'public-counter)
                                  (eq? name 'version)
                                  (eq? name 'rollback)
                                  (eq? name 'validator))))
                  (display allowed) (newline)
                  allowed)))
      
      ;; Version 2: Add some data
      (display "\n=== Version 2: Adding public-counter ===\n")
      (modify! 'public-counter 0)
      (modify! 'version 2)
      
      ;; Version 3: Increment counter
      (display "\n=== Version 3: Incrementing ===\n")
      (modify! 'public-counter (+ (env-lookup (make-env (env-bindings env) 
                                                       (env-validator env) 
                                                       (env-history env)) 
                                             'public-counter) 1))
      (modify! 'version 3)
      
      ;; Version 4: Increment again
      (display "\n=== Version 4: Incrementing again ===\n")
      (modify! 'public-counter (+ (env-lookup (make-env (env-bindings env) 
                                                       (env-validator env) 
                                                       (env-history env)) 
                                             'public-counter) 1))
      (modify! 'version 4)
      
      ;; Show the DAG
      (display "\n=== Complete Version History ===\n")
      (show-history)
      
      ;; Version 5: Try to restrict rollback (validator self-modification)
      (display "\n=== Version 5: Attempting to restrict rollback ===\n")
      (enact! (lambda (name value new-bindings old-env)
                (display "New validator: Can we change ") (display name) 
                (display "? ")
                ;; This validator tries to disallow rollback!
                (let ((allowed (or (eq? name 'public-counter)
                                  (eq? name 'version)
                                  (eq? name 'validator))))
                  (display allowed) (newline)
                  allowed)))
      
      ;; Show we're at version 5
      (display "\n=== Current State (Version 5) ===\n")
      (display "Current counter: ") 
      (display (env-lookup (make-env (env-bindings env) 
                                    (env-validator env) 
                                    (env-history env)) 
                          'public-counter))
      (newline)
      (display "Current version: ")
      (display (env-lookup (make-env (env-bindings env) 
                                    (env-validator env) 
                                    (env-history env)) 
                          'version))
      (newline))
      
   (make-initial-env `((+ . ,+)
                       (- . ,-)
                       (display . ,display)
                       (newline . ,newline)))))

;;; Now let's ROLLBACK to version 3!
(display "\n\n*** TIME TRAVEL ***\n")
(define rolled-back-society
  (evaluate
   '(begin
      (display "\n=== Rolling back to Version 3 ===\n")
      (rollback! 3)
      (display "Counter after rollback: ")
      (display (env-lookup (make-env (env-bindings env) 
                                    (env-validator env) 
                                    (env-history env)) 
                          'public-counter))
      (newline)
      (display "Version after rollback: ")
      (display (env-lookup (make-env (env-bindings env) 
                                    (env-validator env) 
                                    (env-history env)) 
                          'version))
      (newline)
      
      ;; Version 6: Branch from version 3!
      (display "\n=== Version 6: Alternative timeline from v3 ===\n")
      (modify! 'public-counter 100)
      (modify! 'version 6)
      
      ;; Show the new DAG with branch
      (display "\n=== History with Branch ===\n")
      (show-history))
   temporal-society))

;;; Advanced: Query the DAG
(define (version-dag env)
  (let ((history (env-history env)))
    (map (lambda (entry)
           (cons (car entry)  ; version
                 (cddr entry))) ; parent
         (reverse history))))

;;; Find all paths to a version
(define (paths-to-version env target-version)
  (let* ((history (env-history env))
         (dag (version-dag env)))
    (let rec ((version target-version) (path '()))
      (cons version path)
      (if (= version 0)
          (list (cons version path))
          (append-map (lambda (entry)
                       (if (= (cdr entry) version)
                           (rec (car entry) (cons version path))
                           '()))
                     dag)))))

;;; Bonus: Immutable history query language
(define-syntax with-history
  (syntax-rules ()
    ((_ env expr ...)
     (let ((snapshot env))
       (evaluate '(begin expr ...) snapshot)))))

;;; Example: Query what happened at version 4
(display "\n\n=== Query: What happened at version 4? ===\n")
(with-history temporal-society
  (display (assoc 4 (env-history 
                     (make-env (env-bindings env) 
                              (env-validator env) 
                              (env-history env))))))