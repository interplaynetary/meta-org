(use-modules (metaorg play)
             (srfi srfi-1)
             (ice-9 hash-table))

(setvbuf (current-output-port) 'none)
(display "Example.scm starting...\n")


;;; Create a multiplayer organization
(define constitutional-convention
  (evaluate
   '(begin
      ;; Create the founders
      (defexpressor 'madison "crypto-key-1")
      (defexpressor 'hamilton "crypto-key-2")
      (defexpressor 'jay "crypto-key-3")
      
      ;; Constitution: Only Federalist Papers authors can enact laws
      (enact! (lambda (expressor name value new-bindings old-env)
                (let ((authors '(madison hamilton jay)))
                  (member (expressor-name expressor) authors))))
      
      ;; Madison proposes a bill of rights
      (as madison
          (modify! 'freedom-of-speech #t)
          (modify! 'right-to-bear-arms #t)
          (modify! 'protection-from-search #t))
      
      ;; Hamilton proposes an economic system
      (as hamilton
          (modify! 'national-bank #t)
          (modify! 'assume-state-debts #t)
          (modify! 'tariff 0.05))
      
      ;; Jay handles foreign policy
      (as jay
          (modify! 'treaty-power 'executive)
          (modify! 'neutrality-proclamation #t))
      
      (display "\n=== Constitutional Convention Results ===\n")
      (show-history))
   (make-initial-env `((+ . ,+)
                       (- . ,-)
                       (member . ,member)
                       (expressor-name . ,expressor-name)))))

;;; Now see the rich multiplayer history
(with-history constitutional-convention
  (show-history))

;;; A reputation-based validator
(define reputation-org
  (evaluate
   '(begin
      ;; Create participants
      (defexpressor 'alice "key-a")
      (defexpressor 'bob "key-b")
      (defexpressor 'charlie "key-c")
      (defexpressor 'mallory "key-m")
      
      ;; Initialize reputation scores
      (modify! 'reputation (make-hash-table))
      (as alice (modify! 'reputation (hash-set! (reputation) alice 100)))
      (as bob (modify! 'reputation (hash-set! (reputation) bob 80)))
      (as charlie (modify! 'reputation (hash-set! (reputation) charlie 60)))
      (as mallory (modify! 'reputation (hash-set! (reputation) mallory 10)))
      
      ;; Constitution: Changes need quorum of high-reputation expressors
      (enact! (lambda (expressor name value new-bindings old-env)
                (let* ((reps (env-lookup old-env 'reputation))
                       (my-rep (hash-ref reps expressor 0))
                       (total-rep (fold (lambda (k v sum) (+ sum v)) 
                                       0 reps))
                       (quorum-needed (* 0.51 total-rep)))
                  ;; Need >51% of total reputation to approve
                  (>= my-rep quorum-needed))))
      
      ;; Alice can make changes (100 > 51% of 250 = 127.5? No, she needs help)
      ;; This will FAIL - shows reputation requirements
      (as alice (modify! 'policy-1 "Alice's policy"))
      
      ;; But Alice + Bob together can (100 + 80 = 180 > 127.5)
      (as alice (modify! 'policy-1 "Joint Alice-Bob policy"))
      (as bob (modify! 'policy-1 "Final version"))  ;; Overwrites, needs separate vote
      
      (display "\n=== Reputation-Based Governance ===\n")
      (show-history))
   (make-initial-env `((+ . ,+)
                       (- . ,-)
                       (make-hash-table . ,make-hash-table)
                       (hash-set! . ,hash-set!)
                       (hash-ref . ,hash-ref)
                       (fold . ,fold)))))

;;; Define base-env for liquid democracy
(define base-env
  (make-initial-env
   `((make-hash-table . ,make-hash-table)
     (hash-set! . ,hash-set!)
     (hash-ref . ,hash-ref)
     (fold . ,fold)
     (+ . ,+)
     (eq? . ,eq?))))

;;; Delegative Democracy (Liquid Democracy)
(define liquid-democracy
  (evaluate
   '(begin
      (defexpressor 'voter1 "key1")
      (defexpressor 'voter2 "key2")
      (defexpressor 'voter3 "key3")
      (defexpressor 'delegate "key-d")
      
      ;; Delegation mechanism
      (modify! 'delegations (make-hash-table))
      
      (as voter1 (modify! 'delegations 
                         (hash-set! (delegations) voter1 'delegate)))
      (as voter2 (modify! 'delegations 
                         (hash-set! (delegations) voter2 'delegate)))
      (as voter3 (modify! 'delegations 
                         (hash-set! (delegations) voter3 voter3)))  ; Self
      
      ;; Validator computes voting power through delegation chain
      (enact! (lambda (expressor name value new-bindings old-env)
                (let* ((delegs (env-lookup old-env 'delegations))
                       (voting-power
                        (fold (lambda (k v power)
                               (if (eq? (hash-ref delegs k) expressor)
                                   (+ power 1)
                                   power))
                             0 delegs)))
                  ;; Need at least 2 votes
                  (>= voting-power 2))))
      
      ;; Delegate can act (has voter1 + voter2 = 2 votes)
      (as delegate (modify! 'policy "Delegate's policy"))  ;; WORKS
      
      ;; Voter3 acting alone (only 1 vote) - FAILS - This is expected to crash!
      (as voter3 (modify! 'policy "Voter3's policy")))
   base-env))