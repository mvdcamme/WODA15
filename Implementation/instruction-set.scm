(module instruction-set racket
  
  (require "cesk-state.scm"
           "closures.scm"
           "continuations.scm"
           "environment.scm"
           "interaction.scm"
           "output.scm")
  
  (provide (all-defined-out))
  
  (define (return-error error)
    (trace-abnormal error))
  
  (define (return-normal value)
    (trace-step value))
  
  ;;; A counter used to generate id's for newly allocated variables.
  ;;; This id is then used as the address in the environment.
  (define gencounter 2)
  (define (new-gencounter!)
    (let ((temp gencounter))
      (set! gencounter (+ gencounter 1))
      temp))
  
  (define (>>= program-state instructions)
    (cond ((null? instructions) (return-normal program-state))
          ;; Assumes that no abnormal actions can take place during
          ;; regular program interpretation.
          (else (>>= (trace-step-program-state ((car instructions) program-state))
                     (cdr instructions)))))
  
  ;;; Allocate a new variable in the environment and the store with the name x and
  ;;; as current value, the value in the register v.
  (define (alloc-var x)
    (lambda (program-state)
      (let ((ρ (program-state-ρ program-state))
            (σ (program-state-σ program-state))
            (a (new-gencounter!))
            (v (program-state-v program-state)))
        (return-normal (program-state-copy program-state
                                           (ρ (add-var-to-env ρ x a))
                                           (σ (cons (cons a v) σ)))))))
  
  
  ;;; Apply the native procedure currently stored in the register v to the first
  ;;; i values on the stack θ.
  (define (apply-native i)
    (lambda (program-state)
      (let* ((v (program-state-v program-state))
             (θ (program-state-θ program-state))
             (rands (take θ i)))
        (when (contains-env? rands)
          (error "Apply-native: rands contains an environment"))
        (return-normal (program-state-copy program-state
                                           (θ (drop θ i))
                                           (v (apply v rands)))))))
  
  ;;; Creates a closure with the arguments x, and the body es and places this new closure
  ;;; in the register v.
  (define (create-closure x es)
    (lambda (program-state)
      (let ((ρ (program-state-ρ program-state)))
        (return-normal (program-state-copy program-state
                                           (v (clo (lam x es) ρ)))))))
  
  ;;; Check the value of the register v. If it is #f, do nothing, else handle this guard failure.
  (define (guard-false e)
    (lambda (program-state)
      (if (program-state-v program-state)
          (begin (output "Guard-false failed" 'd) (output-newline) (return-error (guard-failed (ev e))))
          (begin (output "Guard passed") (output-newline) (return-normal program-state)))))
  
  ;;; Check whether the register v currently contains the same closure as it did when this guard
  ;;; was recorded. If it does, do nothing, else handle this guard failure.
  (define (guard-same-closure clo i)
    (lambda (program-state)
      (if (clo-equal? (program-state-v program-state) clo)
          (return-normal program-state)
          (begin (output "Closure guard failed, expected: " 'd) (output clo)
                 (output ", evaluated: " 'd) (output (program-state-v program-state)) (output-newline)
                 (return-error (guard-failed (ko (closure-guard-failedk i))))))))
  
  ;;; Check the value of the register v. If it is #t, do nothing, else handle this guard failure.
  (define (guard-true e)
    (lambda (program-state)
      (if (program-state-v program-state)
          (begin (output "Guard passed") (output-newline) (return-normal program-state))
          (begin (output "Guard-true failed" 'd) (output-newline) (return-error (guard-failed (ev e)))))))
  
  ;;; Place the value e in the register v.
  (define (literal-value e)
    (lambda (program-state)
      (return-normal (program-state-copy program-state
                                         (v e)))))
  
  ;;;  Looks up the current value of the variable x and stores in the register v.
  (define (lookup-var x)
    (lambda (program-state)
      (let ((ρ (program-state-ρ program-state))
            (σ (program-state-σ program-state)))
        (let ((binding (assoc x (env-lst ρ))))
          (match binding
            ((cons _ a) (return-normal (program-state-copy program-state
                                                           (v (cdr (assoc a σ))))))
            (_ (return-normal (program-state-copy program-state
                                                  (v (eval x))))))))))
  
  ;;; Pop the first continuation from the continuation stack τ-κ.
  (define (pop-continuation)
    (lambda (program-state)
      (let ((κ (program-state-κ program-state)))
        (return-normal (program-state-copy program-state
                                           (κ (cdr κ)))))))
  
  ;;; Prepares for an application of the closure currently stored in the register v
  ;;; by saving the current environment, popping the first i elements from the stack θ
  ;;; and switching to the lexical environment of the closure to be called.
  (define (prepare-function-call i)
    (define (save-vals)
      (lambda (program-state)
        (let ((v (program-state-v program-state))
              (θ (program-state-θ program-state)))
          (when (contains-env? v)
            (error "Save-vals: saved an environment instead of a val!"))
          (return-normal (program-state-copy program-state
                                             (θ (append (take v i) θ))
                                             (v (drop v i)))))))
    (lambda (program-state)
      (let ((clo (program-state-v program-state)))
        (>>= program-state
             (list (restore-vals i)
                   (save-env)
                   (save-vals)
                   (set-env (clo-ρ clo)))))))
  
  ;;; Push the continuation φ to the continuation stack τ-κ.
  (define (push-continuation φ)
    (lambda (program-state)
      (let ((κ (program-state-κ program-state)))
        (return-normal (program-state-copy program-state
                                           (κ (cons φ κ)))))))
  
  ;;; Place the value e in the register v.
  (define (quote-value e)
     (lambda (program-state)
       (return-normal (program-state-copy program-state
                                          (v e)))))
  
  ;;; Pop the environment from the stack θ and store it in ρ.
  (define (restore-env)
    (lambda (program-state)
      (let ((ρ (program-state-ρ program-state))
            (θ (program-state-θ program-state)))
        (return-normal (program-state-copy program-state
                                           (ρ (car θ))
                                           (θ (cdr θ)))))))
  
  ;;; Pop the first value from the stack θ and store it in the register v.
  (define (restore-val)
     (lambda (program-state)
       (let ((v (program-state-v program-state))
             (θ (program-state-θ program-state)))
         (when (env? (car θ))
           (error "Restore-val: restored an environment instead of a val!"))
         (return-normal (program-state-copy program-state
                                            (v (car θ))
                                            (θ (cdr θ)))))))
  
  ;;; Pop the first i values from the stack θ and store them in the form of a list in the register v.
  (define (restore-vals i)
     (lambda (program-state)
       (let ((v (program-state-v program-state))
             (θ (program-state-θ program-state)))
         (when (contains-env? (take θ i))
           (error "Restore-vals: restored an environment instead of a val!"))
         (return-normal (program-state-copy program-state
                                            (v (take θ i))
                                            (θ (drop θ i)))))))
  
  ;;; Save the environment currently stored in ρ to the stack θ.
  (define (save-env)
     (lambda (program-state)
       (let ((ρ (program-state-ρ program-state))
             (θ (program-state-θ program-state)))
         (return-normal (program-state-copy program-state
                                            (θ (cons ρ θ)))))))
  
  ;;; Save the value in the register v to the stack θ.
  (define (save-val)
     (lambda (program-state)
       (let ((v (program-state-v program-state))
             (θ (program-state-θ program-state)))
         (when (env? v)
           (error "Save-val: saved an environment instead of a val!"))
         (return-normal (program-state-copy program-state (θ (cons v θ)))))))
  
  ;;; Replace the environment currently stored in ρ by ρ*.
  (define (set-env ρ*)
     (lambda (program-state)
       (let ((ρ (program-state-ρ program-state)))
         (return-normal (program-state-copy program-state
                                            (ρ ρ*))))))
  
  ;;; Assign the value currently in the register v to the variable x.
  (define (set-var x)
     (lambda (program-state)
       (let* ((ρ (program-state-ρ program-state))
              (σ (program-state-σ program-state))
              (v (program-state-v program-state))
              (a (cdr (assoc x (env-lst ρ)))))
         (return-normal (program-state-copy program-state
                                            (σ (cons (cons a v) σ))))))))