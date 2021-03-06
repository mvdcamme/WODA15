(module cesk-state racket

  (provide (all-defined-out))
  
  ;
  ; CK wrappers
  ;
  
  ;;; Represents the control of a program when evaluating an expression e.
  (struct ev (e) #:transparent)
  
  ;;; Represents the control of a program when following a continuation φ.
  (struct ko (φ) #:transparent)
  
  ;;; Represents the ck of a program.
  (struct ck (c κ))
  
  ;
  ; Program state
  ;
  
  ;;; The continuation stack is needed to switch back from trace execution
  ;;; to regular program interpretation.

  (define (get-program-state-value program-state)
    (program-state-v program-state))
  
  (struct program-state (c
                         ρ   ; env
                         σ   ; store
                         θ   ; non-kont stack
                         v   ; value returned
                         κ   ;continuation stack
                         ) #:transparent)
  
  (define-syntax program-state-copy
    (syntax-rules ()
      ((_ a-program-state ...)
       (struct-copy program-state a-program-state ...)))))