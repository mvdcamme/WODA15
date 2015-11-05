(module interaction racket
  
  (provide (all-defined-out))
  
  ;
  ; Signaling annotations
  ;
  
  (struct can-start-loop-encountered (label) #:transparent)
  
  ;
  ; Return types
  ;
  
  (struct trace-abnormal (signal) #:transparent)
  (struct trace-step (program-state) #:transparent)
  
  ;
  ; Interpreter return
  ;
  
  (struct interpreter-step (program-state
                            trace
                            annotation-signal) #:transparent)
  (struct interpreter-abnormal (signal) #:transparent)
  
  ;
  ; Interpreter signalling
  ;
  
  (struct interpreter-stopped ())
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;                                                                                                      ;
  ;                                         Error return signals                                         ;
  ;                                                                                                      ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;
  ; Signaling guard failure
  ;
  
  (struct guard-failed (restart-point) #:transparent)
  
  ;
  ; Signaling loops
  ;
  
  (struct trace-loops ())
  
  )
