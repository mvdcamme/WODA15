(module evaluator racket
  
  (require (rename-in "cesk-interpreter.scm"
                      (step cesk-step))
           "interaction.scm"
           "output.scm"
           "tracing.scm")
  
  (provide inject
           run)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;                                                                                                      ;
  ;                                           Evaluator state                                            ;
  ;                                                                                                      ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (struct evaluator-state (state
                           tracer-context
                           program-state
                           trace-executing) #:transparent)
  
  (define-syntax evaluator-state-copy
    (syntax-rules ()
      ((_ an-evaluator-state ...)
       (struct-copy evaluator-state an-evaluator-state ...))))
  
  ;
  ; States
  ;
  
  (define EXECUTING_STATE 'executing-trace)
  (define INTERPRETING_STATE 'regular-interpretation)
  (define TRACING_STATE 'tracing)
  
  (define (state-equals? evaluator-state state)
    (eq? (evaluator-state-state evaluator-state) state))
  
  (define (is-executing? evaluator-state)
    (state-equals? evaluator-state EXECUTING_STATE))
  
  (define (set-executing-trace-state evaluator-state)
    (set-state evaluator-state EXECUTING_STATE))
  
  (define (is-interpreting? evaluator-state)
    (state-equals? evaluator-state INTERPRETING_STATE))
  
  (define (set-interpreting-state evaluator-state)
    (set-state evaluator-state INTERPRETING_STATE))
  
  (define (is-tracing? evaluator-state)
    (state-equals? evaluator-state TRACING_STATE))
  
  (define (set-tracing-state evaluator-state)
    (set-state evaluator-state TRACING_STATE))
  
  (define (set-state evaluator-state new-state)
    (evaluator-state-copy evaluator-state (state new-state)))
  
  ;
  ; Constructors
  ;
  
  (define (make-executing-state tracer-context program-state trace-node)
    (evaluator-state EXECUTING_STATE tracer-context program-state trace-node))
  
  (define (make-interpreting-state tracer-context program-state)
    (evaluator-state INTERPRETING_STATE tracer-context program-state #f))
  
  (define (make-tracing-state tracer-context program-state)
    (evaluator-state TRACING_STATE tracer-context program-state #f))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;                                                                                                      ;
  ;                                          Running evaluator                                           ;
  ;                                                                                                      ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define prev-state 'interpreting)
  (define curr-state #f)
  
  (struct evaluation-done (value) #:transparent)
  
  (define (step-can-start-loop-encountered-regular label new-program-state trace tracer-context)
    (let ((trace-key (make-label-trace-key label)))
      (cond ((trace-exists? tracer-context trace-key)
             (outputln label 'd)
             (outputln "reg ----------- EXECUTING TRACE -----------" 'd)
             (make-executing-state tracer-context new-program-state (get-trace tracer-context trace-key)))
            (else
             (outputln label 'd)
             (outputln "reg ----------- STARTED TRACING -----------" 'd)
             (make-tracing-state (start-tracing-label tracer-context label) new-program-state)))))
  
  (define (step-can-start-loop-encountered-tracing label new-program-state trace tracer-context)
    (let ((trace-key (make-label-trace-key label)))
      (cond ((is-tracing-label? tracer-context label)
             (outputln label 'd)
             (outputln "tracing ----------- TRACING FINISHED; EXECUTING TRACE -----------" 'd)
             (let* ((temp-tracer-context (stop-tracing (append-trace tracer-context trace) optimise)))
               (make-executing-state temp-tracer-context new-program-state (get-trace temp-tracer-context trace-key))))
            (else
             (make-tracing-state (append-trace tracer-context trace) new-program-state)))))
  
  (define (evaluate evaluator-state)
    (define (continue-with-program-state-regular new-program-state)
      (evaluator-state-copy evaluator-state
                            (program-state new-program-state)))
    (define (continue-with-program-state-tracing new-program-state new-trace)
      (evaluator-state-copy evaluator-state
                            (tracer-context (append-trace (evaluator-state-tracer-context evaluator-state) new-trace))
                            (program-state new-program-state)))
    ;; Normal interpretation / trace recording
    (define (do-cesk-interpreter-step)
      (cesk-step (evaluator-state-program-state evaluator-state)))
    ;; Trace execution
    (define (do-trace-executing-step)
      (let* ((trace-node (evaluator-state-trace-executing
                            evaluator-state))
             (trace (trace-node-trace trace-node))
             (label (trace-key-label (trace-node-trace-key
                                        trace-node))))
        (if (null? trace)
           (let* ((old-trace-node (evaluator-state-trace-executing evaluator-state))
                  (old-trace-key (trace-node-trace-key old-trace-node))
                  (trace-key (make-label-trace-key (trace-key-label old-trace-key)))
                  (tracer-context (evaluator-state-tracer-context evaluator-state))
                  (new-trace-node (get-trace tracer-context trace-key)))
             (evaluator-state-copy evaluator-state
                                   (trace-executing new-trace-node)))
            (let* ((instruction (car trace))
                   (program-state
                    (evaluator-state-program-state evaluator-state)))
              (handle-response-executing (instruction program-state))))))
    ;; Trace execution
    (define (handle-response-executing response)
      (let* ((tracer-context (evaluator-state-tracer-context evaluator-state))
             (trace-executing (evaluator-state-trace-executing evaluator-state))
             (trace-key (trace-node-trace-key trace-executing))
             (trace (trace-node-trace trace-executing))
             (old-program-state (evaluator-state-program-state evaluator-state)))
        (define (do-guard-failure restart-point)
          (make-interpreting-state tracer-context (restart restart-point old-program-state)))
        (match response
          ((trace-step new-program-state)
           (evaluator-state-copy evaluator-state
                                 (program-state new-program-state)
                                 (trace-executing (trace-node-copy (evaluator-state-trace-executing evaluator-state)
                                                                   (trace (cdr trace))))))
          ((trace-abnormal (guard-failed restart-point))
           (do-guard-failure restart-point))
          ((trace-abnormal (trace-loops))
           (let* ((old-trace-node (evaluator-state-trace-executing evaluator-state))
                  (old-trace-key (trace-node-trace-key old-trace-node))
                  (trace-key (make-label-trace-key (trace-key-label old-trace-key))) 
                  (new-trace-node (get-trace tracer-context trace-key)))
             (evaluator-state-copy evaluator-state
                                   (trace-executing new-trace-node)))))))
    ;; Normal interpretation
    (define (handle-annotation-signal-regular new-program-state trace annotation-signal)
      (let ((tracer-context (evaluator-state-tracer-context evaluator-state)))
        (match annotation-signal
          ((can-start-loop-encountered label)
           (step-can-start-loop-encountered-regular label new-program-state trace tracer-context)))))
    ;; Trace recording
    (define (handle-annotation-signal-tracing new-program-state trace annotation-signal)
      (let ((tracer-context (evaluator-state-tracer-context evaluator-state)))
        (match annotation-signal
          ((can-start-loop-encountered label)
           (step-can-start-loop-encountered-tracing label new-program-state trace tracer-context)))))
    ;; Normal interpretation / trace recording
    (define (handle-response-abnormal response)
      (match response
        ((interpreter-abnormal (interpreter-stopped))
         (evaluation-done (get-program-state-value (evaluator-state-program-state evaluator-state))))
        ((interpreter-abnormal signal)
         (error "Abnormal return value from interpreter" signal))))
    ;; Normal interpretation
    (define (handle-response-regular response)
      (match response
        ((interpreter-step new-program-state _ #f)
         (continue-with-program-state-regular new-program-state))
        ((interpreter-step new-program-state trace annotation-signal)
         (handle-annotation-signal-regular new-program-state trace annotation-signal))
        (_
         (handle-response-abnormal response))))
    ;; Trace recording
    (define (handle-response-tracing response)
      (match response
        ((interpreter-step new-program-state trace #f)
         (continue-with-program-state-tracing new-program-state trace))
        ((interpreter-step new-program-state trace annotation-signal)
         (handle-annotation-signal-tracing new-program-state trace annotation-signal))
        (_
         (handle-response-abnormal response))))
    
    (define (step)
      (match evaluator-state
        ((? is-executing?) 
         (do-trace-executing-step))
        ((? is-interpreting?)
         (handle-response-regular (do-cesk-interpreter-step)))
        ((? is-tracing?)
         (handle-response-tracing (do-cesk-interpreter-step)))
        (_ (error "Unknown state" (evaluator-state-state evaluator-state)))))
    (if (evaluation-done? evaluator-state)
        (evaluation-done-value evaluator-state)
        (evaluate (step))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;                                                                                                      ;
  ;                                         Starting evaluator                                           ;
  ;                                                                                                      ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (inject e)
    (make-interpreting-state (new-tracer-context) (inject-program-state e)))
  
  (define (run evaluator-state)
    (evaluate evaluator-state))
  
  ;;; Reads an s-expression from the console and runs the evaluator on it.
  (define (start)
    (run (inject (read)))))