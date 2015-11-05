(module tracing racket
  
  (provide ;; Trace-key
   (struct-out trace-key)
   make-label-trace-key
   
   ;; Trace-node
   (struct-out trace-node)
   add-execution!
   trace-node-copy
   
   ;; Tracer-context
   (struct-out tracer-context)
   new-tracer-context
   tracer-context-copy
   
   ;; Start tracing
   start-tracing-label
   
   ;;Stop tracing
   stop-tracing
   
   ;; Finding traces
   get-trace
   trace-exists?
   
   ;; Recording trace
   append-trace
   clear-trace
   is-tracing-label?)
  
  (require "dictionary.scm"
           "interaction.scm"
           "stack.scm"
           "trace-outputting.scm")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;                                                                                                      ;
  ;                                        Tracing bookkeeping                                           ;
  ;                                                                                                      ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;
  ; Trace keys
  ;
  
  (struct trace-key (label) #:transparent)
  (struct label-trace-key trace-key ())
  
  (define (make-label-trace-key label)
    (label-trace-key label))
  
  (define (trace-keys-equal? trace-key-1 trace-key-2)
    (and (label-trace-key? trace-key-1) (label-trace-key? trace-key-2)
         (equal? (trace-key-label trace-key-1) (trace-key-label trace-key-2))))
  
  ;
  ; Trace nodes
  ;
  
  (struct trace-node (trace-key
                      trace
                      (executions #:mutable)) )
  
  (define (make-trace-node trace-key trace)
    (trace-node trace-key trace '()))
  
  ;;; Used for benchmarking purposes
  (define (add-execution! trace-node)
    (let ((old-executions (trace-node-executions trace-node))
          (time (current-seconds)))
      (set-trace-node-executions! trace-node (cons time old-executions))))
  
  (define-syntax trace-node-copy
    (syntax-rules ()
      ((_ a-trace-node ...)
       (struct-copy trace-node a-trace-node ...))))
  
  ;
  ; Tracer context
  ;
  
  (struct tracer-context (trace-key
                          trace-nodes
                          τ) #:transparent)
  
  (define (new-tracer-context)
    (tracer-context #f
                    '()
                    '()))
  
  (define-syntax tracer-context-copy
    (syntax-rules ()
      ((_ a-tracer-context ...)
       (struct-copy tracer-context a-tracer-context ...))))
  
  ;
  ; Start tracing
  ;
  
  (define (start-tracing-label tracer-context label)
    (let ((temp-tracer-context
           (tracer-context-copy tracer-context
                                (trace-key (make-label-trace-key label)))))
      (clear-trace temp-tracer-context)))
  
  ;
  ; Stop tracing
  ;
  
  (define (stop-tracing-bookkeeping tracer-context)
    (let* ((temp-tracer-context
            (tracer-context-copy  tracer-context
                                  (trace-key #f))))
      (clear-trace temp-tracer-context)))
  
  (define (stop-tracing-normal tracer-context)
    (stop-tracing-bookkeeping tracer-context))
  
  (define (stop-tracing tracer-context optimise)
    (let* ((trace (tracer-context-τ tracer-context))
           (optimised-trace (optimise trace))
           (trace-key (tracer-context-trace-key tracer-context))
           (new-tracer-context (add-trace tracer-context trace-key optimised-trace)))
      (stop-tracing-normal new-tracer-context)))
  
  ;
  ; Finding traces
  ;
  
  (define (return-if-existing trace . errormessage)
    (if trace
        trace
        (apply error errormessage)))
  
  (define (search-trace tracer-context trace-key)
    (define (loop trace-nodes)
      (cond ((null? trace-nodes) #f)
            ((trace-keys-equal? (trace-node-trace-key (car trace-nodes)) trace-key) (car trace-nodes))
            (else (loop (cdr trace-nodes)))))
    (loop (tracer-context-trace-nodes tracer-context)))
  
  (define (get-trace tracer-context trace-key)
    (let ((trace-node-found (search-trace tracer-context trace-key)))
      (return-if-existing trace-node-found "Trace not found!" trace-key)))
  
  (define (not-false? value)
    (if value
        #t
        #f))
  
  (define (trace-exists? tracer-context trace-key)
    (not-false? (search-trace tracer-context trace-key)))
  
  ;
  ; Recording trace
  ;
  
  (define (append-trace tracer-context ms)
    (tracer-context-copy tracer-context
                         (τ (append (tracer-context-τ tracer-context) ms))))
  
  (define (clear-trace tracer-context)
    (tracer-context-copy tracer-context
                         (τ '())))
  
  (define (is-tracing-label? tracer-context label)
    (and (tracer-context-trace-key tracer-context)
         (equal? (trace-key-label (tracer-context-trace-key tracer-context)) label)))
  
  ;
  ; Adding traces
  ;
  
  (define (write-trace trace-key trace)
    (let ((label (trace-key-label trace-key)))
      (cond ((label-trace-key? trace-key) (write-label-trace label (gensym) trace))
            (else (error "Trace-key not recognized:" trace-key)))))
  
  (define (add-trace tracer-context trace-key transformed-trace)
    (let* ((label (trace-key-label trace-key))
           (trace-node (make-trace-node trace-key transformed-trace))
           (trace-nodes-list (tracer-context-trace-nodes tracer-context)))
      (write-trace trace-key transformed-trace)
      (tracer-context-copy tracer-context
                           (trace-nodes (cons trace-node trace-nodes-list))))))
