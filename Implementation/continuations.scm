(module continuations racket
  
  (provide (all-defined-out)
           (struct-out closure-guard-failedk))
  
  ;
  ; Continuations
  ;
  
  (struct andk (es) #:transparent)
  (struct applicationk (debug) #:transparent)
  (struct closure-guard-failedk (i) #:transparent)
  (struct condk (pes es) #:transparent)
  (struct definevk (x) #:transparent)
  (struct haltk () #:transparent)
  (struct ifk (e1 e2) #:transparent)
  (struct letk (x es) #:transparent)
  (struct let*k (x bds es) #:transparent)
  (struct letreck (x bds es) #:transparent)
  (struct ork (es) #:transparent)
  (struct randk (e es i) #:transparent)
  (struct ratork (i) #:transparent)
  (struct seqk (es) #:transparent)
  (struct setk (x) #:transparent)
  
  )