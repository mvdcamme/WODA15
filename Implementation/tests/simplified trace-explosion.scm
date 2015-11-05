(begin (define (loop n)
         (if (= (random 2) 0)
             'f0
             'f1)
             ;(begin (display "f 0") (newline))
             ;(begin (display "f 1") (newline)))
         (if (= (random 2) 0)
             'g0
             'g1)
             ;(begin (display "g 0") (newline))
             ;(begin (display "g 1") (newline)))
         (if (= (random 2) 0)
             'h0
             'h1)
             ;(begin (display "h 0") (newline))
             ;(begin (display "h 1") (newline)))
         (if (> n 0)
             (begin (loop (- n 1)))))
       (loop 500))