(module unit-tests racket
  
  (require (file "test-paths.rkt"))
  (require (file "testing.rkt"))
  
  (run-tests 1 (list fac-test-path
                     fib-test-path
                     closed-test-path
                     rsa-test-path
                     nqueens-2-test-path
                     rotate-test-path
                     simplified-trace-explosion-test-path
                     scheme2c-test-path)))