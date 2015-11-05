(module run-tests racket
  
  (provide run-test
           run-tests)
  
  (require racket/date)
  (require racket/include)
  
  (require (file "../file-outputting.scm"))
  (require (file "../evaluator.scm"))
  (require (file "test-paths.rkt"))
  
  (define TEST_INPUT_PATH "input_file.scm")
  
  ;
  ; Output file
  ;
  
  (define BASE_OUTPUT_FILE_NAME "./Testing output/output")
  (define BASE_OUTPUT_EXTENSION "txt")
  
  (define OUTPUT_FILE_NAME (make-full-output-file-name BASE_OUTPUT_FILE_NAME BASE_OUTPUT_EXTENSION))
  
  (define (append-to-output-file text)
    (append-to-file OUTPUT_FILE_NAME text))
  
  (define (output-aux text console-output-function)
    (console-output-function text)
    (append-to-output-file text))
  
  (define (output text)
    (output-aux text display))
  
  (define (output-newline)
    (output #\newline))
  
  (define (output-pretty text)
    (output-aux text pretty-print))
  
  (define (output-metric metric-name result)
    (output "Metric ") (output metric-name) (output " got result ") (output result)
    (output-newline))
  
  (define (output-result test-file evaluator result)
    (output "=> ") (output evaluator) (output " evaluated ") (output test-file) (output " and got result: ") (output result)
    (output-newline)
    (output-newline))
  
  (define (overwrite-input-file test-input-path new-test-path)
    (let* ((input-port (open-input-file new-test-path))
           (output-port (open-output-file test-input-path #:exists 'replace))
           (test-file-contents (read input-port)))
      (write test-file-contents output-port)
      (close-input-port input-port)
      (close-output-port output-port)))
  
  ;
  ; Testing
  ;
  
  (define (start-test test-path)
    (let* ((s-exp (file->value test-path))
           (tracing-interpreter-name "Tracing interpreter")
           (rec-slip-interpreter-normal-name "Recursive Slip interpreter (normal)")
           (rec-slip-interpreter-traced-name "Recursive Slip interpreter (traced merging)")
           (rec-slip-interpreter-traced-no-merging-name "Recursive Slip interpreter (traced no merging)"))
      (define (run-interpreter interpreter-start-function interpreter-name)
        (output-interpreter-start interpreter-name)
        (let ((value (interpreter-start-function)))
          (output-result-from-evaluator interpreter-name value)))
      (define (run-interpreter-timed interpreter-start-function interpreter-name)
        (let ((start-ms (current-milliseconds)))
          (run-interpreter interpreter-start-function interpreter-name)
          (let* ((end-ms (current-milliseconds))
                 (delta-ms (- end-ms start-ms)))
            ;; Safeguard for 32-bit systems
            (if (>= delta-ms 0)
                (begin (output "Time taken for ") (output interpreter-name) (output ": ") (output delta-ms) (output "ms") (output-newline))
                (error "Timing the run of the interpreter failed: calculated a negative time" delta-ms)))))
      (define (output-result-from-evaluator evaluator result)
        (output-result test-path evaluator result))
      (define (output-test-start)
        (output "---------- TEST STARTED: ") (output test-path) (output " ----------")
        (output-newline)
        (output-newline))
      (define (output-test-end)
        (output "---------- TEST FINISHED: ") (output test-path) (output " ----------")
        (output-newline))
      (define (output-interpreter-start interpreter-name)
        (output "interpreter started: ") (output interpreter-name)
        (newline))
      
      (define (run-tracing-interpreter)
        (run-interpreter-timed (lambda () (run (inject s-exp))) tracing-interpreter-name))
      
      (output-test-start)

      (run-tracing-interpreter)
      
      (output-test-end)))
  
  (define (run-test test-path)
    (overwrite-input-file TEST_INPUT_PATH test-path)
    (start-test test-path))
  
  (define (run-tests-aux run-test-function n tests)
    (for ((i (range n)))
      (for ((test tests))
        (run-test-function test)))
    "Finished!")
  
  (define (run-tests n tests)
    (run-tests-aux run-test n tests)))
