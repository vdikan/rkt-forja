#lang racket

(require rackunit "siesta.rkt")


(test-case
    "Siesta H2O example should rapidly run and succeed"
  (let* ([inputlist (parameterize ([current-directory
                                    (build-path 'up "examples" "res" "h2o")])
                      (for/list ([p (in-directory)])
                        (normalize-path p)))]
         [sp (apply launch-siesta "h2o" inputlist)])
    (displayln "Testing... 7 sec.")
    (sleep 7)
    (check-equal? (subprocess-status sp) 0))) ; exit code of siesta process
