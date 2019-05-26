#lang racket

(require "../engine.rkt")

;;; Example of exploding calculations chain
;;
;; A channel must always have unoccupied listeners,
;; otherwise `channel-put` blocks it, and the whole operation.
;;
;; Could be a safeguard against uncontrollably-exploding
;; calculation chains.


(define scalc%
  (class calculation-class%
    (super-new)

    (field [info (make-hash (list (cons 'input 'none)))])
    (init-field info-num)
    (hash-set! info 'input info-num)

    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))
    (define (sleep-work n)
      (sleep (random 2))
      (factorial n))

    (define/override-final (run-calc)
      (hash-set! info 'result
                 (sleep-work (hash-ref info 'input)))
      (format "Operation factorial, result ~a"
              (hash-ref info 'result)))

    (define/override-final (analyze-calc)
      (let ([r (hash-ref info 'result)])
        (if (< r 1000000)
            (list
             (new this% [info-num (+ 2 (hash-ref info 'input))])
             (new this% [info-num (+ 3 (hash-ref info 'input))]))
            (hash-ref info 'result))))))


(define nspc (make-empty-namespace))

(define log-channel (make-channel))
(define log-thread
  (thread (lambda ()
            (let loop ()
              (displayln (channel-get log-channel))
              (loop)))))

(define results '())
(define result-channel (make-channel))
(define result-thread
  (thread (lambda ()
            (let loop ()
              (set! results
                    (append results
                            (list (channel-get result-channel))))
              (loop)))))

(define work-channel (make-channel))

(define work-threads
  (for/list ([i 20])
    (standard-worker i nspc work-channel result-channel log-channel)))

(begin
  (channel-put work-channel
               (push-to-ns (make-object scalc% 1) nspc))
  (channel-put work-channel
               (push-to-ns (make-object scalc% 2) nspc)))


;; (sleep 10)                              ; should be enough to finish
;; results

;; => '(39916800 3628800 479001600 ...)

;; (for-each thread-wait work-threads)
