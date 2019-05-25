#lang racket


(define (gensym-id [str "g"])
  (string->symbol (symbol->string (gensym str))))


(define (push-to-ns obj ns)
  (let ([sym-id (send obj gen-id)])
    (namespace-set-variable-value! sym-id obj #f ns)
    (set-field! label obj sym-id)
    sym-id))


(define (get-from-ns sym ns)
  (namespace-variable-value sym #t #f ns))


(define calculation-interface<%>
  (interface () gen-id run-calc analyze-calc))


(define calculation-class%
  (class* object%
    (calculation-interface<%>)

    (field [label 'unbound])
    (field [links '()])
    (field [status 'new])
    (field [info (make-hash)])

    (super-new)

    (define/public (gen-id) (gensym-id))

    (abstract run-calc)
    (abstract analyze-calc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scalc%
  (class calculation-class%
    (super-new)

    (init-field info-num)
    (inherit-field info)
    (hash-set! info 'input info-num)

    (inherit-field status)

    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))
    (define (sleep-work n)
      (sleep (random 2))
      (factorial n))

    (define/override-final (run-calc)
      (hash-set! info 'result
                 (sleep-work (hash-ref info 'input))))

    (define/override-final (analyze-calc)
      (let ([r (hash-ref info 'result)])
        (if (< r 100000)
            (begin
              (displayln "not enough")
              (list
               (new this% [info-num (+ 2 (hash-ref info 'input))])))
            #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nspc (make-empty-namespace))

(define result-channel (make-channel))

(define result-thread
  (thread (lambda ()
            (let loop ()
              (displayln (channel-get result-channel))
              (loop)))))

(define work-channel (make-channel))

(define (make-worker thread-id)
  (thread
   (lambda ()
     (let loop ()
       (begin
         (define sym (channel-get work-channel))
         (define item (get-from-ns sym nspc))
         (send item run-calc)
         (define res (send item analyze-calc))
         (channel-put result-channel
                      (format "Thread ~a processed ~a;"
                              thread-id (get-field label item)))
         (if (list? res)
             (let* ([newitem (car res)]
                    [newsym (push-to-ns newitem nspc)])
               (channel-put work-channel newsym))
             (channel-put result-channel
                          (format "Ending with ~a;"
                                  (get-field info item))))
         (loop))))))


;; (for ([item (list (make-object scalc% 8)
;;                   (make-object scalc% 5))])
;;   (channel-put work-channel (push-to-ns item nspc)))


(define work-threads (map make-worker '(1 2 3)))

(for-each thread-wait work-threads)

(namespace-mapped-symbols nspc)

;; (get-field info
;;            (namespace-variable-value 'g4471 #t #f nspc))

(get-field info (get-from-ns 'g2596 nspc))

(channel-put work-channel (push-to-ns (make-object scalc% 3) nspc))

(channel-put work-channel (push-to-ns (make-object scalc% 1) nspc))
