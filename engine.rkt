#lang racket


(define (gensym-id [str "g"])
  (string->symbol (symbol->string (gensym str))))


(define (push-to-ns obj ns)
  (let ([sym-id (send obj gen-id)])
    (namespace-set-variable-value! sym-id obj #f ns)
    (set-field! label obj sym-id)
    sym-id))


(define (update-links obj lst)
  (set-field! links obj
              (append (get-field links obj) lst)))


(define (get-from-ns sym ns)
  (namespace-variable-value sym #t #f ns))


(define calculation-interface<%>
  (interface () gen-id run-calc analyze-calc))


(define calculation-class%
  (class* object%
    (calculation-interface<%>)
    (field [label 'unbound])
    (field [links '()])
    (super-new)
    (define/public (gen-id) (gensym-id))
    (abstract run-calc)
    (abstract analyze-calc)))


(define (calcinstance? v)
  (is-a? v calculation-class%))


(define (standard-worker thread-id session-ns work-ch res-ch log-ch)
  (thread
   (lambda ()
     (let loop ()
       (define sym (channel-get work-ch))
       (define item (get-from-ns sym session-ns))

       (channel-put log-ch (format "--> Thread ~a running ~a;"
                                   thread-id (get-field label item)))
       (channel-put log-ch (send item run-calc)) ; Runner is mutating item
       ; presumably by dispatching work to external syscall/executable
       ; and/or other side-efects.
       ; It should return log message, TODO: contract that.

       (define res (send item analyze-calc)) ; Analyzer produces results of
       ; a calculation item, which are:
       ; - either arbitrary results, if an end of calculation chain was reached
       ; on item. Worker registers them in the results channel.
       ; - or list of new calculation instances, that a worker should push to
       ; work channel and link to the current item.
       (channel-put log-ch (format "<-- Thread ~a processed ~a;"
                                   thread-id (get-field label item)))

       (if ((listof calcinstance?) res)
           ; register new items in session namespace, work channel,
           ; and link their ids to parent item
           (update-links item (for/list ([newitem res])
                                (let ([newsym (push-to-ns newitem session-ns)])
                                  (channel-put work-ch newsym)
                                  newsym)))

           ; else - register res in results channel, log end of work chain.
           (begin
             (channel-put
              log-ch (format "Reached end of chain with ~a; registering results"
                             (get-field label item)))
             (channel-put res-ch res)))
       (loop)))))

(provide (all-defined-out))
