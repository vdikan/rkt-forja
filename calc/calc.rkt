#lang racket

(require racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calcfunctions ;;;;;;;;;;;;;;;;;;;;;;
;; just to have something to play with:
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


(define (sleep-work n)
  (sleep (random 2))
  (factorial n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define nspc (make-empty-namespace))


(define (calc-setup inputlst)
  (match inputlst
    [(list n)
     (let ((g (string->symbol
               (symbol->string (gensym)))))
       (namespace-set-variable-value!
        g (list (mcons 'inputs inputlst))
        #f nspc)
       g)]))


(define (calc-run g)
  (let* ((cdat
          (namespace-variable-value
           g #t #f nspc))
         (n (first                      ; need shortcut/better accessor here
             (mcdr                      ; actually, hashmaps
              (first
               (filter
                (lambda (mc)
                  (eq? (mcar mc)
                       'inputs)) cdat)
               ))))
         (f (sleep-work n)))
    (namespace-set-variable-value!
     g (append cdat (list (mcons 'info f)))
     #f nspc)
    g))


(define (calc-analyze g)
  (let* ((cdat
          (namespace-variable-value
           g #t #f nspc))
         (i (mcdr
             (first
              (filter
               (lambda (mc)
                 (eq? (mcar mc)
                      'info)) cdat))))
         (r (append cdat (list (mcons 'result i)))))
    ;; (namespace-set-variable-value!
    ;;  g (append cdat (mcons 'info f))
    ;;  #f nspc)
    (namespace-set-variable-value!
     g (append cdat (list (mcons 'result i)))
     #f nspc)
    g))


(define (run lst)
  (calc-analyze
   (calc-run
    (calc-setup lst))))


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
       (define item (channel-get work-channel))
       (case item
         [(#f)
          (channel-put result-channel
                       (format "Thread ~a done" thread-id))]
         [else
          (begin
            (define res  (run (list item)))
            (channel-put result-channel
                         (format "Thread ~a processed ~a; Result: ~a"
                                 thread-id
                                 item
                                 (mcdr
                                  (first
                                   (filter
                                    (lambda (mc)
                                      (eq? (mcar mc)
                                           'info))
                                    (namespace-variable-value
                                     res #t #f nspc)
                                    )))
                                 )))
          (loop)])))))

(define work-threads (map make-worker '(1 2 3)))

(for ([item '(12 13 17 14 16 #f 15 11 19 18 #f #f #f)])
  (channel-put work-channel item))

(for-each thread-wait work-threads)

(run '(5))



(namespace-variable-value 'g7545 #t #f nspc)

(namespace-undefine-variable! 'g7469 nspc)

(namespace-mapped-symbols nspc)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;VOPROS;;;;;;;;;;;;;;;;
;; (define-syntax (scalc-setup stx)
;;   (match (syntax->list stx)
;;     [(list _ prefstr initval)
;;      (let ((g (gensym (syntax->datum prefstr))))
;;        ;; (datum->syntax stx `(begin (define ,g ,initval)
;;        ;;                            ,g)))]))
;;        (datum->syntax stx
;;                       `(begin (namespace-set-variable-value! g ,initval)
;;                               ;; (define ,g ,initval)
;;                               ;; ,(symbol->string g)
;;                               )))]))


;; (datum->syntax stx `(define ,(gensym) ,initval))]))
