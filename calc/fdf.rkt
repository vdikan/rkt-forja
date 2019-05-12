#lang racket

(require racket/string)


;; I need to get the strings manipulation...
;; And start to pile up contracts on these:
(define (fdf-parameter-set fdf-inst param value)
  (let ([old-params (fdf-parameters fdf-inst)])
    (hash-set! old-params param value)))


(define (fdf-parameter-ref fdf-inst param)
  (hash-ref (fdf-parameters fdf-inst) param))


(define (fdf-parameters-display fdf-inst port mode)
  (let ([params-h (fdf-parameters fdf-inst)]
        [recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))])])
    (hash-for-each
     params-h
     (lambda (param value)
       (recur param port)
       (if (list? value)
           (begin (for-each (lambda (v)
                              (write-string "  " port)
                              (recur v port)) value)
                  (recur "\n" port))
           (begin (write-string "  " port)
                  (recur value port)
                  (recur "\n" port)))))))


(define (fdf-chemicalspecieslabel-display fdf-inst port mode)
  (let ([params-h (fdf-parameters fdf-inst)]
        [recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))])])
    (recur "%block  ChemicalSpeciesLabel\n" port)
    (recur "# need some logic here\n" port)
    (recur "%endblock  ChemicalSpeciesLabel\n" port)))


(define (fdf-display fdf-inst port mode)
  (fdf-parameters-display fdf-inst port mode)
  (fdf-chemicalspecieslabel-display fdf-inst port mode))


(struct fdf (parameters
             chemicalspecieslabel)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc fdf-display)])


(define (mk-fdf)
  (fdf (make-hash                       ; Parameters - hash table
        '([systemlabel . "forja"]
          [systemname  . "Template fdf struct instance"]))
       (list)))                         ; block ChemicalSpeciesLabel


(define (output-fdf-to-dir calc-dir fdf-inst)
  ;; gonna output fdf as `systemlabel`.fdf in dir
  (call-with-output-file
    (build-path calc-dir
                (string-append
                 (fdf-parameter-ref fdf-inst 'systemlabel) ".fdf"))
    (lambda (fdf-file)
      (display fdf-inst fdf-file))
    #:exists 'replace))


(provide (all-defined-out))
