#lang racket


(define dir-prefix "forja")


(define siesta-exec
  (find-executable-path "siesta"))


(define (launch-siesta system-label . inputs)
  (let* ([sym-name (symbol->string (gensym system-label))]
         [calc-dir (build-path "/tmp" dir-prefix sym-name)])

    (define (cp-input i)
      (let ([iname (file-name-from-path i)])
        (copy-file i (build-path calc-dir iname))))

    (make-directory* calc-dir)
    (for/list ([i inputs]) (cp-input i))

    (define-values (sp stdout stdin stderr)
      (parameterize ([current-directory calc-dir])
        (subprocess
         (open-output-file (string-append system-label ".out") #:exists 'truncate)
         (open-input-file  (string-append system-label ".fdf"))
         (open-output-file "error.log" #:exists 'truncate)
         siesta-exec)))

    (displayln sym-name)
    (values (normalize-path calc-dir) sp)))


(provide (all-defined-out))
