#lang racket


(require "../engine.rkt")

(require xml xml/path)
(require plot)

(define dir-prefix "forja")

(define siesta-exec
  (find-executable-path "siesta"))


(define (prepare-calc-dir calc-dir inputs-dir)
  (let ([inputlist (parameterize ([current-directory inputs-dir])
                     (for/list ([p (in-directory)])
                       (normalize-path p)))])
    (define (cp-input i)
      (let ([iname (file-name-from-path i)])
        (copy-file i (build-path calc-dir iname))))
    (make-directory* calc-dir)
    (for/list ([i inputlist]) (cp-input i))))


(define (run-siesta-exec system-label calc-dir)
  (define-values (sp stdout stdin stderr)
    (parameterize ([current-directory calc-dir])
      (subprocess
       (open-output-file (string-append system-label ".out") #:exists 'truncate)
       (open-input-file  (string-append system-label ".fdf"))
       (open-output-file "error.log" #:exists 'truncate)
       siesta-exec)))
  (values sp stdout stdin stderr))


(define (get-total-energy system-label calc-dir)
  (define parsed
    (xml->xexpr
     ((eliminate-whitespace (list 'propertyList 'property))
      (document-element
       (read-xml
        (open-input-file
         (build-path calc-dir (string-append system-label ".xml"))))))))
  (define properties (se-path*/list '(propertyList ) parsed))

  (let etot ((plst properties))
    (if (empty? plst)
        #f
        (match (car plst)
          [`(property
             ((dictRef "siesta:Etot"))
             (scalar
              ((dataType "xsd:double") (units "siestaUnits:eV"))
              ,v)) (string->number (string-trim v))]
          [else (etot (cdr plst))]))))


(define siesta-calc%
  (class calculation-class%
    (super-new)
    (init-field system-label)
    (init-field inputs-dir)
    (inherit-field label)
    (field [calc-dir 'unsubmitted])

    (define/override (gen-id) (gensym-id system-label))

    (define/pubment (prepare-submission)
      (set! calc-dir (build-path "/tmp" dir-prefix
                                 (symbol->string label)))
      (prepare-calc-dir calc-dir inputs-dir)
      (inner (void) prepare-submission))

    (define/override (run-calc)
      (if (not (eq? label 'unbound))
          (begin
            (prepare-submission)
            (let-values ([(sp stdout stdin stderr)
                          (run-siesta-exec system-label calc-dir)])
              (let pwait ()
                (if (eq? (subprocess-status sp) 'running)
                    (pwait)
                    (format "Siesta calculation process ~a ended with status ~a"
                            label (subprocess-status sp))))))
          (error "Attempt to run unbound calculation - work folder path unknown!")))

    (define/override (analyze-calc)
      (let ([toten (get-total-energy system-label calc-dir)])
        (if toten toten (error "No Total Energy value in outputs"))))))


(define siesta-eos%
  (class siesta-calc%
    (super-new)
    (inherit-field system-label)
    (inherit-field calc-dir)
    (init-field latt-const)
    (define (prepare-submission)
      (call-with-output-file
        (build-path calc-dir (string-append system-label ".fdf"))
        #:exists 'append
        (λ (out-port)
          (displayln (format "LatticeConstant  ~a Ang" latt-const) out-port))))
    (augment-final prepare-submission)

    (define/override-final (analyze-calc)
      (let ([toten (get-total-energy system-label calc-dir)])
        (if toten
            (vector latt-const toten)
            (error "No Total Energy value in outputs"))))))


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


;; This creates a number of subprocessess greater
;; than one probably has cores on a local machine.

;; Still, works for me.
(for ([alat (range 5.35 5.50 0.02)])
  (channel-put
   work-channel
   (push-to-ns (make-object siesta-eos%
                            alat
                            "Si"
                            (build-path "res" "Si_eos"))
               nspc)))


;;; Wait for it and plot:
;; (plot (points results #:color 'red #:sym 'fullcircle)
;;       #:width 800
;;       #:height 600
;;       #:x-max 5.51
;;       #:x-min 5.33
;;       #:y-max -215.448
;;       #:y-min -215.472
;;       #:title "EOS plot for Si"
;;       #:x-label "Lattice Constant (Ang)"
;;       #:y-label "Total Energy calcualted by SIESTA (eV)")


;;; Alternatively, use the `data-frame` package for polynomial fitting curve.
;;; It is not included in the standard kit, and I did not specify dependencies yet.
;;; Run `raco pkg install data-frame` to install it,
;;; then proceed:
;; (require data-frame)
;;
;; (let finalplot ()
;;   (define df (make-data-frame))

;;   (df-add-series df (make-series "xs"
;;                                  #:data (for/vector ([vec results])
;;                                           (vector-ref vec 0))))
;;   (df-add-series df (make-series "ys"
;;                                  #:data (for/vector ([vec results])
;;                                           (vector-ref vec 1))))

;;   (define ff (df-least-squares-fit df "xs" "ys"
;;                                    #:mode 'polynomial
;;                                    #:polynomial-degree 5))
;;   (plot (list
;;          (points results #:color 'red #:sym 'fullcircle)
;;          (function ff #:color 'blue))
;;         #:width 800
;;         #:height 600
;;         #:x-max 5.51
;;         #:x-min 5.33
;;         #:y-max -215.448
;;         #:y-min -215.472
;;         #:title "EOS plot for Si"
;;         #:x-label "Lattice Constant (Ang)"
;;         #:y-label "Total Energy calcualted by SIESTA (eV)"))
