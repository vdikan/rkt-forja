#lang racket


(require "../engine.rkt")

(require remote-shell/ssh)
(require racket/path)
(require xml xml/path)
(require plot)


(define dir-prefix "/tmp/forja")
(define retrieve-list '(".xml" ".RHO"))

;;; Remote computer credentials. Do not forget to provide!
;;; Do not push them upstream either...
;;; (yes, I know, they should be passed as environment variables or command-line args)
(define remote-computer (remote #:host ""
                                #:user ""))
(define remote-dir-prefix  "")
(define remote-siesta-path "")


(define (read-stm-data fpath)
  (filter (λ (v) (= (vector-length v) 3))
          (for/list ([line (file->lines fpath #:mode 'text)])
            (list->vector
             (map string->number (string-split line))))))


(define plstm-exec (find-executable-path "plstm"))
(define (run-plstm-exec calc-dir)
  (define-values (sp stdout stdin stderr)
    (parameterize ([current-directory calc-dir])
      (subprocess
       (open-output-file "stm.out" #:exists 'truncate)
       (open-input-file  "stm.in")
       (open-output-file "stm.error.log" #:exists 'truncate)
       plstm-exec)))
  (values sp stdout stdin stderr))


(define (prepare-plstm-dir system-label calc-dir inputs-dir z-level)
  (make-directory* calc-dir)
  (let ([iname (string-append system-label ".RHO")])
    (copy-file (build-path inputs-dir iname)
               (build-path calc-dir iname)))
  (call-with-output-file
    (build-path calc-dir (string-append "stm.in"))
    #:exists 'truncate
    (λ (out-port)
      (displayln system-label out-port)
      (displayln "rho" out-port)
      (displayln "constant-height" out-port)
      (displayln (number->string z-level) out-port)
      (displayln "unformatted" out-port))))


(define plstm-calc%
  (class calculation-class%
    (super-new)
    (init-field system-label)
    (init-field inputs-dir)
    (init-field z-level)
    (inherit-field label)
    (field [calc-dir 'unsubmitted])

    (define/override (gen-id) (gensym-id (string-append "plstm-"
                                                        system-label)))

    (define (prepare-submission)
      (set! calc-dir (build-path dir-prefix (symbol->string label)))
      (prepare-plstm-dir system-label calc-dir inputs-dir z-level))

    (define/override-final (run-calc)
      (if (not (eq? label 'unbound))
          (begin
            (prepare-submission)
            (let-values ([(sp stdout stdin stderr)
                          (run-plstm-exec calc-dir)])
              (let pwait ()
                (if (eq? (subprocess-status sp) 'running)
                    (pwait)
                    (format "Pl-STM calculation process ~a ended with status ~a"
                            label (subprocess-status sp))))))
          (error "Attempt to run unbound calculation - work folder path unknown!")))

    (define/override-final (analyze-calc)
      (list
       system-label z-level
       (read-stm-data (build-path calc-dir
                                  (string-append system-label ".CH.STM")))))))


(define (prepare-calc-dir calc-dir inputs-dir)
  (let ([inputlist (parameterize ([current-directory inputs-dir])
                     (for/list ([p (in-directory)])
                       (normalize-path p)))])
    (define (cp-input i)
      (let ([iname (file-name-from-path i)])
        (copy-file i (build-path calc-dir iname))))
    (make-directory* calc-dir)
    (for/list ([i inputlist]) (cp-input i))))


(define (prepare-remote-calc-dir calc-dir)
  (let ([inputlist (parameterize ([current-directory calc-dir])
                     (for/list ([p (in-directory)])
                       (normalize-path p)))])

    (define dname (file-name-from-path calc-dir))

    (define (cp-input i)
      (let ([iname (file-name-from-path i)])
        (scp remote-computer
             i (at-remote remote-computer
                          (build-path remote-dir-prefix dname iname)))))

    (ssh remote-computer (format "mkdir -p ~a"
                                 (build-path remote-dir-prefix dname)))

    (for/list ([i inputlist]) (cp-input i))))


(define (run-at-remote system-label calc-dir)
  (define mod-avail-str "source ~/bin/spack_siesta_modules.sh")
  (define dname (build-path remote-dir-prefix
                            (file-name-from-path calc-dir)))
  (define mpi-num-procs 2)

  (ssh remote-computer (format "~a && cd ~a && mpirun -np ~a ~a < ~a.fdf > ~a.out"
                               mod-avail-str dname mpi-num-procs remote-siesta-path
                               system-label system-label)))


(define (retrieve-from-remote system-label calc-dir)
  (define dname (build-path remote-dir-prefix
                            (file-name-from-path calc-dir)))
  (andmap (λ(x)x)
          (for/list ([ext retrieve-list])
            (scp remote-computer
                 (at-remote remote-computer
                            (build-path dname (string-append system-label ext)))
                 (build-path calc-dir (string-append system-label ext))
                 #:mode 'result))))


(define siesta-calc%
  (class calculation-class%
    (super-new)
    (init-field system-label)
    (init-field inputs-dir)
    (inherit-field label)
    (field [calc-dir 'unsubmitted])

    (define/override (gen-id) (gensym-id system-label))

    (define/override (run-calc)
      (if (not (eq? label 'unbound))
          (begin
            (set! calc-dir (build-path dir-prefix (symbol->string label)))
            (prepare-calc-dir calc-dir inputs-dir)
            (prepare-remote-calc-dir calc-dir)
            (run-at-remote system-label calc-dir))
          (error "Attempt to run unbound calculation - work folder path unknown!")))

    (define/override (analyze-calc)
      (if (retrieve-from-remote system-label calc-dir)
          (for/list ([z-level (range 2.5 3.5 0.2)])
            (make-object plstm-calc% system-label calc-dir z-level))
          (error "Could not retrieve all listed files from remote!")))))


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
  (for/list ([i 10])
    (standard-worker i nspc work-channel result-channel log-channel)))


(channel-put
 work-channel
 (push-to-ns (make-object siesta-calc%
                          "benzene"
                          (build-path "res" "benzene_stm"))
             nspc))


(make-directory* "/tmp/plots")


;; (let ((lst (car results)))
;;   (let* ([syslabel (first lst)]
;;          [zlevel (exact->inexact (/ (inexact->exact
;;                                      (round (* 10 (second lst))))
;;                                     10))]
;;          [data (third lst)])
;;     (plot3d
;;      (points3d data
;;                #:sym 'dot #:color 'blue #:alpha 0.7)
;;      #:z-max 0.05
;;      #:title (format "STM image of ~a at Z-level ~a Bohr"
;;                      syslabel zlevel))))


;; (for ([lst results])
;;   (let* ([syslabel (first lst)]
;;          [zlevel (exact->inexact (/ (inexact->exact
;;                                      (round (* 10 (second lst))))
;;                                     10))]
;;          [data (third lst)])
;;     (plot3d-file
;;      (points3d data
;;                #:sym 'dot #:color 'blue #:alpha 0.7)
;;      (format "/tmp/plots/stm_~a_~a.svg" syslabel zlevel) 'svg
;;      #:z-max 0.05
;;      #:title (format "STM image of ~a at Z-level ~a Bohr"
;;                      syslabel zlevel))))
