;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                        >>> tests.rkt <<<                                       ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

;
; TO DO IN SWITCH-3WAY & SWITCH-CROSS
; -> id's als lists implementeren om zo de 2 sub-wissels te representeren
; -> deze 2 sub-wissels implementeren
; -> set-position! aanpassen om deze 2 sub-wissels aan te spreken
;

;; What about de twee wissels in een lijst steken als connections??
;; -> ook voor testing accessible??

;; Ander idee: deze concepten bestaan niet op "railway niveau", maar enkel op
;; "infrabel niveau", tezamen met bv. lights en crossings die gekoppeld zijn aan
;; segments & detectionblocks + switches die dit alles verbinden.

(require racket/gui rackunit rackunit/gui rackunit/text-ui
         ;; TRACK
         "track/interface.rkt"
         "track/interface.test.rkt"
         ;; RAILWAY
         "railway/algorithms/search-track.test.rkt"
         "railway/components/crossing.test.rkt"
         "railway/components/light.test.rkt"
         "railway/components/segment.test.rkt"
         "railway/components/detection-block.test.rkt"
         "railway/components/switch.test.rkt"
         ;"railway/components/switch-3way.test.rkt"
         ;"railway/components/switch-cross.test.rkt"
         ;"railway/components/train.test.rkt"
         "railway/interface.rkt"
         ;"railway/interface.test.rkt"
         ;; INFRABEL
         ;; PROVIDER
         )

;;
;; Test suites for testing by hand
;;
(define (repl-test-track architecture)
  (define track (new track%))
  (cond ((eq? architecture 'simulator) (send track config! 'sim "hardware"))
        ((eq? architecture 'hardware) (send track config! 'hw "hardware"))
        (else (error "repl-test-track: type does not exist on track%" architecture)))
  (send track start)
  track)

(define (repl-test-railway architecture)
  (define track (repl-test-track architecture))
  (define railway (new railway% (connection track)))
  railway)

(define (repl-test-infrabel)
  (display "test infrabel"))

(define (repl-test-provider)
  (display "test provider"))

(define (repl-test-nmbs)
  (display "test nmbs"))

;;
;; Test suites for (automatic) unit testing
;;
(define (all-tests gui? s-eff? slp?)
  (test-suite
   "Unit testing all modules"
   ;; TRACK
   (when (and s-eff? gui?) (track-interface-test))
   ;; RAILWAY
   (test-suite
    "Unit testing of all RAILWAY module operations"
    (when slp? crossing-test)
    light-test
    segment-test
    detection-block-test
    switch-test
    ;switch-3way-test
    ;switch-cross-test
    ;train-test
    ;railway-interface-test
    search-track-test
    )
   ;; INFRABEL
   (test-suite
    "Unit testing of all INFRABEL module operations"
    (test-case "test"
               (check-not-exn (λ () '())))
    )
   ;; PROVIDER
   (test-suite
    "Unit testing of all PROVIDER module operations"
    (test-case "test"
               (check-not-exn (λ () '())))
    )))

;;
;; RackUnit setup GUI for manual running of program (rackunit gui)
;;
(define gui%
  (class dialog%
    (init-field callback)
    (super-new (label "RackUnit setup"))
    (let ((config-pane
           (new vertical-pane% (parent this) (alignment '(left center))))
          (start-pane
           (new pane% (parent this) (alignment '(center center))))
          (gui? #f) (s-eff? #t) (slp? #f))
      (new check-box%
           (label "Include tests with timeouts or sleeps?") (horiz-margin 10)
           (value slp?) (parent config-pane)
           (callback (λ (t e) (set! slp? (send t get-value)))))
      (new check-box%
           (label "Include tests with side-effects?") (horiz-margin 10)
           (value s-eff?) (parent config-pane)
           (callback (λ (t e) (set! s-eff? (send t get-value)))))
      (new check-box%
           (label "Test components with GUI dependancy?") (horiz-margin 10)
           (value gui?) (parent config-pane)
           (callback (λ (t e) (set! gui? (send t get-value)))))
      (new button%
           (label "start") (vert-margin 10) (parent start-pane)
           (callback
            (λ (t e) (send this show #f) (callback gui? s-eff? slp?)))))))
(define callback
  (λ (gui? s-eff? slp?) (test/gui (all-tests gui? s-eff? slp?))))

;;
;; Manual running or automatic testing with "-g" flag (github actions)
;;
(let/cc exit
  (command-line
   #:once-any
   [("-g" "--github")
    "Run tests without GUI"
    (run-tests (all-tests #f #t #t) 'verbose)
    (exit)]
   #:args ()
   (send (make-object gui% callback) show #t)))