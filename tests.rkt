;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                              >>> tests.rkt <<<                             ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/gui

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

(require racket/cmdline rackunit rackunit/gui rackunit/text-ui
         ;; TRACK
         "track/interface.test.rkt"
         ;; RAILWAY
         "railway/crossing.test.rkt"
         "railway/light.test.rkt"
         "railway/segment.test.rkt"
         "railway/detection-block.test.rkt"
         "railway/switch.test.rkt"
         ;"railway/switch-3way.test.rkt"
         ;"railway/switch-cross.test.rkt"
         ;"railway/train.test.rkt"
         ;"railway/interface.test.rkt"
         ;; INFRABEL
         ;; PROVIDER
         )

; running all test suites
(define (all-tests gui? s-eff? slp?)
  (test-suite
   "Unit testing all modules"
   ;; TRACK
   (when (and slp? s-eff? gui?) (track-interface-test))
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

;; RackUnit setup GUI for manual running of program
(define gui%
  (class dialog%
    (init-field callback)
    (super-new (label "RackUnit setup"))
    (let ((config-pane
           (new vertical-pane% (parent this) (alignment '(left center))))
          (start-pane
           (new pane% (parent this) (alignment '(center center))))
          (gui? #t) (s-eff? #t) (slp? #f))
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

;; Manual running or automatic testing with "-g" flag
(let/cc exit
  (command-line
   #:once-any
   [("-g" "--github")
    "Run tests without GUI"
    (run-tests (all-tests #f #t #t) 'verbose)
    (exit)]
   #:args ()
   (send (make-object gui% callback) show #t)))