;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                              >>> tests.rkt <<<                             ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

;
; TO DO IN SWITCH-3WAY & SWITCH-CROSS
; -> id's als lists implementeren om zo de 2 sub-wissels te representeren
; -> deze 2 sub-wissels implementeren
; -> set-position! aanpassen om deze 2 sub-wissels aan te spreken
;

;; What about de twee wissels in een lijst steken als connections??
;; -> ook voor testing accessible??

(require racket/gui racket/cmdline rackunit rackunit/gui rackunit/text-ui
         ;; RAILWAY
         "railway/crossing.test.rkt"
         "railway/light.test.rkt"
         "railway/segment.test.rkt"
         "railway/detection-block.test.rkt"
         "railway/switch.test.rkt"
         ;"railway/switch-3way.test.rkt"
         ;"railway/switch-cross.test.rkt"
         ;"railway/train.test.rkt"
         ;"railway/main.test.rkt"
         ;; INFRABEL
         "infrabel/logic/track-interface.test.rkt"
         ;; PROVIDER
         )

; running all test suites
(define (all-tests sim? gui?)
  (test-suite
   "Unit testing all modules"
   ;; TRACK
   (test-suite "Unit testing of all TRACK module operations"
               (test-case "test"
                          (check-not-exn (λ () '())))
               )
   ;; RAILWAY
   (test-suite "Unit testing of all RAILWAY module operations"
               crossing-test
               light-test
               segment-test
               detection-block-test
               switch-test
               ;switch-3way-test
               ;switch-cross-test
               ;train-test
               ;main-test
               )
   ;; INFRABEL
   (test-suite "Unit testing of all INFRABEL module operations"
               ;(track-interface-test 'sim)
               (test-case "test"
                          (check-not-exn (λ () '())))
               )
   ;; PROVIDER
   (test-suite "Unit testing of all PROVIDER module operations"
               (test-case "test"
                          (check-not-exn (λ () '())))
               )))

;; RackUnit setup GUI for manual running of program
(define gui%
  (class dialog%
    (init-field startup-callback)
    (super-new (label "RackUnit setup"))
    (let ((config-pane
           (new vertical-pane% (parent this) (alignment '(left bottom))))
          (start-pane
           (new pane% (parent this) (alignment '(center center))))
          (sim? #t))
      (new check-box%
           (label "Test GUI components") (vert-margin 10) (horiz-margin 10)
           (parent config-pane)
           (value #t)
           (callback (λ (t e) (set! sim? (send t get-value)))))
      (new check-box%
           (label "Test SIM components xxxxxxxxxxxx") (horiz-margin 10)
           (parent config-pane)
           (value #t)
           (callback (λ (t e) (set! sim? (send t get-value)))))
      (new button%
           (label "start") (vert-margin 10)
           (parent start-pane)
           (callback (λ (t e) (send this show #f) (startup-callback sim?)))))))

(define startup-callback
  (λ (sim?)
    (test/gui (all-tests sim? #t))))

;; Manual running or automatic testing with "-g" flag
(let/cc exit
  (command-line
   #:once-any
   [("-g" "--github")
    "Run tests without GUI"
    (run-tests (all-tests #t #f) 'verbose)
    (exit)]
   #:args ()
   (send (make-object gui% startup-callback) show #t)))