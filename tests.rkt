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

(require rackunit
         rackunit/gui
         rackunit/text-ui
         racket/cmdline
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

(provide all-tests)

; running all test suites
(define all-tests
  (test-suite "Unit testing all modules"
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
                          )
              ))
(let/cc exit
(command-line
 #:once-any
 [("-g" "--github") "Run tests without GUI"
                    (run-tests all-tests 'verbose)
                    (exit)]
 #:args ()
 (test/gui all-tests)))