;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                               >>> test.rkt <<<                             ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         ;; RAILWAY
         "railway/main.test.rkt"
         "railway/crossing.test.rkt"
         "railway/detection-block.test.rkt"
         "railway/light.test.rkt"
         "railway/segment.test.rkt"
         "railway/switch.test.rkt"
         "railway/switch-3way.test.rkt"
         "railway/switch-cross.test.rkt"
         "railway/train.test.rkt"
         ;; INFRABEL
         ;; PROVIDER
         )

; running all test suites
(define all-tests
  (test-suite "Unit testing all modules"
              ;; RAILWAY
              (test-suite "Unit testing of all RAILWAY module operations"
                          main-test
                          crossing-test
                          detection-block-test
                          light-test
                          segment-test
                          switch-test
                          switch-3way-test
                          switch-cross-test
                          train-test
                          )
              ;; INFRABEL
              (test-suite "Unit testing of all INFRABEL module operations"
                          (test-case "test"
                                     (check-not-exn (λ () '())))
                          )
              ;; PROVIDER
              (test-suite "Unit testing of all PROVIDER module operations"
                          (test-case "test"
                                     (check-not-exn (λ () '())))
                          )
  ))

; GUI test:
(test/gui all-tests)

; Github actions: https://defn.io/2020/05/05/github-actions-for-racket-revised/