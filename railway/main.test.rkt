;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> main.test.rkt <<<                           ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         "main.rkt")
(provide main-test)

;
; aliasses & abstrations
;
(define connection%
  (class object%
    (super-new)
    (define/public (set-state! new-state) #t)))

(define connection (make-object connection%))

(define basic-railway%
  (class railway%
    (super-new)
    ; track
    (define/override (track) '())
    ; segments
    (define/override (segment-list segment%)
      (thunk (λ () '())))
    ; detection-blocks
    (define/override (detection-block-list detection-block%)
      (thunk (λ () '())))
    ; switches
    (define/override (switch-list switch%)
      (thunk (λ () '())))
    ; crossings
    (define/override (crossing-list crossing%)
      (thunk (λ () '())))
    ; lights
    (define/override (light-list light%)
      (thunk (λ () '())))
    ))

(define basic-railway
  (make-object basic-railway%))

;
; individual test suites
;
(define test-make-object
  (test-suite "testing make-object"
              (test-case "check 'main%' exists"
                         (check-not-exn (λ () basic-railway%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () basic-railway)))))

;
; running all test suites
;
(define main-test
  (test-suite "All main% operation tests"
              ;test-make-object
              ))