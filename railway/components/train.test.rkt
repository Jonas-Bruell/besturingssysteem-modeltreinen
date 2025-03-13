;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> train.test.rkt <<<                          ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         "train.rkt")
(provide train-test)

;
; aliasses & abstrations
;
(define connection%
  (class object%
    (super-new)
    (define/public (set-state! new-state) #t)
    (define/public (add-loco id previous current) #t)
    ))

(define connection (make-object connection%))

(define train
  (make-object train% connection 'test 'test 'test))

;
; individual test suites
;
(define test-make-object
  (test-suite "testing make-object"
              (test-case "check 'train%' exists"
                         (check-not-exn (λ () train%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () train)))))

;
; running all test suites
;
(define train-test
  (test-suite "All train% operation tests"
              test-make-object
              ))