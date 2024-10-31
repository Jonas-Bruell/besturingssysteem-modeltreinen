;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        >>> switch-3way.test.rkt <<<                        ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         "switch-3way.rkt")
(provide switch-3way-test)

;
; aliasses & abstrations
;
(define connection%
  (class object%
    (super-new)
    (define/public (set-state! new-state) #t)))

(define connection (make-object connection%))

(define switch-3way
  (make-object switch-3way% connection 'test 'test))

;
; individual test suites
;
(define test-make-object
  (test-suite "testing make-object"
              (test-case "check 'switch-3way%' exists"
                         (check-not-exn (λ () switch-3way%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () switch-3way)))))

;
; running all test suites
;
(define switch-3way-test
  (test-suite "All switch-3way% operation tests"
              test-make-object
              ))