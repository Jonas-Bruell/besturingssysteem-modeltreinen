;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        >>> switch-cross.test.rkt <<<                       ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         "switch-cross.rkt")
(provide switch-cross-test)

;
; aliasses & abstrations
;
(define connection%
  (class object%
    (super-new)
    (define/public (set-state! new-state) #t)))

(define connection (make-object connection%))

(define switch-cross
  (make-object switch-cross% connection 'test 'test))

;
; individual test suites
;
(define test-make-object
  (test-suite "testing make-object"
              (test-case "check 'switch-cross%' exists"
                         (check-not-exn (λ () switch-cross%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () switch-cross)))))

;
; running all test suites
;
(define switch-cross-test
  (test-suite "All switch-cross% operation tests"
              test-make-object
              ))