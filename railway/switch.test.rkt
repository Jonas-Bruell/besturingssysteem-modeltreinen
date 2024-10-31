;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                           >>> switch.test.rkt <<<                          ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         "switch.rkt")
(provide switch-test)
#|
; aliasses & abstrations
(define switch% switch:switch%)
(define connection%
  (class object%
    (super-new)
    (define/public (set-switch-position! id new-state) #t)))
(define switch$
  (new switch% (connection (new connection%)) (id 'test) (position 'test)))

; individual test suites
(define test-new
  (test-suite "testing new"
              (test-case "check 'switch%' exists"
                         (check-not-exn (λ () switch%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () switch$)))
              ))

(define test-get-position
  (test-suite "testing get-position"
              (test-case "check 'get-state' returns"
                         (check-true (symbol? (send switch$ get-position))))
              ))

(define test-set-position!
  (test-suite "testing set-state"
              (test-case "check 'set-state!' doesn't error"
                         (check-not-exn (λ () (send switch$ set-position! 1))))
              ))
|#
;
; aliasses & abstrations
;
(define connection%
  (class object%
    (super-new)
    (define/public (set-state! new-state) #t)))

(define connection (make-object connection%))

(define switch
  (make-object switch% connection 'test 'test))

;
; individual test suites
;
(define test-make-object
  (test-suite "testing make-object"
              (test-case "check 'switch%' exists"
                         (check-not-exn (λ () switch%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () switch)))))

;
; running all test suites
;
(define switch-test
  (test-suite "All switch% operation tests"
              test-make-object
              ))