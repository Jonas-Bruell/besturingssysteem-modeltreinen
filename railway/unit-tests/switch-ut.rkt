#lang racket
(require rackunit
         rackunit/gui
         (prefix-in switch: "../switch.rkt"))

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

; running all test suites
(define all-tests
  (test-suite "All switch% operation tests"
              test-new
              test-get-position
              test-set-position!
              ))
(test/gui all-tests)