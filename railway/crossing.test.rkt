#lang racket
(require rackunit
         rackunit/gui
         (prefix-in crossing: "../crossing.rkt"))

; aliasses & abstrations
(define crossing% crossing:crossing%)
(define connection%
  (class object%
    (super-new)
    (define/public (open-crossing!) #t)
    (define/public (close-crossing!) #t)))
(define crossing$
  (new crossing% (connection (new connection%)) (id 'test) (state 'test)))

; individual test suites
(define test-new
  (test-suite "testing new"
              (test-case "check 'crossing%' exists"
                         (check-not-exn (λ () crossing%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () crossing$)))
              ))

(define test-get-state
  (test-suite "testing get-state"
              (test-case "check 'get-state' returns"
                         (check-true (symbol? (send crossing$ get-state))))
              ))

(define test-set-state!
  (test-suite "testing set-state"
              (test-case "check 'set-state!' doesn't error"
                         (check-not-exn (λ () (send crossing$ set-state! 'n))))
              ))

; running all test suites
(define all-tests
  (test-suite "All crossing% operation tests"
              test-new
              test-get-state
              test-set-state!
              ))
(test/gui all-tests)