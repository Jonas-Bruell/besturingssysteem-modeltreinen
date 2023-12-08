#lang racket
(require rackunit
         rackunit/gui
         (prefix-in light: "../light.rkt"))

; aliasses & abstrations
(define light% light:light%)
(define connection%
  (class object%
    (super-new)
    (define/public (set-sign-code! id new-signal) #t)))
(define light$
  (new light% (connection (new connection%)) (id 'test) (signal 'test)))

; individual test suites
(define test-new
  (test-suite "testing new"
              (test-case "check 'light%' exists"
                         (check-not-exn (λ () light%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () light$)))
              ))

(define test-get-signal
  (test-suite "testing get-signal"
              (test-case "check 'get-signal' returns"
                         (check-true (symbol? (send light$ get-signal))))
              ))

(define test-set-signal!
  (test-suite "testing set-signal"
              (test-case "check 'set-signal!' doesn't error"
                         (check-not-exn (λ () (send light$ set-signal! 'n))))
              ))

; running all test suites
(define all-tests
  (test-suite "All light% operation tests"
              test-new
              test-get-signal
              test-set-signal!
              ))
(test/gui all-tests)