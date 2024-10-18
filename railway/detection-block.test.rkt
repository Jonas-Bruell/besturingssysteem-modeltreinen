#lang racket
(require rackunit
         rackunit/gui
         (prefix-in detection-block: "../detection-block.rkt"))

; aliasses & abstrations
(define detection-block% detection-block:detection-block%)
(define connection%
  (class object%
    (super-new)
    (define/public (get-occupied-detection-blocks) '())))
(define detection-block$
  (new detection-block% (connection (new connection%)) (id 'test)))

; individual test suites
(define test-new
  (test-suite "testing new"
              (test-case "check 'switch%' exists"
                         (check-not-exn (λ () detection-block%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () detection-block$)))
              ))

(define test-get-position
  (test-suite "testing get-position"
              (test-case "check 'get-state' returns"
                         (check-true (symbol? (send detection-block$ get-position))))
              ))

; running all test suites
(define all-tests
  (test-suite "All switch% operation tests"
              test-new
              test-get-position
              ))
(test/gui all-tests)