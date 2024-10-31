;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                      >>> detection-block.test.rkt <<<                      ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         "detection-block.rkt")
(provide detection-block-test)
#|
; aliasses & abstrations
(define detection-block% detection-block:detection-block%)
(define connection%
  (class object%
    (super-new)
    (define/public (get-occupied-detection-blocks) '())))
(define detection-block$
  (new detection-block% (connection (new connection%)) (id 'test)))

; individual test suites
(define test-make-object
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
|#
;
; aliasses & abstrations
;
(define connection%
  (class object%
    (super-new)
    (define/public (set-state! new-state) #t)))

(define connection (make-object connection%))

(define detection-block
  (make-object detection-block% connection 'test 'test))

;
; individual test suites
;
(define test-make-object
  (test-suite "testing make-object"
              (test-case "check 'detection-block%' exists"
                         (check-not-exn (λ () detection-block%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () detection-block)))))

;
; running all test suites
;
(define detection-block-test
  (test-suite "All detection-block% operation tests"
              test-make-object
              ))