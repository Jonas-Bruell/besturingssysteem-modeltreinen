;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> light.test.rkt <<<                          ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         "light.rkt")
(provide light-test)
#|
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
|#
;
; aliasses & abstrations
;
(define connection%
  (class object%
    (super-new)
    (define/public (set-state! new-state) #t)))

(define connection (make-object connection%))

(define light
  (make-object light% connection 'test 'test))

;
; individual test suites
;
(define test-make-object
  (test-suite "testing make-object"
              (test-case "check 'light%' exists"
                         (check-not-exn (λ () light%)))
              (test-case "check constructor doesn't error"
                         (check-not-exn (λ () light)))))

;
; running all test suites
;
(define light-test
  (test-suite "All light% operation tests"
              test-make-object
              ))