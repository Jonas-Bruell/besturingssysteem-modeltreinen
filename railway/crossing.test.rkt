;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                          >>> crossing.test.rkt <<<                         ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         "crossing.rkt")
(provide crossing-test)

;
; aliasses
;
(define open 'open)
(define close 'close)

;
; abstrations
;
(define connection%
  (class object%
    (super-new)
    (define internal '())
    (define/public (get-reply) internal)
    (define/public (set-state! new-state)
      (set! internal new-state))))

(define connection (make-object connection%))

(define crossing
  (make-object crossing% connection 'id 'state))

;
; individual test suites
;
(define test-make-object
  (test-suite
   "testing make-object"
   
   (test-case
    "check 'crossing%' exists"
    (check-not-exn (λ () crossing%)))
   (test-case
    "check constructor doesn't error"
    (check-not-exn (λ () (make-object crossing% 'connection 'id 'state))))
   (test-case
    "check if constructor returns an object"
    (check-true (object? (make-object crossing% 'connection 'id 'state))))
   ))

(define test-get-id
  (test-suite
   "testing get-id"

   (test-case
    "check 'get-id' doesn't error"
    (check-not-exn (λ () (send crossing get-id))))
   (test-case
    "check 'get-id' returns 'id"
    (check-eq? (send crossing get-id) 'id))
   ))

(define test-get-state
  (test-suite
   "testing get-state"

   (test-case
    "check 'get-state' doesn't error"
    (check-not-exn (λ () (send crossing get-state))))
   (test-case
    "check 'get-state' returns 'state"
    (check-eq? (send crossing get-state) 'state))
   ))

(define test-set-state!
  (test-suite
   "testing set-state"
   
   (test-case
    "check 'set-state!' doesn't error when calling 'open"
    (check-not-exn (λ () (send crossing set-state! open))))
   (test-case
    "check 'set-state!' doesn't error when calling 'close"
    (check-not-exn (λ () (send crossing set-state! close))))
   (test-case
    "check 'set-state!' does error when calling with wrong message"
    (check-exn exn:fail? (λ () (send crossing set-state! 'wrong))))
   (test-case
    "check 'set-state! 'close' closes when open"
    (let ((connection (make-object connection%))
          (crossing (make-object crossing% connection 'id open)))
      (check-not-exn (λ () (send crossing set-state! close)))))
   (test-case
    "check 'set-state! 'close' does nothing when closed"
    (let ((connection (make-object connection%))
          (crossing (make-object crossing% connection 'id close)))
      (check-not-exn (λ () (send crossing set-state! close)))))
   (test-case
    "check 'set-state! 'open' opens when closed"
    (let ((connection (make-object connection%))
          (crossing (make-object crossing% connection 'id close)))
      (check-not-exn (λ () (send crossing set-state! open)))))
   (test-case
    "check 'set-state! 'open' does nothing when open"
    (let ((connection (make-object connection%))
          (crossing (make-object crossing% connection 'id open)))
      (check-true
       (begin
         (λ () (send crossing set-state! open))
         (check-eq? (send connection get-reply) open)
         ))))
   ))

;
; running all test suites
;
(define crossing-test
  (test-suite "All crossing% operation tests"
              test-make-object
              test-get-id
              test-get-state
              test-set-state!
              ))