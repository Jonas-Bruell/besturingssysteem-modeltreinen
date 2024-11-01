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
(define generic 'generic)

;
; abstrations
;
(define connection%
  (class object%
    (super-new)
    (init-field state)
    (define/public (get-state) state)
    (define/public (set-state! new-state)
      (set! state new-state))))

(define make-connection
  (λ () (make-object connection% generic)))

(define make-generic-crossing
  (λ () (make-object crossing% 'id (make-connection))))

(define make-open-crossing
  (λ () (make-object crossing% 'id (make-object connection% open))))

(define make-closed-crossing
  (λ () (make-object crossing% 'id (make-object connection% close))))

;
; individual test suites
;
(define test-make-object
  (test-suite
   "Testing make-object"
   
   (test-case
    "check if 'crossing%' exists"
    (check-not-exn (λ () crossing%)))
   (test-case
    "check if constructor doesn't error"
    (check-not-exn (λ () (make-object crossing% 'id (make-connection)))))
   (test-case
    "check if constructor returns an object"
    (check-true (object? (make-object crossing% 'id (make-connection)))))
   ))

(define test-get-id
  (test-suite
   "Testing get-id"

   (test-case
    "check if 'get-id' exists"
    (check-true
     (object-method-arity-includes? (make-generic-crossing) 'get-id 0)))
   (test-case
    "check if 'get-id' doesn't error"
    (check-not-exn (λ () (send (make-generic-crossing) get-id))))
   (test-case
    "check if 'get-id' returns 'id"
    (check-eq? (send (make-generic-crossing) get-id) 'id))
   ))

(define test-get-state
  (test-suite
   "Testing get-state"

   (test-case
    "check if 'get-state' exists"
    (check-true
     (object-method-arity-includes? (make-generic-crossing) 'get-state 0)))
   (test-case
    "check if 'get-state' doesn't error"
    (check-not-exn (λ () (send (make-generic-crossing) get-state))))
   (test-case
    "check if 'get-state' returns 'generic"
    (check-eq? (send (make-generic-crossing) get-state) generic))
   ))

(define test-set-state!
  (test-suite
   "Testing set-state"
   
   (test-case
    "check if 'set-state!' exists"
    (check-true
     (object-method-arity-includes? (make-generic-crossing) 'set-state! 1)))
   (test-case
    "check if 'set-state!' doesn't error when calling 'open"
    (check-not-exn (λ () (send (make-generic-crossing) set-state! open))))
   (test-case
    "check if 'set-state!' doesn't error when calling 'close"
    (check-not-exn (λ () (send (make-generic-crossing) set-state! close))))
   (test-case
    "check if (set-state! 'close) closes when open"
    (let* ((connection (make-object connection% open))
           (crossing (make-object crossing% 'id connection)))
      (send crossing set-state! close)
      (check-eq? (send connection get-state) close)))
   (test-case
    "check if (set-state! 'close) stays closed when closed"
    (let* ((connection (make-object connection% close))
           (crossing (make-object crossing% 'id connection)))
      (send crossing set-state! close)
      (check-eq? (send connection get-state) close)))
   (test-case
    "check if (set-state! 'open) opens when closed"
    (let* ((connection (make-object connection% close))
           (crossing (make-object crossing% 'id connection)))
      (send crossing set-state! open)
      (check-eq? (send connection get-state) open)))
   (test-case
    "check if (set-state! 'open) stays open when open"
    (let* ((connection (make-object connection% open))
           (crossing (make-object crossing% 'id connection)))
      (send crossing set-state! open)
      (check-eq? (send connection get-state) open)))
   (test-case
    "check if 'set-state!' does error when calling with wrong message"
    (check-exn exn:fail?
               (λ () (send (make-generic-crossing) set-state! 'wrong))))
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