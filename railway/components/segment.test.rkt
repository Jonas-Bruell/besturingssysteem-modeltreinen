;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                          >>> segment.test.rkt <<<                          ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit "segment.rkt")
(provide segment-test)

;
; aliasses
;
(define id       'id)
(define free     'free)
(define reserved 'reserved)
(define generic  'generic)
(define in       'in)
(define out      'out)

;
; abstrations
;
(define connection%
  (class object%
    (super-new)
    (init-field state)
    (define/public (get-state id) state)
    (define/public (set-state! id new-state)
      (set! state new-state))
    (define/public (get-position id) generic)))

(define (make-segment-with connection)
  (make-object segment% id connection in out))

(define make-generic-segment
  (λ () (make-segment-with (make-object connection% generic))))

(define make-free-segment
  (λ () (make-segment-with (make-object connection% free))))

(define make-reserved-segment
  (λ () (make-segment-with (make-object connection% reserved))))

;
; test-make-object test suites
;
(define test-make-object
  (test-suite
   "Testing make-object"
   
   (test-case
    "check if 'segment%' exists"
    (check-not-exn (λ () segment%)))
   (test-case
    "check if constructor doesn't error"
    (check-not-exn (λ () (make-generic-segment))))
   (test-case
    "check if constructor returns an object"
    (check-true (object? (make-generic-segment))))
   ))

;
; test-get-id test suites
;
(define test-get-id
  (test-suite
   "Testing get-id"

   (test-case
    "check if 'get-id' exists"
    (check-true
     (object-method-arity-includes? (make-generic-segment) 'get-id 0)))
   (test-case
    "check if 'get-id' doesn't error"
    (check-not-exn (λ () (send (make-generic-segment) get-id))))
   (test-case
    "check if 'get-id' returns 'id"
    (check-eq? (send (make-generic-segment) get-id) id))
   ))

;
; test-get-state test suites
;
(define test-get-state
  (test-suite
   "Testing get-state"

   (test-case
    "check if 'get-state' exists"
    (check-true
     (object-method-arity-includes? (make-generic-segment) 'get-state 0)))
   (test-case
    "check if 'get-state' doesn't error"
    (check-not-exn (λ () (send (make-generic-segment) get-state))))
   (test-case
    "check if 'get-state' returns 'generic"
    (check-eq? (send (make-generic-segment) get-state) generic))
   ))

;
; test-get-next test suites
;
(define test-get-next
  (test-suite
   "Testing get-next"

   (test-case
    "check if 'get-next' exists"
    (check-true
     (object-method-arity-includes? (make-generic-segment) 'get-next 0)))
   (test-case
    "check if 'get-next' doesn't error"
    (check-not-exn (λ () (send (make-generic-segment) get-next))))
   (test-case
    "check if 'get-next' returns 'out"
    (check-eq? (send (make-generic-segment) get-next) out))
   ))

;
; test-get-prev test suites
;
(define test-get-prev
  (test-suite
   "Testing get-prev"

   (test-case
    "check if 'get-prev' exists"
    (check-true
     (object-method-arity-includes? (make-generic-segment) 'get-prev 0)))
   (test-case
    "check if 'get-prev' doesn't error"
    (check-not-exn (λ () (send (make-generic-segment) get-prev))))
   (test-case
    "check if 'get-prev' returns 'out"
    (check-eq? (send (make-generic-segment) get-prev) in))
   ))

;
; test-set-state! test suites
;
(define test-set-state!
  (test-suite
   "Testing set-state!"
   
   (test-case
    "check if 'set-state!' exists"
    (check-true
     (object-method-arity-includes? (make-generic-segment) 'set-state! 1)))
   (test-case
    "check if 'set-state!' does error when calling with wrong message"
    (check-exn exn:fail?
               (λ () (send (make-generic-segment) set-state! 'wrong))))

   ;
   ; FREE :: (test-set-state! 'free) test suites
   ;
   (test-suite
    "check 'set-state!' with argemunt 'free"

    ;
    ; (test-set-state! 'free) on free segment test suites
    ;
    (test-suite
     "check with original state 'free"

     (test-case
      "check if (set-state! 'free) doesn't error"
      (check-not-exn
       (λ () (send (make-free-segment) set-state! free))))
     
     (test-case
      "check if free segment stays free"
      (let* ((segment (make-free-segment)))
        (send segment set-state! free)
        (check-eq? (send segment get-state) free)))
     (test-case
      "check if free segment returns true after freed"
      (let* ((segment (make-free-segment)))
        (check-true (send segment set-state! free))))
     (test-case
      "check if free connection stays free"
      (let* ((connection (make-object connection% free))
             (segment (make-segment-with connection)))
        (send segment set-state! free)
        (check-eq? (send connection get-state id) free)))
     )
    
    ;
    ; (test-set-state! 'free) on reserved segment test suites
    ;
    (test-suite
     "check with original state 'reserved"

     (test-case
      "check if (set-state! 'free) doesn't error"
      (check-not-exn
       (λ () (send (make-reserved-segment) set-state! free))))
    
     (test-case
      "check if reserved segment frees"
      (let* ((segment (make-reserved-segment)))
        (send segment set-state! free)
        (check-eq? (send segment get-state) free)))
     (test-case
      "check if reserved segment returns true after freed"
      (let* ((segment (make-reserved-segment)))
        (check-true (send segment set-state! free))))
     (test-case
      "check if reserved connection frees"
      (let* ((connection (make-object connection% reserved))
             (segment (make-segment-with connection)))
        (send segment set-state! free)
        (check-eq? (send connection get-state id) free)))
     ))

   ;
   ; RESERVED :: (test-set-state! 'reserved) test suites
   ;
   (test-suite
    "check 'set-state!' with argemunt 'reserved"

    ;
    ; (test-set-state! 'reserved) on free segment test suites
    ;
    (test-suite
     "check with original state 'free"
     
     (test-case
      "check if (set-state! 'reserved) doesn't error"
      (check-not-exn
       (λ () (send (make-free-segment) set-state! reserved))))
    
     (test-case
      "check if free segment reserves"
      (let* ((segment (make-free-segment)))
        (send segment set-state! reserved)
        (check-eq? (send segment get-state) reserved)))
     (test-case
      "check if free segment returns true after reserved"
      (let* ((segment (make-free-segment)))
        (check-true (send segment set-state! reserved))))
     (test-case
      "check if free connection reserves"
      (let* ((connection (make-object connection% free))
             (segment (make-segment-with connection)))
        (send segment set-state! reserved)
        (check-eq? (send connection get-state id) reserved)))
     )

    ;
    ; (test-set-state! 'reserved) on reserved segment test suites
    ;
    (test-suite
     "check with original state 'reserved"

     (test-case
      "check if (set-state! 'reserved) doesn't error"
      (check-not-exn
       (λ () (send (make-reserved-segment) set-state! reserved))))
     
     (test-case
      "check if reserved segment stays reserved"
      (let* ((segment (make-reserved-segment)))
        (send segment set-state! reserved)
        (check-eq? (send segment get-state) reserved)))
     (test-case
      "check if reserved segment returns false after reserved"
      (let* ((segment (make-reserved-segment)))
        (check-false (send segment set-state! reserved))))
     (test-case
      "check if reserved connection stays reserved"
      (let* ((connection (make-object connection% reserved))
             (segment (make-segment-with connection)))
        (send segment set-state! reserved)
        (check-eq? (send connection get-state id) reserved)))
     ))))

;
; running all test suites
;
(define segment-test
  (test-suite "All segment% operation tests"
              test-make-object
              test-get-id
              test-get-state
              test-get-next
              test-get-prev
              test-set-state!
              ))