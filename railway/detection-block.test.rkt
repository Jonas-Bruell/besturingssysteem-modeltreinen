;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                      >>> detection-block.test.rkt <<<                      ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit "detection-block.rkt")
(provide detection-block-test)

;
; aliasses
;
(define id       'id)
(define free     'free)
(define reserved 'reserved)
(define occupied 'occupied)
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
    (define/public (get-state) state)
    (define/public (set-state! new-state)
      (set! state new-state))))

(define make-connection
  (λ () (make-object connection% generic)))

(define (make-detection-block-with connection)
  (make-object detection-block% id connection in out))

(define make-generic-detection-block
  (λ () (make-detection-block-with (make-connection))))

(define make-free-detection-block
  (λ () (make-detection-block-with (make-object connection% free))))

(define make-reserved-detection-block
  (λ () (make-detection-block-with (make-object connection% reserved))))

(define make-occupied-detection-block
  (λ () (make-detection-block-with (make-object connection% occupied))))

;
; test-make-object test suites
;
(define test-make-object
  (test-suite
   "Testing make-object"
   
   (test-case
    "check if 'detection-block%' exists"
    (check-not-exn (λ () detection-block%)))
   (test-case
    "check if constructor doesn't error"
    (check-not-exn
     (λ () (make-generic-detection-block))))
   (test-case
    "check if constructor returns an object"
    (check-true
     (object? (make-generic-detection-block))))
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
     (object-method-arity-includes? (make-generic-detection-block) 'get-id 0)))
   (test-case
    "check if 'get-id' doesn't error"
    (check-not-exn (λ () (send (make-generic-detection-block) get-id))))
   (test-case
    "check if 'get-id' returns 'id"
    (check-eq? (send (make-generic-detection-block) get-id) id))
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
     (object-method-arity-includes?
      (make-generic-detection-block) 'get-state 0)))
   (test-case
    "check if 'get-state' doesn't error"
    (check-not-exn (λ () (send (make-generic-detection-block) get-state))))
   (test-case
    "check if 'get-state' returns 'generic"
    (check-eq? (send (make-generic-detection-block) get-state) generic))
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
     (object-method-arity-includes?
      (make-generic-detection-block) 'get-next 0)))
   (test-case
    "check if 'get-next' doesn't error"
    (check-not-exn (λ () (send (make-generic-detection-block) get-next))))
   (test-case
    "check if 'get-next' returns 'out"
    (check-eq? (send (make-generic-detection-block) get-next) out))
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
     (object-method-arity-includes?
      (make-generic-detection-block) 'get-prev 0)))
   (test-case
    "check if 'get-prev' doesn't error"
    (check-not-exn (λ () (send (make-generic-detection-block) get-prev))))
   (test-case
    "check if 'get-prev' returns 'out"
    (check-eq? (send (make-generic-detection-block) get-prev) in))
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
     (object-method-arity-includes?
      (make-generic-detection-block) 'set-state! 1)))
   (test-case
    "check if 'set-state!' does error when calling with wrong message"
    (check-exn exn:fail?
               (λ () (send (make-generic-detection-block) set-state! 'wrong))))

   ;
   ; FREE SUITE :: (test-set-state! 'free) test suites
   ;
   (test-suite
    "check 'set-state!' with argemunt 'free"

    ;
    ; (test-set-state! 'free) on free detection-block test suites
    ;
    (test-suite
     "check with original state 'free"

     (test-case
      "check if (set-state! 'free) doesn't error"
      (check-not-exn
       (λ () (send (make-free-detection-block) set-state! free))))
     
     (test-case
      "check if free detection-block stays free"
      (let* ((detection-block (make-free-detection-block)))
        (send detection-block set-state! free)
        (check-eq? (send detection-block get-state) free)))
     (test-case
      "check if free detection-block returns true after freed"
      (let* ((detection-block (make-free-detection-block)))
        (check-true (send detection-block set-state! free))))
     (test-case
      "check if free connection stays free"
      (let* ((connection (make-object connection% free))
             (detection-block (make-detection-block-with connection)))
        (send detection-block set-state! free)
        (check-eq? (send connection get-state) free)))
     )
    
    ;
    ; (test-set-state! 'free) on reserved detection-block test suites
    ;
    (test-suite
     "check with original state 'reserved"

     (test-case
      "check if (set-state! 'free) doesn't error"
      (check-not-exn
       (λ () (send (make-reserved-detection-block) set-state! free))))
    
     (test-case
      "check if reserved detection-block frees"
      (let* ((detection-block (make-reserved-detection-block)))
        (send detection-block set-state! free)
        (check-eq? (send detection-block get-state) free)))
     (test-case
      "check if reserved detection-block returns true after freed"
      (let* ((detection-block (make-reserved-detection-block)))
        (check-true (send detection-block set-state! free))))
     (test-case
      "check if reserved connection frees"
      (let* ((connection (make-object connection% reserved))
             (detection-block (make-detection-block-with connection)))
        (send detection-block set-state! free)
        (check-eq? (send connection get-state) free)))
     )

    ;
    ; (test-set-state! 'free) on occupied detection-block test suites
    ;
    (test-suite
     "check with original state 'occupied"

     (test-case
      "check if (set-state! 'free) doesn't error"
      (check-not-exn
       (λ () (send (make-occupied-detection-block) set-state! free))))
    
     (test-case
      "check if occupied detection-block frees"
      (let* ((detection-block (make-occupied-detection-block)))
        (send detection-block set-state! free)
        (check-eq? (send detection-block get-state) free)))
     (test-case
      "check if occupied detection-block returns true after freed"
      (let* ((detection-block (make-occupied-detection-block)))
        (check-true (send detection-block set-state! free))))
     (test-case
      "check if occupied connection frees"
      (let* ((connection (make-object connection% occupied))
             (detection-block (make-detection-block-with connection)))
        (send detection-block set-state! free)
        (check-eq? (send connection get-state) free)))
     ))

   ;
   ; RESERVED SUITE :: (test-set-state! 'reserved) test suites
   ;
   (test-suite
    "check 'set-state!' with argemunt 'reserved"

    ;
    ; (test-set-state! 'reserved) on free detection-block test suites
    ;
    (test-suite
     "check with original state 'free"
     
     (test-case
      "check if (set-state! 'reserved) doesn't error"
      (check-not-exn
       (λ () (send (make-free-detection-block) set-state! reserved))))
    
     (test-case
      "check if free detection-block reserves"
      (let* ((detection-block (make-free-detection-block)))
        (send detection-block set-state! reserved)
        (check-eq? (send detection-block get-state) reserved)))
     (test-case
      "check if free detection-block returns true after reserved"
      (let* ((detection-block (make-free-detection-block)))
        (check-true (send detection-block set-state! reserved))))
     (test-case
      "check if free connection reserves"
      (let* ((connection (make-object connection% free))
             (detection-block (make-detection-block-with connection)))
        (send detection-block set-state! reserved)
        (check-eq? (send connection get-state) reserved)))
     )

    ;
    ; (test-set-state! 'reserved) on reserved detection-block test suites
    ;
    (test-suite
     "check with original state 'reserved"

     (test-case
      "check if (set-state! 'reserved) doesn't error"
      (check-not-exn
       (λ () (send (make-reserved-detection-block) set-state! reserved))))
     
     (test-case
      "check if reserved detection-block stays reserved"
      (let* ((detection-block (make-reserved-detection-block)))
        (send detection-block set-state! reserved)
        (check-eq? (send detection-block get-state) reserved)))
     (test-case
      "check if reserved detection-block returns false after reserved"
      (let* ((detection-block (make-reserved-detection-block)))
        (check-false (send detection-block set-state! reserved))))
     (test-case
      "check if reserved connection stays reserved"
      (let* ((connection (make-object connection% reserved))
             (detection-block (make-detection-block-with connection)))
        (send detection-block set-state! reserved)
        (check-eq? (send connection get-state) reserved)))
     )

    ;
    ; (test-set-state! 'reserved) on occupied detection-block test suites
    ;
    (test-suite
     "check with original state 'occupied"

     (test-case
      "check if (set-state! 'reserved) doesn't error"
      (check-not-exn
       (λ () (send (make-occupied-detection-block) set-state! reserved))))
     
     (test-case
      "check if occupied detection-block stays occupied"
      (let* ((detection-block (make-occupied-detection-block)))
        (send detection-block set-state! reserved)
        (check-eq? (send detection-block get-state) occupied)))
     (test-case
      "check if occupied detection-block returns false after reserved"
      (let* ((detection-block (make-occupied-detection-block)))
        (check-false (send detection-block set-state! reserved))))
     (test-case
      "check if occupied connection stays occupied"
      (let* ((connection (make-object connection% occupied))
             (detection-block (make-detection-block-with connection)))
        (send detection-block set-state! reserved)
        (check-eq? (send connection get-state) occupied)))
     ))

   ;
   ; OCCUPIED SUITE :: (test-set-state! 'reserved) test suites
   ;
   (test-suite
    "check 'set-state!' with argemunt 'occupied"

    ;
    ; (test-set-state! 'occupied) on free detection-block test suites
    ;
    (test-suite
     "check with original state 'free"
     
     (test-case
      "check if (set-state! 'occupied) doesn't error"
      (check-not-exn
       (λ () (send (make-free-detection-block) set-state! occupied))))
    
     (test-case
      "check if free detection-block occupies"
      (let* ((detection-block (make-free-detection-block)))
        (send detection-block set-state! occupied)
        (check-eq? (send detection-block get-state) occupied)))
     (test-case
      "check if free detection-block returns true after occupied"
      (let* ((detection-block (make-free-detection-block)))
        (check-true (send detection-block set-state! occupied))))
     (test-case
      "check if free connection occupies"
      (let* ((connection (make-object connection% free))
             (detection-block (make-detection-block-with connection)))
        (send detection-block set-state! occupied)
        (check-eq? (send connection get-state) occupied)))
     )

    ;
    ; (test-set-state! 'occupied) on reserved detection-block test suites
    ;
    (test-suite
     "check with original state 'reserved"

     (test-case
      "check if (set-state! 'occupied) doesn't error"
      (check-not-exn
       (λ () (send (make-reserved-detection-block) set-state! occupied))))
    
     (test-case
      "check if reserved detection-block occupies"
      (let* ((detection-block (make-reserved-detection-block)))
        (send detection-block set-state! occupied)
        (check-eq? (send detection-block get-state) occupied)))
     (test-case
      "check if reserved detection-block returns true after occupied"
      (let* ((detection-block (make-reserved-detection-block)))
        (check-true (send detection-block set-state! occupied))))
     (test-case
      "check if reserved connection occupies"
      (let* ((connection (make-object connection% reserved))
             (detection-block (make-detection-block-with connection)))
        (send detection-block set-state! occupied)
        (check-eq? (send connection get-state) occupied)))
     )

    ;
    ; (test-set-state! 'occupied) on occupied detection-block test suites
    ;
    (test-suite
     "check with original state 'occupied"

     (test-case
      "check if (set-state! 'occupied) doesn't error"
      (check-not-exn
       (λ () (send (make-occupied-detection-block) set-state! occupied))))
     
     (test-case
      "check if occupied detection-block stays occupied"
      (let* ((detection-block (make-occupied-detection-block)))
        (send detection-block set-state! occupied)
        (check-eq? (send detection-block get-state) occupied)))
     (test-case
      "check if occupied detection-block returns false after occupied"
      (let* ((detection-block (make-occupied-detection-block)))
        (check-false (send detection-block set-state! occupied))))
     (test-case
      "check if occupied connection stays occupied"
      (let* ((connection (make-object connection% occupied))
             (detection-block (make-detection-block-with connection)))
        (send detection-block set-state! occupied)
        (check-eq? (send connection get-state) occupied)))
     ))))

;
; running all test suites
;
(define detection-block-test
  (test-suite "All detection-block% operation tests"
              test-make-object
              test-get-id
              test-get-state
              test-get-next
              test-get-prev
              test-set-state!
              ))