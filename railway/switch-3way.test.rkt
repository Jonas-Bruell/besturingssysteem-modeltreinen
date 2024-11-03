;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        >>> switch-3way.test.rkt <<<                        ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit "switch-3way.rkt")
(provide switch-3way-test)

;
; aliasses
;
(define id       'id)
(define free     'free)
(define reserved 'reserved)
(define left     'left)
(define middle   'middle)
(define right    'right)
(define generic  'generic)
(define in       'in)
(define out      (list left middle right))

;
; abstrations
;
(define connection%
  (class object%
    (super-new)
    (init-field state pos)
    (define/public (get-state) state)
    (define/public (get-position) pos)
    (define/public (set-state! new-state) (set! state new-state))
    (define/public (set-position! new-pos) (set! pos new-pos))))

(define make-connection
  (λ () (make-object connection% generic generic)))

(define (make-switch-3way-with connection)
  (make-object switch-3way% id connection in out))

(define make-generic-switch-3way
  (λ () (make-switch-3way-with (make-connection))))

(define make-free-switch-3way
  (λ () (make-switch-3way-with (make-object connection% free generic))))

(define make-reserved-switch-3way
  (λ () (make-switch-3way-with (make-object connection% reserved generic))))

(define make-left-switch-3way
  (λ () (make-switch-3way-with (make-object connection% generic left))))

(define make-middle-switch-3way
  (λ () (make-switch-3way-with (make-object connection% generic middle))))

(define make-right-switch-3way
  (λ () (make-switch-3way-with (make-object connection% generic right))))

;
; test-make-object test suites
;
(define test-make-object
  (test-suite
   "Testing make-object"
   
   (test-case
    "check if 'switch-3way%' exists"
    (check-not-exn (λ () switch-3way%)))
   (test-case
    "check if constructor doesn't error"
    (check-not-exn (λ () (make-generic-switch-3way))))
   (test-case
    "check if constructor returns an object"
    (check-true (object? (make-generic-switch-3way))))
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
     (object-method-arity-includes? (make-generic-switch-3way) 'get-id 0)))
   (test-case
    "check if 'get-id' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-3way) get-id))))
   (test-case
    "check if 'get-id' returns 'id"
    (check-eq? (send (make-generic-switch-3way) get-id) id))
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
     (object-method-arity-includes? (make-generic-switch-3way) 'get-prev 0)))
   (test-case
    "check if 'get-prev' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-3way) get-prev))))
   (test-case
    "check if 'get-id' returns 'in"
    (check-eq? (send (make-generic-switch-3way) get-prev) in))
   ))

;
; test-get-next-left test suites
;
(define test-get-next-left
  (test-suite
   "Testing get-next-left"

   (test-case
    "check if 'get-next-left' exists"
    (check-true (object-method-arity-includes?
      (make-generic-switch-3way) 'get-next-left 0)))
   (test-case
    "check if 'get-next-left' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-3way) get-next-left))))
   (test-case
    "check if 'get-next-left' returns 'left"
    (check-eq? (send (make-generic-switch-3way) get-next-left) left))
   ))

;
; test-get-next-left test suites
;
(define test-get-next-middle
  (test-suite
   "Testing get-next-middle"

   (test-case
    "check if 'get-next-middle' exists"
    (check-true (object-method-arity-includes?
      (make-generic-switch-3way) 'get-next-middle 0)))
   (test-case
    "check if 'get-next-middle' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-3way) get-next-middle))))
   (test-case
    "check if 'get-next-middle' returns 'middle"
    (check-eq? (send (make-generic-switch-3way) get-next-middle) middle))
   ))

;
; test-get-next-right test suites
;
(define test-get-next-right
  (test-suite
   "Testing get-next-right"

   (test-case
    "check if 'get-next-right' exists"
    (check-true
     (object-method-arity-includes?
      (make-generic-switch-3way) 'get-next-right 0)))
   (test-case
    "check if 'get-next-right' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-3way) get-next-right))))
   (test-case
    "check if 'get-next-right' returns 'right"
    (check-eq? (send (make-generic-switch-3way) get-next-right) right))
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
     (object-method-arity-includes? (make-generic-switch-3way) 'get-state 0)))
   (test-case
    "check if 'get-state' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-3way) get-state))))
   (test-case
    "check if 'get-state' returns 'generic"
    (check-eq? (send (make-generic-switch-3way) get-state) generic))
   ))

;
; test-get-position test suites
;
(define test-get-position
  (test-suite
   "Testing get-position"

   (test-case
    "check if 'get-position' exists"
    (check-true
     (object-method-arity-includes?
      (make-generic-switch-3way) 'get-position 0)))
   (test-case
    "check if 'get-position' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-3way) get-position))))
   (test-case
    "check if 'get-position' returns 'generic"
    (check-eq? (send (make-generic-switch-3way) get-position) generic))
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
     (object-method-arity-includes? (make-generic-switch-3way) 'set-state! 1)))
   (test-case
    "check if (set-state! 'free) doesn't error on free switch-3way"
    (check-not-exn (λ () (send (make-free-switch-3way) set-state! 'free))))
   (test-case
    "check if (set-state! 'reserved) doesn't error on free switch-3way"
    (check-not-exn (λ () (send (make-free-switch-3way) set-state! 'reserved))))
   (test-case
    "check if (set-state! 'free) doesn't error on reserved switch-3way"
    (check-not-exn (λ () (send (make-reserved-switch-3way) set-state! 'free))))
   (test-case
    "check if (set-state! 'reserved) doesn't error on reserved switch-3way"
    (check-not-exn
     (λ () (send (make-reserved-switch-3way) set-state! 'reserved))))
   ))

;
; test-set-position! test suites
;
(define test-set-position!
  (test-suite
   "Testing set-position!"
   
   (test-case
    "check if 'set-position!' exists"
    (check-true
     (object-method-arity-includes?
      (make-generic-switch-3way) 'set-position! 1)))
   (test-case
    "check if 'set-position!' does error when calling with wrong message"
    (check-exn exn:fail?
               (λ () (send (make-generic-switch-3way) set-position! 'wrong))))

   ;
   ; (test-set-position! 'left) test suites
   ;
   (test-suite
    "check 'set-position!' with argemunt 'left"

    (test-case
     "check if (set-position! 'left) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way) set-position! left))))

    (test-case
     "check if left-positioned switch-3way stays left"
     (let* ((switch-3way (make-left-switch-3way)))
       (send switch-3way set-position! left)
       (check-eq? (send switch-3way get-position) left)))
    (test-case
     "check if left-positioned connection stays left"
     (let* ((connection (make-object connection% generic left))
            (switch-3way (make-switch-3way-with connection)))
       (send switch-3way set-position! left)
       (check-eq? (send connection get-position) left)))

    (test-case
     "check if middle-positioned switch-3way switches left"
     (let* ((switch-3way (make-middle-switch-3way)))
       (send switch-3way set-position! left)
       (check-eq? (send switch-3way get-position) left)))
    (test-case
     "check if middle-positioned connection switches lefts"
     (let* ((connection (make-object connection% generic middle))
            (switch-3way (make-switch-3way-with connection)))
       (send switch-3way set-position! left)
       (check-eq? (send connection get-position) left)))
    
    (test-case
     "check if right-positioned switch-3way switches left"
     (let* ((switch-3way (make-right-switch-3way)))
       (send switch-3way set-position! left)
       (check-eq? (send switch-3way get-position) left)))
    (test-case
     "check if right-positioned connection switches lefts"
     (let* ((connection (make-object connection% generic right))
            (switch-3way (make-switch-3way-with connection)))
       (send switch-3way set-position! left)
       (check-eq? (send connection get-position) left)))
    )

   ;
   ; (test-set-position! 'middle) test suites
   ;
   (test-suite
    "check 'set-position!' with argemunt 'middle"

    (test-case
     "check if (set-position! 'middle) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way) set-position! middle))))
    
    (test-case
     "check if left-positioned switch-3way stays middle"
     (let* ((switch-3way (make-left-switch-3way)))
       (send switch-3way set-position! middle)
       (check-eq? (send switch-3way get-position) middle)))
    (test-case
     "check if left-positioned connection stays middle"
     (let* ((connection (make-object connection% generic left))
            (switch-3way (make-switch-3way-with connection)))
       (send switch-3way set-position! middle)
       (check-eq? (send connection get-position) middle)))
    
    (test-case
     "check if middle-positioned switch-3way stays middle"
     (let* ((switch-3way (make-middle-switch-3way)))
       (send switch-3way set-position! middle)
       (check-eq? (send switch-3way get-position) middle)))
    (test-case
     "check if middle-positioned connection stays middle"
     (let* ((connection (make-object connection% generic middle))
            (switch-3way (make-switch-3way-with connection)))
       (send switch-3way set-position! middle)
       (check-eq? (send connection get-position) middle)))
    
    (test-case
     "check if right-positioned switch-3way switches middle"
     (let* ((switch-3way (make-right-switch-3way)))
       (send switch-3way set-position! middle)
       (check-eq? (send switch-3way get-position) middle)))
    (test-case
     "check if right-positioned connection switches middle"
     (let* ((connection (make-object connection% generic right))
            (switch-3way (make-switch-3way-with connection)))
       (send switch-3way set-position! middle)
       (check-eq? (send connection get-position) middle)))
    )

   ;
   ; (test-set-position! 'right) test suites
   ;
   (test-suite
    "check 'set-position!' with argument 'right"

    (test-case
     "check if (set-position! 'right) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way) set-position! right))))
    
    (test-case
     "check if left-positioned switch-3way switches right"
     (let* ((switch-3way (make-left-switch-3way)))
       (send switch-3way set-position! right)
       (check-eq? (send switch-3way get-position) right)))
    (test-case
     "check if left-positioned connection switches right"
     (let* ((connection (make-object connection% generic left))
            (switch-3way (make-switch-3way-with connection)))
       (send switch-3way set-position! right)
       (check-eq? (send connection get-position) right)))

    (test-case
     "check if middle-positioned switch-3way switches right"
     (let* ((switch-3way (make-middle-switch-3way)))
       (send switch-3way set-position! right)
       (check-eq? (send switch-3way get-position) right)))
    (test-case
     "check if middle-positioned connection switches right"
     (let* ((connection (make-object connection% generic middle))
            (switch-3way (make-switch-3way-with connection)))
       (send switch-3way set-position! right)
       (check-eq? (send connection get-position) right)))
    
    (test-case
     "check if right-positioned switch-3way stays right"
     (let* ((switch-3way (make-right-switch-3way)))
       (send switch-3way set-position! right)
       (check-eq? (send switch-3way get-position) right)))
    (test-case
     "check if right-positioned connection stays right"
     (let* ((connection (make-object connection% generic right))
            (switch-3way (make-switch-3way-with connection)))
       (send switch-3way set-position! right)
       (check-eq? (send connection get-position) right)))
    )))

;
; running all test suites
;
(define switch-3way-test
  (test-suite "All switch-3way% operation tests"
              test-make-object
              test-get-id
              test-get-prev
              test-get-next-left
              test-get-next-middle
              test-get-next-right
              test-get-state
              test-get-position
              test-set-state!
              test-set-position!
              ))