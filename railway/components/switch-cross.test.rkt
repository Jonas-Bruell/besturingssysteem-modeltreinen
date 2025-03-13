;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        >>> switch-cross.test.rkt <<<                       ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         rackunit/gui
         "switch-cross.rkt")
(provide switch-cross-test)

;
; aliasses
;
(define id        'id)
(define free      'free)
(define reserved  'reserved)
(define in-left   'in-left)
(define in-right  'in-right)
(define out-left  'out-left)
(define out-right 'out-right)
(define generic   'generic)
(define in        (cons in-left in-right))
(define out       (cons out-left out-right))

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

(define (make-switch-cross-with connection)
  (make-object switch-cross% id connection in out))

(define make-generic-switch-cross
  (λ () (make-switch-cross-with (make-connection))))

(define make-free-switch-cross
  (λ () (make-switch-cross-with (make-object connection% free generic))))

(define make-reserved-switch-cross
  (λ () (make-switch-cross-with (make-object connection% reserved generic))))

(define make-in-left-switch-cross
  (λ () (make-switch-cross-with (make-object connection% generic in-left))))

(define make-in-right-switch-cross
  (λ () (make-switch-cross-with (make-object connection% generic in-right))))

(define make-out-left-switch-cross
  (λ () (make-switch-cross-with (make-object connection% generic out-left))))

(define make-out-right-switch-cross
  (λ () (make-switch-cross-with (make-object connection% generic out-right))))

;
; test-make-object test suites
;
(define test-make-object
  (test-suite
   "Testing make-object"
   
   (test-case
    "check if 'switch-cross%' exists"
    (check-not-exn (λ () switch-cross%)))
   (test-case
    "check if constructor doesn't error"
    (check-not-exn (λ () (make-generic-switch-cross))))
   (test-case
    "check if constructor returns an object"
    (check-true (object? (make-generic-switch-cross))))
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
     (object-method-arity-includes? (make-generic-switch-cross) 'get-id 0)))
   (test-case
    "check if 'get-id' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-cross) get-id))))
   (test-case
    "check if 'get-id' returns 'id"
    (check-eq? (send (make-generic-switch-cross) get-id) id))
   ))

;
; test-get-prev-left test suites
;
(define test-get-prev-left
  (test-suite
   "Testing get-prev-left"

   (test-case
    "check if 'get-prev-left' exists"
    (check-true (object-method-arity-includes?
                 (make-generic-switch-cross) 'get-prev-left 0)))
   (test-case
    "check if 'get-prev-left' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-cross) get-prev-left))))
   (test-case
    "check if 'get-prev-left' returns 'in-left"
    (check-eq? (send (make-generic-switch-cross) get-prev-left) in-left))
   ))

;
; test-get-prev-right test suites
;
(define test-get-prev-right
  (test-suite
   "Testing get-prev-right"

   (test-case
    "check if 'get-prev-right' exists"
    (check-true (object-method-arity-includes?
                 (make-generic-switch-cross) 'get-prev-right 0)))
   (test-case
    "check if 'get-prev-right' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-cross) get-prev-right))))
   (test-case
    "check if 'get-prev-right' returns 'in-right"
    (check-eq? (send (make-generic-switch-cross) get-prev-right) in-right))
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
                 (make-generic-switch-cross) 'get-next-left 0)))
   (test-case
    "check if 'get-next-left' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-cross) get-next-left))))
   (test-case
    "check if 'get-next-left' returns 'out-left"
    (check-eq? (send (make-generic-switch-cross) get-next-left) out-left))
   ))

;
; test-get-next-right test suites
;
(define test-get-next-right
  (test-suite
   "Testing get-next-right"

   (test-case
    "check if 'get-next-right' exists"
    (check-true (object-method-arity-includes?
                 (make-generic-switch-cross) 'get-next-right 0)))
   (test-case
    "check if 'get-next-right' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-cross) get-next-right))))
   (test-case
    "check if 'get-next-right' returns 'out-right"
    (check-eq? (send (make-generic-switch-cross) get-next-right) out-right))
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
     (object-method-arity-includes? (make-generic-switch-cross) 'get-state 0)))
   (test-case
    "check if 'get-state' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-cross) get-state))))
   (test-case
    "check if 'get-state' returns 'generic"
    (check-eq? (send (make-generic-switch-cross) get-state) generic))
   ))

;
; test-get-position test suites
;
(define test-get-position
  (test-suite
   "Testing get-position"

   (test-case
    "check if 'get-position' exists"
    (check-true (object-method-arity-includes?
                 (make-generic-switch-cross) 'get-position 0)))
   (test-case
    "check if 'get-position' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-cross) get-position))))
   (test-case
    "check if 'get-position' returns 'generic"
    (check-eq? (send (make-generic-switch-cross) get-position) generic))
   ))

;
; test-set-state! test suites
;
(define test-set-state!
  (test-suite
   "Testing set-state!"

   (test-case
    "check if 'set-state!' exists"
    (check-true (object-method-arity-includes?
                 (make-generic-switch-cross) 'set-state! 1)))
   (test-case
    "check if (set-state! 'free) doesn't error on free switch-cross"
    (check-not-exn (λ () (send (make-free-switch-cross) set-state! 'free))))
   (test-case
    "check if (set-state! 'reserved) doesn't error on free switch-cross"
    (check-not-exn
     (λ () (send (make-free-switch-cross) set-state! 'reserved))))
   (test-case
    "check if (set-state! 'free) doesn't error on reserved switch-cross"
    (check-not-exn
     (λ () (send (make-reserved-switch-cross) set-state! 'free))))
   (test-case
    "check if (set-state! 'reserved) doesn't error on reserved switch-cross"
    (check-not-exn
     (λ () (send (make-reserved-switch-cross) set-state! 'reserved))))
   ))

;
; test-set-position! test suites
;
(define test-set-position!
  (test-suite
   "Testing set-position!"
   
   (test-case
    "check if 'set-position!' exists"
    (check-true (object-method-arity-includes?
                 (make-generic-switch-cross) 'set-position! 1)))
   (test-case
    "check if 'set-position!' does error when calling with wrong message"
    (check-exn exn:fail?
               (λ () (send (make-generic-switch-cross) set-position! 'wrong))))

   ;
   ; (test-set-position! 'in-left) test suites
   ;
   (test-suite
    "check 'set-position!' with argemunt 'in-left"

    (test-case
     "check if (set-position! 'in-left) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-cross) set-position! in-left))))
    
    (test-case
     "check if in-left-positioned switch-cross stays in-left"
     (let* ((switch-cross (make-in-left-switch-cross)))
       (send switch-cross set-position! in-left)
       (check-eq? (send switch-cross get-position) in-left)))
    (test-case
     "check if in-left-positioned connection stays in-left"
     (let* ((connection (make-object connection% generic in-left))
            (switch-cross (make-switch-cross-with connection)))
       (send switch-cross set-position! in-left)
       (check-eq? (send connection get-position) in-left)))
    
    (test-case
     "check if in-right-positioned switch-cross switches in-left"
     (let* ((switch-cross (make-in-right-switch-cross)))
       (send switch-cross set-position! in-left)
       (check-eq? (send switch-cross get-position) in-left)))
    (test-case
     "check if in-right-positioned connection switches in-left"
     (let* ((connection (make-object connection% generic in-right))
            (switch-cross (make-switch-cross-with connection)))
       (send switch-cross set-position! in-left)
       (check-eq? (send connection get-position) in-left)))
    )

   ;
   ; (test-set-position! 'in-right) test suites
   ;
   (test-suite
    "check 'set-position!' with argument 'in-right"

    (test-case
     "check if (set-position! 'in-right) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-cross) set-position! in-right))))
    
    (test-case
     "check if in-left-positioned switch-cross switches in-right"
     (let* ((switch-cross (make-in-left-switch-cross)))
       (send switch-cross set-position! in-right)
       (check-eq? (send switch-cross get-position) in-right)))
    (test-case
     "check if in-left-positioned connection switches in-right"
     (let* ((connection (make-object connection% generic in-left))
            (switch-cross (make-switch-cross-with connection)))
       (send switch-cross set-position! in-right)
       (check-eq? (send connection get-position) in-right)))
    
    (test-case
     "check if in-right-positioned switch-cross stays in-right"
     (let* ((switch-cross (make-in-right-switch-cross)))
       (send switch-cross set-position! in-right)
       (check-eq? (send switch-cross get-position) in-right)))
    (test-case
     "check if in-right-positioned connection stays in-right"
     (let* ((connection (make-object connection% generic in-right))
            (switch-cross (make-switch-cross-with connection)))
       (send switch-cross set-position! in-right)
       (check-eq? (send connection get-position) in-right)))
    )

   ;
   ; (test-set-position! 'out-left) test suites
   ;
   (test-suite
    "check 'set-position!' with argemunt 'out-left"

    (test-case
     "check if (set-position! 'out-left) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-cross) set-position! out-left))))
    
    (test-case
     "check if out-left-positioned switch-cross stays out-left"
     (let* ((switch-cross (make-out-left-switch-cross)))
       (send switch-cross set-position! out-left)
       (check-eq? (send switch-cross get-position) out-left)))
    (test-case
     "check if out-left-positioned connection stays out-left"
     (let* ((connection (make-object connection% generic out-left))
            (switch-cross (make-switch-cross-with connection)))
       (send switch-cross set-position! out-left)
       (check-eq? (send connection get-position) out-left)))
    
    (test-case
     "check if out-right-positioned switch-cross switches out-left"
     (let* ((switch-cross (make-out-right-switch-cross)))
       (send switch-cross set-position! out-left)
       (check-eq? (send switch-cross get-position) out-left)))
    (test-case
     "check if out-right-positioned connection switches out-left"
     (let* ((connection (make-object connection% generic out-right))
            (switch-cross (make-switch-cross-with connection)))
       (send switch-cross set-position! out-left)
       (check-eq? (send connection get-position) out-left)))
    )

   ;
   ; (test-set-position! 'out-right) test suites
   ;
   (test-suite
    "check 'set-position!' with argument 'out-right"

    (test-case
     "check if (set-position! 'out-right) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-cross) set-position! out-right))))
    
    (test-case
     "check if out-left-positioned switch-cross switches out-right"
     (let* ((switch-cross (make-out-left-switch-cross)))
       (send switch-cross set-position! out-right)
       (check-eq? (send switch-cross get-position) out-right)))
    (test-case
     "check if out-left-positioned connection switches out-right"
     (let* ((connection (make-object connection% generic out-left))
            (switch-cross (make-switch-cross-with connection)))
       (send switch-cross set-position! out-right)
       (check-eq? (send connection get-position) out-right)))
    
    (test-case
     "check if out-right-positioned switch-cross stays out-right"
     (let* ((switch-cross (make-out-right-switch-cross)))
       (send switch-cross set-position! out-right)
       (check-eq? (send switch-cross get-position) out-right)))
    (test-case
     "check if out-right-positioned connection stays out-right"
     (let* ((connection (make-object connection% generic out-right))
            (switch-cross (make-switch-cross-with connection)))
       (send switch-cross set-position! out-right)
       (check-eq? (send connection get-position) out-right)))
    )))
;
; running all test suites
;
(define switch-cross-test
  (test-suite "All switch-cross% operation tests"
              
              test-make-object
              test-get-id
              test-get-prev-left
              test-get-prev-right
              test-get-next-left
              test-get-next-right
              test-get-state
              test-get-position
              test-set-state!
              test-set-position!
              ))