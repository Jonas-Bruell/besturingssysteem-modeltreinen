;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        >>> switch-3way.test.rkt <<<                        ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit
         "switch.rkt"
         "switch-3way-abstract.rkt"
         "switch-3way-left.rkt"
         "switch-3way-right.rkt")
(provide switch-3way-test)

;
; aliasses
;
(define id        'id)
(define primary   'primary)
(define secondary 'secondary)
(define ids       (list id primary secondary))
(define free      'free)
(define reserved  'reserved)
(define left      'left)
(define middle    'middle)
(define right     'right)
(define generic   'generic)
(define dummy     'dummy)
(define in        'in)
(define out       (list left middle right))

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

;; switch-3way-abstract
(define switch-3way-generic%
  (class switch-3way-abstract%
    (super-new)
    (inherit-field id connection in out)
    (inherit-field state position)
    (define/override (make-primary-switch primary secondary generic)
      (thunk (make-object switch%
               generic connection generic (cons generic generic))))
    (define/override (make-secondary-switch primary secondary generic dummy)
      (thunk (make-object switch%
               generic connection generic (cons generic generic))))
    (define/override (set-position-left! generic dummy) (thunk (void)))
    (define/override (set-position-middle! generic dummy) (thunk (void)))
    (define/override (set-position-right! generic dummy) (thunk (void)))
    ))

(define (make-switch-3way-generic-with connection)
  (make-object switch-3way-generic% ids connection in out))

(define make-generic-switch-3way-generic
  (λ () (make-switch-3way-generic-with (make-connection))))

(define make-free-switch-3way-generic
  (λ () (make-switch-3way-generic-with
         (make-object connection% free generic))))

(define make-reserved-switch-3way-generic
  (λ () (make-switch-3way-generic-with
         (make-object connection% reserved generic))))

;; switch-3way-left
(define (make-switch-3way-left-with connection)
  (make-object switch-3way-left% ids connection in out))

(define make-generic-switch-3way-left
  (λ ()
    (make-switch-3way-left-with (make-object connection% generic generic))))

(define make-left-switch-3way-left
  (λ () (make-switch-3way-left-with (make-object connection% generic left))))

(define make-middle-switch-3way-left
  (λ () (make-switch-3way-left-with (make-object connection% generic middle))))

(define make-right-switch-3way-left
  (λ () (make-switch-3way-left-with (make-object connection% generic right))))

;; switch-3way-right
(define (make-switch-3way-right-with connection)
  (make-object switch-3way-right% ids connection in out))

(define make-generic-switch-3way-right
  (λ ()
    (make-switch-3way-right-with (make-object connection% generic generic))))

(define make-left-switch-3way-right
  (λ () (make-switch-3way-right-with (make-object connection% generic left))))

(define make-middle-switch-3way-right
  (λ () (make-switch-3way-right-with (make-object connection% generic middle))))

(define make-right-switch-3way-right
  (λ () (make-switch-3way-right-with (make-object connection% generic right))))

;
; test-make-object test suites
;
(define test-make-object
  (test-suite
   "Testing make-object"

   ;; abstract
   (test-suite
    "Testing make-object of switch-3way-abstract%"
   
    (test-case
     "check if 'switch-3way-abstract%' exists"
     (check-not-exn (λ () switch-3way-abstract%)))
    (test-case
     "check if constructor doesn't error"
     (check-not-exn (λ () (make-generic-switch-3way-generic))))
    (test-case
     "check if constructor returns an object"
     (check-true (object? (make-generic-switch-3way-generic))))
    )

   ;; left
   (test-suite
    "Testing make-object of switch-3way-left%"
   
    (test-case
     "check if 'switch-3way-left%' exists"
     (check-not-exn (λ () switch-3way-left%)))
    (test-case
     "check if constructor doesn't error"
     (check-not-exn (λ () (make-generic-switch-3way-left))))
    (test-case
     "check if constructor returns an object"
     (check-true (object? (make-generic-switch-3way-left))))
    )

   ;; right
   (test-suite
    "Testing make-object of switch-3way-right%"
   
    (test-case
     "check if 'switch-3way-right%' exists"
     (check-not-exn (λ () switch-3way-right%)))
    (test-case
     "check if constructor doesn't error"
     (check-not-exn (λ () (make-generic-switch-3way-right))))
    (test-case
     "check if constructor returns an object"
     (check-true (object? (make-generic-switch-3way-right))))
    )))

;
; test-get-id test suites
;
(define test-get-id
  (test-suite
   "Testing get-id"

   ;; abstract
   (test-suite
    "Testing get-id of switch-3way-abstract%"

    (test-case
     "check if 'get-id' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-generic) 'get-id 0)))
    (test-case
     "check if 'get-id' doesn't error"
     (check-not-exn (λ () (send (make-generic-switch-3way-generic) get-id))))
    (test-case
     "check if 'get-id' returns 'id"
     (check-eq? (send (make-generic-switch-3way-generic) get-id) id))
    )

   ;; left
   (test-suite
    "Testing get-id of switch-3way-left%"

    (test-case
     "check if 'get-id' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-left) 'get-id 0)))
    (test-case
     "check if 'get-id' doesn't error"
     (check-not-exn (λ () (send (make-generic-switch-3way-left) get-id))))
    (test-case
     "check if 'get-id' returns 'id"
     (check-eq? (send (make-generic-switch-3way-left) get-id) id))
    )

   ;; right
   (test-suite
    "Testing get-id of switch-3way-right%"

    (test-case
     "check if 'get-id' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-right) 'get-id 0)))
    (test-case
     "check if 'get-id' doesn't error"
     (check-not-exn (λ () (send (make-generic-switch-3way-right) get-id))))
    (test-case
     "check if 'get-id' returns 'id"
     (check-eq? (send (make-generic-switch-3way-right) get-id) id))
    )))

;
; test-get-prev test suites
;
(define test-get-prev
  (test-suite
   "Testing get-prev"

   ;; abstract
   (test-suite
    "Testing get-prev of switch-3way-abstract%"
   
    (test-case
     "check if 'get-prev' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-generic) 'get-prev 0)))
    (test-case
     "check if 'get-prev' doesn't error"
     (check-not-exn (λ () (send (make-generic-switch-3way-generic) get-prev))))
    (test-case
     "check if 'get-id' returns 'in"
     (check-eq? (send (make-generic-switch-3way-generic) get-prev) in))
    )
   
   ;; left
   (test-suite
    "Testing get-prev of switch-3way-left%"
   
    (test-case
     "check if 'get-prev' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-left) 'get-prev 0)))
    (test-case
     "check if 'get-prev' doesn't error"
     (check-not-exn (λ () (send (make-generic-switch-3way-left) get-prev))))
    (test-case
     "check if 'get-id' returns 'in"
     (check-eq? (send (make-generic-switch-3way-left) get-prev) in))
    )
   
   ;; right
   (test-suite
    "Testing get-prev of switch-3way-right%"
   
    (test-case
     "check if 'get-prev' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-right) 'get-prev 0)))
    (test-case
     "check if 'get-prev' doesn't error"
     (check-not-exn (λ () (send (make-generic-switch-3way-right) get-prev))))
    (test-case
     "check if 'get-id' returns 'in"
     (check-eq? (send (make-generic-switch-3way-right) get-prev) in))
    )))

;
; test-get-next-left test suites
;
(define test-get-next-left
  (test-suite
   "Testing get-next-left"

   ;; abstract
   (test-suite
    "Testing get-next-left on switch-3way-abstract%"

    (test-case
     "check if 'get-next-left' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-generic) 'get-next-left 0)))
    (test-case
     "check if 'get-next-left' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-generic) get-next-left))))
    (test-case
     "check if 'get-next-left' returns 'left"
     (check-eq? (send (make-generic-switch-3way-generic) get-next-left) left))
    )
   
   ;; left
   (test-suite
    "Testing get-next-left on switch-3way-left%"

    (test-case
     "check if 'get-next-left' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-left) 'get-next-left 0)))
    (test-case
     "check if 'get-next-left' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-left) get-next-left))))
    (test-case
     "check if 'get-next-left' returns 'left"
     (check-eq? (send (make-generic-switch-3way-left) get-next-left) left))
    )
   
   ;; right
   (test-suite
    "Testing get-next-left on switch-3way-right%"

    (test-case
     "check if 'get-next-left' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-right) 'get-next-left 0)))
    (test-case
     "check if 'get-next-left' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-right) get-next-left))))
    (test-case
     "check if 'get-next-left' returns 'left"
     (check-eq? (send (make-generic-switch-3way-right) get-next-left) left))
    )))

;
; test-get-next-middle test suites
;
(define test-get-next-middle
  (test-suite
   "Testing get-next-middle"

   ;; abstract
   (test-suite
    "Testing get-next-middle on switch-3way-abstract%"

    (test-case
     "check if 'get-next-middle' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-generic) 'get-next-middle 0)))
    (test-case
     "check if 'get-next-middle' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-generic) get-next-middle))))
    (test-case
     "check if 'get-next-middle' returns 'middle"
     (check-eq? (send (make-generic-switch-3way-generic) get-next-middle)
                middle))
    )

   ;; left
   (test-suite
    "Testing get-next-middle on switch-3way-left%"
    
    (test-case
     "check if 'get-next-middle' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-left) 'get-next-middle 0)))
    (test-case
     "check if 'get-next-middle' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-left) get-next-middle))))
    (test-case
     "check if 'get-next-middle' returns 'middle"
     (check-eq? (send (make-generic-switch-3way-left) get-next-middle)
                middle))
    )

   ;; right
   (test-suite
    "Testing get-next-middle on switch-3way-right%"
    
    (test-case
     "check if 'get-next-middle' exists"
     (check-true (object-method-arity-includes?
                  (make-generic-switch-3way-right) 'get-next-middle 0)))
    (test-case
     "check if 'get-next-middle' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-right) get-next-middle))))
    (test-case
     "check if 'get-next-middle' returns 'middle"
     (check-eq? (send (make-generic-switch-3way-right) get-next-middle)
                middle))
    )))

;
; test-get-next-right test suites
;
(define test-get-next-right
  (test-suite
   "Testing get-next-right"

   ;; abstract
   (test-suite
    "Testing get-next-right on switch-3way-abstract%"

    (test-case
     "check if 'get-next-right' exists"
     (check-true
      (object-method-arity-includes?
       (make-generic-switch-3way-generic) 'get-next-right 0)))
    (test-case
     "check if 'get-next-right' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-generic) get-next-right))))
    (test-case
     "check if 'get-next-right' returns 'right"
     (check-eq? (send (make-generic-switch-3way-generic) get-next-right) right))
    )
   
   ;; left
   (test-suite
    "Testing get-next-right on switch-3way-left%"

    (test-case
     "check if 'get-next-right' exists"
     (check-true
      (object-method-arity-includes?
       (make-generic-switch-3way-left) 'get-next-right 0)))
    (test-case
     "check if 'get-next-right' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-left) get-next-right))))
    (test-case
     "check if 'get-next-right' returns 'right"
     (check-eq? (send (make-generic-switch-3way-left) get-next-right) right))
    )
   
   ;; abstract
   (test-suite
    "Testing get-next-right on switch-3way-right%"

    (test-case
     "check if 'get-next-right' exists"
     (check-true
      (object-method-arity-includes?
       (make-generic-switch-3way-right) 'get-next-right 0)))
    (test-case
     "check if 'get-next-right' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-right) get-next-right))))
    (test-case
     "check if 'get-next-right' returns 'right"
     (check-eq? (send (make-generic-switch-3way-right) get-next-right) right))
    )))

;
; test-get-state test suites
;
(define test-get-state
  (test-suite
   "Testing get-state"

   (test-case
    "check if 'get-state' exists"
    (check-true (object-method-arity-includes?
                 (make-generic-switch-3way-generic) 'get-state 0)))
   (test-case
    "check if 'get-state' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch-3way-generic) get-state))))
   (test-case
    "check if 'get-state' returns 'generic"
    (check-eq? (send (make-generic-switch-3way-generic) get-state) generic))
   ))

;
; test-get-position test suites
;
(define test-get-position
  (test-suite
   "Testing get-position"

   ;; abstract
   (test-suite
    "Testing get-position on switch-3way-abstract%"

    (test-case
     "check if 'get-position' exists"
     (check-true
      (object-method-arity-includes?
       (make-generic-switch-3way-generic) 'get-position 0)))
    (test-case
     "check if 'get-position' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-generic) get-position))))
    (test-case
     "check if 'get-position' returns 'generic"
     (check-eq? (send (make-generic-switch-3way-generic) get-position) generic))
    )

   ;; left
   (test-suite
    "Testing get-position on switch-3way-left%"

    (test-case
     "check if 'get-position' exists"
     (check-true
      (object-method-arity-includes?
       (make-generic-switch-3way-left) 'get-position 0)))
    (test-case
     "check if 'get-position' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-left) get-position))))
    (test-case
     "check if 'get-position' returns 'generic"
     (check-eq? (send (make-generic-switch-3way-left) get-position) generic))
    )

   ;; right
   (test-suite
    "Testing get-position on switch-3way-right%"

    (test-case
     "check if 'get-position' exists"
     (check-true
      (object-method-arity-includes?
       (make-generic-switch-3way-right) 'get-position 0)))
    (test-case
     "check if 'get-position' doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch-3way-right) get-position))))
    (test-case
     "check if 'get-position' returns 'generic"
     (check-eq? (send (make-generic-switch-3way-right) get-position) generic))
    )))

;
; test-set-state! test suites
;
(define test-set-state!
  (test-suite
   "Testing set-state!"

   (test-case
    "check if 'set-state!' exists"
    (check-true (object-method-arity-includes?
                 (make-generic-switch-3way-generic) 'set-state! 1)))
   (test-case
    "check if (set-state! 'free) doesn't error on free switch-3way"
    (check-not-exn
     (λ () (send (make-free-switch-3way-generic) set-state! 'free))))
   (test-case
    "check if (set-state! 'reserved) doesn't error on free switch-3way"
    (check-not-exn
     (λ () (send (make-free-switch-3way-generic) set-state! 'reserved))))
   (test-case
    "check if (set-state! 'free) doesn't error on reserved switch-3way"
    (check-not-exn
     (λ () (send (make-reserved-switch-3way-generic) set-state! 'free))))
   (test-case
    "check if (set-state! 'reserved) doesn't error on reserved switch-3way"
    (check-not-exn
     (λ () (send (make-reserved-switch-3way-generic) set-state! 'reserved))))
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
      (make-generic-switch-3way-generic) 'set-position! 1)))
   (test-case
    "check if 'set-position!' does error when calling with wrong message"
    (check-exn exn:fail? (λ () (send (make-generic-switch-3way-generic)
                                     set-position! 'wrong))))
   
   ;; left
   (test-suite
    "Testing set-position! on switch-3way-left%"
   
    ;
    ; (test-set-position! 'left) test suites
    ;
    (test-suite
     "check 'set-position!' with argemunt 'left"

     (test-case
      "check if (set-position! 'left) doesn't error"
      (check-not-exn
       (λ () (send (make-generic-switch-3way-left) set-position! left))))

     (test-case
      "check if left-positioned switch-3way-left stays left"
      (let* ((switch-3way-left (make-left-switch-3way-left)))
        (send switch-3way-left set-position! left)
        (check-eq? (send switch-3way-left get-position) left)))
     (test-case
      "check if left-positioned connection stays left"
      (let* ((connection (make-object connection% generic left))
             (switch-3way-left (make-switch-3way-left-with connection)))
        (send switch-3way-left set-position! left)
        (check-eq? (send connection get-position) left)))

     (test-case
      "check if middle-positioned switch-3way-left switches left"
      (let* ((switch-3way-left (make-middle-switch-3way-left)))
        (send switch-3way-left set-position! left)
        (check-eq? (send switch-3way-left get-position) left)))
     (test-case
      "check if middle-positioned connection switches lefts"
      (let* ((connection (make-object connection% generic middle))
             (switch-3way-left (make-switch-3way-left-with connection)))
        (send switch-3way-left set-position! left)
        (check-eq? (send connection get-position) left)))
    
     (test-case
      "check if right-positioned switch-3way-left switches left"
      (let* ((switch-3way-left (make-right-switch-3way-left)))
        (send switch-3way-left set-position! left)
        (check-eq? (send switch-3way-left get-position) left)))
     (test-case
      "check if right-positioned connection switches lefts"
      (let* ((connection (make-object connection% generic right))
             (switch-3way-left (make-switch-3way-left-with connection)))
        (send switch-3way-left set-position! left)
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
       (λ () (send (make-generic-switch-3way-generic) set-position! middle))))
    
     (test-case
      "check if left-positioned switch-3way-left stays middle"
      (let* ((switch-3way-left (make-left-switch-3way-left)))
        (send switch-3way-left set-position! middle)
        (check-eq? (send switch-3way-left get-position) middle)))
     (test-case
      "check if left-positioned connection stays middle"
      (let* ((connection (make-object connection% generic left))
             (switch-3way-left (make-switch-3way-left-with connection)))
        (send switch-3way-left set-position! middle)
        (check-eq? (send connection get-position) middle)))
    
     (test-case
      "check if middle-positioned switch-3way-left stays middle"
      (let* ((switch-3way-left (make-middle-switch-3way-left)))
        (send switch-3way-left set-position! middle)
        (check-eq? (send switch-3way-left get-position) middle)))
     (test-case
      "check if middle-positioned connection stays middle"
      (let* ((connection (make-object connection% generic middle))
             (switch-3way-left (make-switch-3way-left-with connection)))
        (send switch-3way-left set-position! middle)
        (check-eq? (send connection get-position) middle)))
    
     (test-case
      "check if right-positioned switch-3way-left switches middle"
      (let* ((switch-3way-left (make-right-switch-3way-left)))
        (send switch-3way-left set-position! middle)
        (check-eq? (send switch-3way-left get-position) middle)))
     (test-case
      "check if right-positioned connection switches middle"
      (let* ((connection (make-object connection% generic right))
             (switch-3way-left (make-switch-3way-left-with connection)))
        (send switch-3way-left set-position! middle)
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
       (λ () (send (make-generic-switch-3way-generic) set-position! right))))
    
     (test-case
      "check if left-positioned switch-3way-left switches right"
      (let* ((switch-3way-left (make-left-switch-3way-left)))
        (send switch-3way-left set-position! right)
        (check-eq? (send switch-3way-left get-position) right)))
     (test-case
      "check if left-positioned connection switches right"
      (let* ((connection (make-object connection% generic left))
             (switch-3way-left (make-switch-3way-left-with connection)))
        (send switch-3way-left set-position! right)
        (check-eq? (send connection get-position) right)))

     (test-case
      "check if middle-positioned switch-3way-left switches right"
      (let* ((switch-3way-left (make-middle-switch-3way-left)))
        (send switch-3way-left set-position! right)
        (check-eq? (send switch-3way-left get-position) right)))
     (test-case
      "check if middle-positioned connection switches right"
      (let* ((connection (make-object connection% generic middle))
             (switch-3way-left (make-switch-3way-left-with connection)))
        (send switch-3way-left set-position! right)
        (check-eq? (send connection get-position) right)))
    
     (test-case
      "check if right-positioned switch-3way-left stays right"
      (let* ((switch-3way-left (make-right-switch-3way-left)))
        (send switch-3way-left set-position! right)
        (check-eq? (send switch-3way-left get-position) right)))
     (test-case
      "check if right-positioned connection stays right"
      (let* ((connection (make-object connection% generic right))
             (switch-3way-left (make-switch-3way-left-with connection)))
        (send switch-3way-left set-position! right)
        (check-eq? (send connection get-position) right)))
     ))

   ;; right
   (test-suite
    "Testing set-position! on switch-3way-right%"
   
    ;
    ; (test-set-position! 'left) test suites
    ;
    (test-suite
     "check 'set-position!' with argemunt 'left"

     (test-case
      "check if (set-position! 'left) doesn't error"
      (check-not-exn
       (λ () (send (make-generic-switch-3way-generic) set-position! left))))

     (test-case
      "check if left-positioned switch-3way-right stays left"
      (let* ((switch-3way-right (make-left-switch-3way-right)))
        (send switch-3way-right set-position! left)
        (check-eq? (send switch-3way-right get-position) left)))
     (test-case
      "check if left-positioned connection stays left"
      (let* ((connection (make-object connection% generic left))
             (switch-3way-right (make-switch-3way-right-with connection)))
        (send switch-3way-right set-position! left)
        (check-eq? (send connection get-position) left)))

     (test-case
      "check if middle-positioned switch-3way-right switches left"
      (let* ((switch-3way-right (make-middle-switch-3way-right)))
        (send switch-3way-right set-position! left)
        (check-eq? (send switch-3way-right get-position) left)))
     (test-case
      "check if middle-positioned connection switches lefts"
      (let* ((connection (make-object connection% generic middle))
             (switch-3way-right (make-switch-3way-right-with connection)))
        (send switch-3way-right set-position! left)
        (check-eq? (send connection get-position) left)))
    
     (test-case
      "check if right-positioned switch-3way-right switches left"
      (let* ((switch-3way-right (make-right-switch-3way-right)))
        (send switch-3way-right set-position! left)
        (check-eq? (send switch-3way-right get-position) left)))
     (test-case
      "check if right-positioned connection switches lefts"
      (let* ((connection (make-object connection% generic right))
             (switch-3way-right (make-switch-3way-right-with connection)))
        (send switch-3way-right set-position! left)
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
       (λ () (send (make-generic-switch-3way-generic) set-position! middle))))
    
     (test-case
      "check if left-positioned switch-3way-right stays middle"
      (let* ((switch-3way-right (make-left-switch-3way-right)))
        (send switch-3way-right set-position! middle)
        (check-eq? (send switch-3way-right get-position) middle)))
     (test-case
      "check if left-positioned connection stays middle"
      (let* ((connection (make-object connection% generic left))
             (switch-3way-right (make-switch-3way-right-with connection)))
        (send switch-3way-right set-position! middle)
        (check-eq? (send connection get-position) middle)))
    
     (test-case
      "check if middle-positioned switch-3way-right stays middle"
      (let* ((switch-3way-right (make-middle-switch-3way-right)))
        (send switch-3way-right set-position! middle)
        (check-eq? (send switch-3way-right get-position) middle)))
     (test-case
      "check if middle-positioned connection stays middle"
      (let* ((connection (make-object connection% generic middle))
             (switch-3way-right (make-switch-3way-right-with connection)))
        (send switch-3way-right set-position! middle)
        (check-eq? (send connection get-position) middle)))
    
     (test-case
      "check if right-positioned switch-3way-right switches middle"
      (let* ((switch-3way-right (make-right-switch-3way-right)))
        (send switch-3way-right set-position! middle)
        (check-eq? (send switch-3way-right get-position) middle)))
     (test-case
      "check if right-positioned connection switches middle"
      (let* ((connection (make-object connection% generic right))
             (switch-3way-right (make-switch-3way-right-with connection)))
        (send switch-3way-right set-position! middle)
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
       (λ () (send (make-generic-switch-3way-generic) set-position! right))))
    
     (test-case
      "check if left-positioned switch-3way-right switches right"
      (let* ((switch-3way-right (make-left-switch-3way-right)))
        (send switch-3way-right set-position! right)
        (check-eq? (send switch-3way-right get-position) right)))
     (test-case
      "check if left-positioned connection switches right"
      (let* ((connection (make-object connection% generic left))
             (switch-3way-right (make-switch-3way-right-with connection)))
        (send switch-3way-right set-position! right)
        (check-eq? (send connection get-position) right)))

     (test-case
      "check if middle-positioned switch-3way-right switches right"
      (let* ((switch-3way-right (make-middle-switch-3way-right)))
        (send switch-3way-right set-position! right)
        (check-eq? (send switch-3way-right get-position) right)))
     (test-case
      "check if middle-positioned connection switches right"
      (let* ((connection (make-object connection% generic middle))
             (switch-3way-right (make-switch-3way-right-with connection)))
        (send switch-3way-right set-position! right)
        (check-eq? (send connection get-position) right)))
    
     (test-case
      "check if right-positioned switch-3way-right stays right"
      (let* ((switch-3way-right (make-right-switch-3way-right)))
        (send switch-3way-right set-position! right)
        (check-eq? (send switch-3way-right get-position) right)))
     (test-case
      "check if right-positioned connection stays right"
      (let* ((connection (make-object connection% generic right))
             (switch-3way-right (make-switch-3way-right-with connection)))
        (send switch-3way-right set-position! right)
        (check-eq? (send connection get-position) right)))
     ))
   ))

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