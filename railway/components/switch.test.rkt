;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                           >>> switch.test.rkt <<<                          ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit "switch.rkt")
(provide switch-test)

;
; aliasses
;
(define id       'id)
(define free     'free)
(define reserved 'reserved)
(define left     'left)
(define right    'right)
(define generic  'generic)
(define in       'in)
(define out      (cons left right))

;
; abstrations
;
(define connection%
  (class object%
    (super-new)
    (init-field state pos)
    (define/public (get-state id) state)
    (define/public (get-position id) pos)
    (define/public (set-state! id new-state) (set! state new-state))
    (define/public (set-switch-position! id new-pos) (set! pos new-pos))))

(define (make-switch-with connection)
  (make-object switch% id connection in out))

(define make-generic-switch
  (λ () (make-switch-with (make-object connection% generic generic))))

(define make-free-switch
  (λ () (make-switch-with (make-object connection% free generic))))

(define make-reserved-switch
  (λ () (make-switch-with (make-object connection% reserved generic))))

(define make-left-switch
  (λ () (make-switch-with (make-object connection% generic left))))

(define make-right-switch
  (λ () (make-switch-with (make-object connection% generic right))))

;
; test-make-object test suites
;
(define test-make-object
  (test-suite
   "Testing make-object"
   
   (test-case
    "check if 'switch%' exists"
    (check-not-exn (λ () switch%)))
   (test-case
    "check if constructor doesn't error"
    (check-not-exn (λ () (make-generic-switch))))
   (test-case
    "check if constructor returns an object"
    (check-true (object? (make-generic-switch))))
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
     (object-method-arity-includes? (make-generic-switch) 'get-id 0)))
   (test-case
    "check if 'get-id' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch) get-id))))
   (test-case
    "check if 'get-id' returns 'id"
    (check-eq? (send (make-generic-switch) get-id) id))
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
     (object-method-arity-includes? (make-generic-switch) 'get-prev 0)))
   (test-case
    "check if 'get-prev' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch) get-prev))))
   (test-case
    "check if 'get-id' returns 'in"
    (check-eq? (send (make-generic-switch) get-prev) in))
   ))

;
; test-get-next-left test suites
;
(define test-get-next-left
  (test-suite
   "Testing get-next-left"

   (test-case
    "check if 'get-next-left' exists"
    (check-true
     (object-method-arity-includes? (make-generic-switch) 'get-next-left 0)))
   (test-case
    "check if 'get-next-left' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch) get-next-left))))
   (test-case
    "check if 'get-next-left' returns 'left"
    (check-eq? (send (make-generic-switch) get-next-left) left))
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
     (object-method-arity-includes? (make-generic-switch) 'get-next-right 0)))
   (test-case
    "check if 'get-next-right' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch) get-next-right))))
   (test-case
    "check if 'get-next-right' returns 'right"
    (check-eq? (send (make-generic-switch) get-next-right) right))
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
     (object-method-arity-includes? (make-generic-switch) 'get-state 0)))
   (test-case
    "check if 'get-state' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch) get-state))))
   (test-case
    "check if 'get-state' returns 'generic"
    (check-eq? (send (make-generic-switch) get-state) generic))
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
     (object-method-arity-includes? (make-generic-switch) 'get-position 0)))
   (test-case
    "check if 'get-position' doesn't error"
    (check-not-exn (λ () (send (make-generic-switch) get-position))))
   (test-case
    "check if 'get-position' returns 'generic"
    (check-eq? (send (make-generic-switch) get-position) generic))
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
     (object-method-arity-includes? (make-generic-switch) 'set-state! 1)))
   (test-case
    "check if (set-state! 'free) doesn't error on free switch"
    (check-not-exn (λ () (send (make-free-switch) set-state! 'free))))
   (test-case
    "check if (set-state! 'reserved) doesn't error on free switch"
    (check-not-exn (λ () (send (make-free-switch) set-state! 'reserved))))
   (test-case
    "check if (set-state! 'free) doesn't error on reserved switch"
    (check-not-exn (λ () (send (make-reserved-switch) set-state! 'free))))
   (test-case
    "check if (set-state! 'reserved) doesn't error on reserved switch"
    (check-not-exn (λ () (send (make-reserved-switch) set-state! 'reserved))))
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
     (object-method-arity-includes? (make-generic-switch) 'set-position! 1)))
   (test-case
    "check if 'set-position!' does error when calling with wrong message"
    (check-exn exn:fail?
               (λ () (send (make-generic-switch) set-position! 'wrong))))

   ;
   ; (test-set-position! 'left) test suites
   ;
   (test-suite
    "check 'set-position!' with argemunt 'left"

    (test-case
     "check if (set-position! 'left) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch) set-position! left))))
    
    (test-case
     "check if right-positioned switch switches left"
     (let* ((switch (make-right-switch)))
       (send switch set-position! left)
       (check-eq? (send switch get-position) left)))
    (test-case
     "check if right-positioned connection switches lefts"
     (let* ((connection (make-object connection% generic right))
            (switch (make-switch-with connection)))
       (send switch set-position! left)
       (check-eq? (send connection get-position id) left)))
    
    (test-case
     "check if left-positioned switch stays left"
     (let* ((switch (make-left-switch)))
       (send switch set-position! left)
       (check-eq? (send switch get-position) left)))
    (test-case
     "check if left-positioned connection stays left"
     (let* ((connection (make-object connection% generic left))
            (switch (make-switch-with connection)))
       (send switch set-position! left)
       (check-eq? (send connection get-position id) left)))
    )

   ;
   ; (test-set-position! 'right) test suites
   ;
   (test-suite
    "check 'set-position!' with argument 'right"

    (test-case
     "check if (set-position! 'right) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-switch) set-position! right))))
    
    (test-case
     "check if left-positioned switch switches right"
     (let* ((switch (make-left-switch)))
       (send switch set-position! right)
       (check-eq? (send switch get-position) right)))
    (test-case
     "check if left-positioned connection switches right"
     (let* ((connection (make-object connection% generic left))
            (switch (make-switch-with connection)))
       (send switch set-position! right)
       (check-eq? (send connection get-position id) right)))
    
    (test-case
     "check if right-positioned switch stays right"
     (let* ((switch (make-right-switch)))
       (send switch set-position! right)
       (check-eq? (send switch get-position) right)))
    (test-case
     "check if right-positioned connection stays right"
     (let* ((connection (make-object connection% generic right))
            (switch (make-switch-with connection)))
       (send switch set-position! right)
       (check-eq? (send connection get-position id) right)))
    )))

;
; running all test suites
;
(define switch-test
  (test-suite "All switch% operation tests"
              test-make-object
              test-get-id
              test-get-prev
              test-get-next-left
              test-get-next-right
              test-get-state
              test-get-position
              test-set-state!
              test-set-position!
              ))