;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> light.test.rkt <<<                          ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require rackunit "light.rkt")
(provide light-test)

;
; aliasses
;
(define Hp0          'Hp0)
(define Hp1          'Hp1)
(define Hp0+Sh0      'Hp0+Sh0)
(define Ks1+Zs3      'Ks1+Zs3)
(define Ks2          'Ks2)
(define Ks2+Zs3      'Ks2+Zs3)
(define Sh1          'Sh1)
(define Ks1+Zs3+Zs3v 'Ks1+Zs3+Zs3v)
(define generic      'generic)
(define segment      'segment)

;
; abstrations
;
(define connection%
  (class object%
    (super-new)
    (init-field signal)
    (define/public (get-signal) signal)
    (define/public (set-signal! new-signal)
      (set! signal new-signal))))

(define make-connection
  (λ () (make-object connection% generic)))

(define make-generic-light
  (λ () (make-object light% 'id (make-connection) segment)))

(define make-Hp0-light
  (λ () (make-object light% 'id (make-object connection% Hp0) segment)))

(define make-Hp1-light
  (λ () (make-object light% 'id (make-object connection% Hp1) segment)))

(define make-Hp0+Sh0-light
  (λ () (make-object light% 'id (make-object connection% Hp0+Sh0) segment)))

(define make-Ks1+Zs3-light
  (λ () (make-object light% 'id (make-object connection% Ks1+Zs3) segment)))

(define make-Ks2-light
  (λ () (make-object light% 'id (make-object connection% Ks2) segment)))

(define make-Ks2+Zs3-light
  (λ () (make-object light% 'id (make-object connection% Ks2+Zs3) segment)))

(define make-Sh1-light
  (λ () (make-object light% 'id (make-object connection% Sh1) segment)))

(define make-Ks1+Zs3+Zs3v-light
  (λ ()
    (make-object light% 'id (make-object connection% Ks1+Zs3+Zs3v) segment)))

;
; test-make-object test suites
;
(define test-make-object
  (test-suite
   "Testing make-object"
   
   (test-case
    "check if 'light%' exists"
    (check-not-exn (λ () light%)))
   (test-case
    "check if constructor doesn't error"
    (check-not-exn (λ () (make-object light% 'id (make-connection) segment))))
   (test-case
    "check if constructor returns an object"
    (check-true (object? (make-object light% 'id (make-connection) segment))))
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
     (object-method-arity-includes? (make-generic-light) 'get-id 0)))
   (test-case
    "check if 'get-id' doesn't error"
    (check-not-exn (λ () (send (make-generic-light) get-id))))
   (test-case
    "check if 'get-id' returns 'id"
    (check-eq? (send (make-generic-light) get-id) 'id))
   ))

;
; test-get-signal test suites
;
(define test-get-signal
  (test-suite
   "Testing get-signal"

   (test-case
    "check if 'get-signal' exists"
    (check-true
     (object-method-arity-includes? (make-generic-light) 'get-signal 0)))
   (test-case
    "check if 'get-signal' doesn't error"
    (check-not-exn (λ () (send (make-generic-light) get-signal))))
   (test-case
    "check if 'get-signal' returns 'generic"
    (check-eq? (send (make-generic-light) get-signal) generic))
   ))

;
; test-get-segment test suites
;
(define test-get-segment
  (test-suite
   "Testing get-segment"

   (test-case
    "check if 'get-segment' exists"
    (check-true
     (object-method-arity-includes? (make-generic-light) 'get-segment 0)))
   (test-case
    "check if 'get-segment' doesn't error"
    (check-not-exn (λ () (send (make-generic-light) get-segment))))
   (test-case
    "check if 'get-segment' returns a symbol"
    (check-true (symbol? (send (make-generic-light) get-segment))))
   (test-case
    "check if 'get-segment' returns 'segment with generic light"
    (check-true (eq? (send (make-generic-light) get-segment) segment)))
   ))

;
; test-set-signal! test suites
;
(define test-set-signal!
  (test-suite
   "Testing set-signal"
   
   (test-case
    "check if 'set-signal!' exists"
    (check-true
     (object-method-arity-includes? (make-generic-light) 'set-signal! 1)))
   (test-case
    "check if 'set-signal!' does error when calling with wrong message"
    (check-exn exn:fail?
               (λ () (send (make-generic-light) set-signal! 'wrong))))

   ;
   ; (test-set-signal! 'Hp0) test suites
   ;
   (test-suite
    "check 'set-signal!' with argument 'Hp0"
   
    (test-case
     "check if 'set-signal!' doesn't error when calling 'Hp0"
     (check-not-exn (λ () (send (make-generic-light) set-signal! Hp0))))
    
    (test-case
     "check if generic light switches to 'Hp0 when calling it"
     (let* ((light (make-generic-light)))
       (send light set-signal! Hp0)
       (check-eq? (send light get-signal) Hp0)))
    (test-case
     "check if connection switches to 'Hp0 when calling it"
     (let* ((connection (make-connection))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Hp0)
       (check-eq? (send connection get-signal) Hp0)))
    
    (test-case
     "check if light stays on same signal when calling 'Hp0"
     (let* ((light (make-Hp0-light)))
       (send light set-signal! Hp0)
       (check-eq? (send light get-signal) Hp0)))
    (test-case
     "check if connection stays on same signal when calling 'Hp0"
     (let* ((connection (make-object connection% Hp0))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Hp0)
       (check-eq? (send connection get-signal) Hp0)))
    )

   ;
   ; (test-set-signal! 'Hp1) test suites
   ;
   (test-suite
    "check 'set-signal!' with argument 'Hp1"
    
    (test-case
     "check if 'set-signal!' doesn't error when calling 'Hp1"
     (check-not-exn (λ () (send (make-generic-light) set-signal! Hp1))))
    
    (test-case
     "check if generic light switches to 'Hp1 when calling it"
     (let* ((light (make-generic-light)))
       (send light set-signal! Hp1)
       (check-eq? (send light get-signal) Hp1)))
    (test-case
     "check if connection switches to 'Hp1 when calling it"
     (let* ((connection (make-connection))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Hp1)
       (check-eq? (send connection get-signal) Hp1)))
    
    (test-case
     "check if light stays on same signal when calling 'Hp1"
     (let* ((light (make-Hp1-light)))
       (send light set-signal! Hp1)
       (check-eq? (send light get-signal) Hp1)))
    (test-case
     "check if connection stays on same signal when calling 'Hp1"
     (let* ((connection (make-object connection% Hp1))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Hp1)
       (check-eq? (send connection get-signal) Hp1)))
    )

   ;
   ; (test-set-signal! 'Hp0+Sh0) test suites
   ;
   (test-suite
    "check 'set-signal!' with argument 'Hp0+Sh0"
    
    (test-case
     "check if 'set-signal!' doesn't error when calling 'Hp0+Sh0"
     (check-not-exn (λ () (send (make-generic-light) set-signal! Hp0+Sh0))))
    
    (test-case
     "check if generic light switches to 'Hp0+Sh0 when calling it"
     (let* ((light (make-generic-light)))
       (send light set-signal! Hp0+Sh0)
       (check-eq? (send light get-signal) Hp0+Sh0)))
    (test-case
     "check if connection switches to 'Hp0+Sh0 when calling it"
     (let* ((connection (make-connection))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Hp0+Sh0)
       (check-eq? (send connection get-signal) Hp0+Sh0)))
    
    (test-case
     "check if light stays on same signal when calling 'Hp0+Sh0"
     (let* ((light (make-Hp0+Sh0-light)))
       (send light set-signal! Hp0+Sh0)
       (check-eq? (send light get-signal) Hp0+Sh0)))
    (test-case
     "check if connection stays on same signal when calling 'Hp0+Sh0"
     (let* ((connection (make-object connection% Hp0+Sh0))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Hp0+Sh0)
       (check-eq? (send connection get-signal) Hp0+Sh0)))
    )

   ;
   ; (test-set-signal! 'Ks1+Zs3) test suites
   ;
   (test-suite
    "check 'set-signal!' with argument 'Ks1+Zs3"

    (test-case
     "check if 'set-signal!' doesn't error when calling 'Ks1+Zs3"
     (check-not-exn (λ () (send (make-generic-light) set-signal! Ks1+Zs3))))
    
    (test-case
     "check if generic light switches to 'Ks1+Zs3 when calling it"
     (let* ((light (make-generic-light)))
       (send light set-signal! Ks1+Zs3)
       (check-eq? (send light get-signal) Ks1+Zs3)))
    (test-case
     "check if connection switches to 'Ks1+Zs3 when calling it"
     (let* ((connection (make-connection))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Ks1+Zs3)
       (check-eq? (send connection get-signal) Ks1+Zs3)))
    
    (test-case
     "check if light stays on same signal when calling 'Ks1+Zs3"
     (let* ((light (make-Ks1+Zs3-light)))
       (send light set-signal! Ks1+Zs3)
       (check-eq? (send light get-signal) Ks1+Zs3)))
    (test-case
     "check if connection stays on same signal when calling 'Ks1+Zs3"
     (let* ((connection (make-object connection% Ks1+Zs3))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Ks1+Zs3)
       (check-eq? (send connection get-signal) Ks1+Zs3)))
    )

   ;
   ; (test-set-signal! 'Ks2) test suites
   ;
   (test-suite
    "check 'set-signal!' with argument 'Ks2"
   
    (test-case
     "check if 'set-signal!' doesn't error when calling 'Ks2"
     (check-not-exn (λ () (send (make-generic-light) set-signal! Ks2))))
    
    (test-case
     "check if generic light switches to 'Ks2 when calling it"
     (let* ((light (make-generic-light)))
       (send light set-signal! Ks2)
       (check-eq? (send light get-signal) Ks2)))
    (test-case
     "check if connection switches to 'Ks2 when calling it"
     (let* ((connection (make-connection))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Ks2)
       (check-eq? (send connection get-signal) Ks2)))
    
    (test-case
     "check if light stays on same signal when calling 'Ks2"
     (let* ((light (make-Ks2-light)))
       (send light set-signal! Ks2)
       (check-eq? (send light get-signal) Ks2)))
    (test-case
     "check if connection stays on same signal when calling 'Ks2"
     (let* ((connection (make-object connection% Ks2))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Ks2)
       (check-eq? (send connection get-signal) Ks2)))
    )

   ;
   ; (test-set-signal! 'Ks2+Zs3) test suites
   ;
   (test-suite
    "check 'set-signal!' with argument 'Ks2+Zs3"

    (test-case
     "check if 'set-signal!' doesn't error when calling 'Ks2+Zs3"
     (check-not-exn (λ () (send (make-generic-light) set-signal! Ks2+Zs3))))
    
    (test-case
     "check if generic light switches to 'Ks2+Zs3 when calling it"
     (let* ((light (make-generic-light)))
       (send light set-signal! Ks2+Zs3)
       (check-eq? (send light get-signal) Ks2+Zs3)))
    (test-case
     "check if connection switches to 'Ks2+Zs3 when calling it"
     (let* ((connection (make-connection))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Ks2+Zs3)
       (check-eq? (send connection get-signal) Ks2+Zs3)))
    
    (test-case
     "check if light stays on same signal when calling 'Ks2+Zs3"
     (let* ((light (make-Ks2+Zs3-light)))
       (send light set-signal! Ks2+Zs3)
       (check-eq? (send light get-signal) Ks2+Zs3)))
    (test-case
     "check if connection stays on same signal when calling 'Ks2+Zs3"
     (let* ((connection (make-object connection% Ks2+Zs3))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Ks2+Zs3)
       (check-eq? (send connection get-signal) Ks2+Zs3)))
    )

   ;
   ; (test-set-signal! 'Sh1) test suites
   ;
   (test-suite
    "check 'set-signal!' with argument 'Sh1"
    
    (test-case
     "check if 'set-signal!' doesn't error when calling 'Sh1"
     (check-not-exn (λ () (send (make-generic-light) set-signal! Sh1))))
    
    (test-case
     "check if generic light switches to 'Sh1 when calling it"
     (let* ((light (make-generic-light)))
       (send light set-signal! Sh1)
       (check-eq? (send light get-signal) Sh1)))
    (test-case
     "check if connection switches to 'Sh1 when calling it"
     (let* ((connection (make-connection))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Sh1)
       (check-eq? (send connection get-signal) Sh1)))
    
    (test-case
     "check if light stays on same signal when calling 'Sh1"
     (let* ((light (make-Sh1-light)))
       (send light set-signal! Sh1)
       (check-eq? (send light get-signal) Sh1)))
    (test-case
     "check if connection stays on same signal when calling 'Sh1"
     (let* ((connection (make-object connection% Sh1))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Sh1)
       (check-eq? (send connection get-signal) Sh1)))
    )

   ;
   ; (test-set-signal! 'Ks1+Zs3+Zs3v) test suites
   ;
   (test-suite
    "check 'set-signal!' with argument 'Ks1+Zs3+Zs3v"
    
    (test-case
     "check if 'set-signal!' doesn't error when calling 'Ks1+Zs3+Zs3v"
     (check-not-exn
      (λ () (send (make-generic-light) set-signal! Ks1+Zs3+Zs3v))))
    
    (test-case
     "check if generic light switches to 'Ks1+Zs3+Zs3v when calling it"
     (let* ((light (make-generic-light)))
       (send light set-signal! Ks1+Zs3+Zs3v)
       (check-eq? (send light get-signal) Ks1+Zs3+Zs3v)))
    (test-case
     "check if connection switches to 'Ks1+Zs3+Zs3v when calling it"
     (let* ((connection (make-connection))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Ks1+Zs3+Zs3v)
       (check-eq? (send connection get-signal) Ks1+Zs3+Zs3v)))
    
    (test-case
     "check if light stays on same signal when calling 'Ks1+Zs3+Zs3v"
     (let* ((light (make-Ks1+Zs3+Zs3v-light)))
       (send light set-signal! Ks1+Zs3+Zs3v)
       (check-eq? (send light get-signal) Ks1+Zs3+Zs3v)))
    (test-case
     "check if connection stays on same signal when calling 'Ks1+Zs3+Zs3v"
     (let* ((connection (make-object connection% Ks1+Zs3+Zs3v))
            (light (make-object light% 'id connection segment)))
       (send light set-signal! Ks1+Zs3+Zs3v)
       (check-eq? (send connection get-signal) Ks1+Zs3+Zs3v)))
    )))

;
; running all test suites
;
(define light-test
  (test-suite "All light% operation tests"
              test-make-object
              test-get-id
              test-get-signal
              test-get-segment
              test-set-signal!
              ))