#lang racket
(require rackunit "track-interface.rkt")
(provide track-interface-test)

;
; aliasses
;
(define hardware 'hardware)

;
; abstractions
;
(define (test-on-track methode . arguments)
  (let* ((track (new track%))
         (conf (send track config 'sim 'hardware))
         (strt (send track start))
         (mthd (send track methode arguments))
         (stp  (send track stop)))
    mthd))

; setting up all individual test suites
(define (test-start architecture-setup)
  (test-suite
   "Testing start"
   (test-case
    "check if 'start' exists"
    (object-method-arity-includes? (new track%) 'start 0))
   (test-case
    "check if 'start' doesn't error"
    (let ((track (new track%)))
      ;(send track config architecture-setup hardware)
      (check-not-exn (λ () (send track start)))
      (send track stop)))
   ))
(define (test-stop architecture-setup)
  (test-suite
   "Testing stop"
   (test-case
    "check if 'stop' exists"
    (object-method-arity-includes? (new track%) 'stop 0))
   (test-case
    "check if 'stop' doesn't error"
    (let ((track (new track%)))
      ;(send track config architecture-setup hardware)
      (send track start)
      (check-not-exn (λ () (send track stop)))))
   ))
#|
(define (test-add-loco architecture-setup)
  (test-suite
   "Testing add-loco"
   (test-case
    "check if 'add-loco' exists"
    (check-not-exn (λ () add-loco)))
   ))
(define (test-get-loco-speed architecture-setup)
  (test-suite
   "Testing get-loco-speed"
   (test-case
    "check if 'get-loco-speed' exists"
    (check-not-exn (λ () get-loco-speed)))
   ))
(define (test-set-loco-speed! architecture-setup)
  (test-suite
   "Testing set-loco-speed!"
   (test-case
    "check if 'set-loco-speed!' exists"
    (check-not-exn (λ () set-loco-speed!)))
   ))
(define (test-get-detection-block-ids architecture-setup)
  (test-suite
   "Testing get-detection-blocks-ids"
   (test-case
    "check if 'get-detection-blocks-ids' exists"
    (check-not-exn (λ () get-detection-block-ids)))
   ))
(define (test-get-occupied-detection-blocks architecture-setup)
  (test-suite
   "Testing get-occupied-detection-blocks"
   (test-case
    "check if 'get-occupied-detection-blocks' exists"
    (check-not-exn (λ () get-occupied-detection-blocks)))
   ))
(define (test-get-switch-ids architecture-setup)
  (test-suite
   "Testing get-switch-ids"
   (test-case
    "check if 'get-switch-ids' exists"
    (check-not-exn (λ () get-switch-ids)))
   ))
(define (test-get-switch-position architecture-setup)
  (test-suite
   "Testing get-switch-position"
   (test-case
    "check if 'get-switch-position' exists"
    (check-not-exn (λ () get-switch-position)))
   ))
(define (test-set-switch-position! architecture-setup)
  (test-suite
   "Testing set-switch-position!"
   (test-case
    "check if 'set-switch-position!' exists"
    (check-not-exn (λ () set-switch-position!)))
   ))
(define (test-open-crossing! architecture-setup)
  (test-suite
   "Testing open-crossing!"
   (test-case
    "check if 'open-crossing!' exists"
    (check-not-exn (λ () open-crossing!)))
   ))
(define (test-close-crossing! architecture-setup)
  (test-suite
   "Testing close-crossing!"
   (test-case
    "check if 'close-crossing!' exists"
    (check-not-exn (λ () close-crossing!)))
   ))
(define (test-set-sign-code! architecture-setup)
  (test-suite
   "Testing set-sign-code!"
   (test-case
    "check if 'set-sign-code!' exists"
    (check-not-exn (λ () set-sign-code!)))
   ))
|#
; testing all test suites
(define (track-interface-test architecture-setup)
  (send (new track%) config architecture-setup hardware)
  (test-suite
   "Track interface tests"
   (test-start architecture-setup)
   (test-stop architecture-setup)
   ;(test-add-loco architecture-setup)
   ;(test-get-loco-speed architecture-setup)
   ;(test-set-loco-speed! architecture-setup)
   ;(test-get-detection-block-ids architecture-setup)
   ;(test-get-occupied-detection-blocks architecture-setup)
   ;(test-get-switch-ids architecture-setup)
   ;(test-get-switch-position architecture-setup)
   ;(test-set-switch-position! architecture-setup)
   ;(test-open-crossing! architecture-setup)
   ;(test-close-crossing! architecture-setup)
   ;(test-set-sign-code! architecture-setup)
   ))