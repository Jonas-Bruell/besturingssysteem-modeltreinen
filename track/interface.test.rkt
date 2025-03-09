#lang racket
(require rackunit "interface.rkt")
(provide track-interface-test)


;    !!! WARNING: THESE TESTS HAVE SIDE EFFECTS & INFLUENCE EACH OTHER !!!


;
; aliasses
;
(define versions       '("hardware" "loop-and-switches" "loop" "straight-with-switch" "straight"))
(define switch-ids-sim '(S-1 S-2 S-3 S-4 S-5 S-6 S-7 S-8 S-9 S-10 S-11 S-12 S-16 S-20 S-23 S-24 S-25 S-26 S-27 S-28))
(define switch-ids-hw  '(S-16 S-1 S-28 S-8 S-7 S-4 S-26 S-12 S-5 S-27 S-11 S-2 S-24 S-3 S-25 S-9 S-20 S-6 S-23 S-10))
(define detect-ids-sim '(1-1 1-2 1-3 1-4 1-5 1-6 1-7 1-8 2-1 2-2 2-3 2-4 2-5 2-6 2-7 2-8))
(define detect-ids-hw  '(1-1 1-2 1-3 1-4 1-5 1-6 1-7 1-8 2-1 2-2 2-3 2-4 2-5 2-6 2-7 2-8))

;
; setting up all individual test suites
;
(define (test-get-versions track)
  (test-case
   "check if 'get-versions' returns all versions"
   (check-equal? (send track get-versions) versions)))

(define (test-open-crossing! track)
  (test-suite
   "Testing open-crossing!"
   (test-case
    "check if 'open-crossing!' exists"
    (object-method-arity-includes? track 'open-crossing! 0))
   (test-case
    "check if 'open-crossing!' doesn't error"
    (check-not-exn (λ () (send track open-crossing! 'C-1))))))

(define (test-close-crossing! track)
  (test-suite
   "Testing close-crossing!"
   (test-case
    "check if 'close-crossing!' exists"
    (object-method-arity-includes? track 'close-crossing! 0))
   (test-case
    "check if 'close-crossing!' doesn't error"
    (check-not-exn (λ () (send track close-crossing! 'C-1))))))

(define (test-set-sign-code! track)
  (test-suite
   "Testing set-sign-code!"
   (test-case
    "check if 'set-sign-code!' exists"
    (object-method-arity-includes? track 'set-sign-code! 0))
   (test-case
    "check if 'set-sign-code!' doesn't error"
    (check-not-exn (λ () (send track set-sign-code! 'L-1 'Hp0))))))

(define (test-get-switch-ids track architecture)
  (test-suite
   "Testing get-switch-ids"
   (test-case
    "check if 'get-switch-ids' exists"
    (object-method-arity-includes? track 'get-switch-ids 0))
   (test-case
    "check if 'get-switch-ids' doesn't error"
    (check-not-exn (λ () (send track get-switch-ids))))
   (test-case
    "check if 'get-switch-ids' returns correct switch ids"
    (check-equal? (send track get-switch-ids)
                  (if (eq? architecture 'sim) switch-ids-sim switch-ids-hw)))))

(define (test-get-set-switch-position track architecture)
  (let ((switch-ids (send track get-switch-ids)))
    (test-suite
     "Testing get-switch-position & set-switch-position!"
     (test-case
      "check if 'get-switch-position' exists"
      (object-method-arity-includes? track 'get-switch-position 0))
     (test-case
      "check if 'set-switch-position!' exists"
      (object-method-arity-includes? track 'set-switch-position! 0))
     (test-case
      "check if 'get-switch-position' doesn't error"
      (check-not-exn (λ () (send track get-switch-position (car switch-ids)))))
     (test-case
      "check if 'set-switch-position!' doesn't error"
      (check-not-exn (λ () (send track set-switch-position! (car switch-ids) 1))))
     (test-case
      "check if all switches can be set to position '1'"
      (for-each (λ (switch) (send track set-switch-position! switch 1)) switch-ids)
      (when (eq? architecture 'hw) (sleep 1))
      (check-equal? (map (λ (switch) (send track get-switch-position switch)) switch-ids)
                    '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
     (test-case
      "check if all switches can be set to position '2'"
      (for-each (λ (switch) (send track set-switch-position! switch 2)) switch-ids)
      (when (eq? architecture 'hw) (sleep 1))
      (check-equal? (map (λ (switch) (send track get-switch-position switch)) switch-ids)
                    '(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))))))

(define (test-get-detection-block-ids track architecture)
  (test-suite
   "Testing get-detection-blocks-ids"
   (test-case
    "check if 'get-detection-blocks-ids' exists"
    (object-method-arity-includes? track 'get-detection-blocks-ids 0))
   (test-case
    "check if 'get-detection-blocks-ids' doesn't error"
    (check-not-exn (λ () (send track get-detection-block-ids))))
   (test-case
    "check if 'get-detection-blocks-ids' returns correct detection block ids"
    (check-equal? (send track get-detection-block-ids)
                  (if (eq? architecture 'sim) detect-ids-sim detect-ids-hw)))))

(define (test-get-occupied-detection-blocks-no-train track)
  (test-suite
   "Testing get-occupied-detection-blocks without trains"
   (test-case
    "check if 'get-occupied-detection-blocks' exists"
    (object-method-arity-includes? track 'get-occupied-detection-blocks 0))
   (test-case
    "check if 'get-occupied-detection-blocks' doesn't error"
    (check-not-exn (λ () (send track get-occupied-detection-blocks))))
   (test-case
    "check if 'get-occupied-detection-blocks' returns no occupied blocks"
    (check-true (null? (send track get-occupied-detection-blocks))))))

(define (test-add-loco track)
  (test-suite
   "Testing add-loco"
   (test-case
    "check if 'add-loco' exists"
    (object-method-arity-includes? track 'add-loco 0))
   (test-case
    "check if 'add-loco' doesn't error"
    (check-not-exn (λ () (send track add-loco 'T-3 '1-4 '1-5))))))

(define (test-get-set-loco-speed track)
  (test-suite
   "Testing get-loco-speed"
   (test-case
    "check if 'get-loco-speed' exists"
    (object-method-arity-includes? track 'get-loco-speed 0))
   (test-case
    "check if 'set-loco-speed!' exists"
    (object-method-arity-includes? track 'set-loco-speed! 0))
   (test-case
    "check if 'get-loco-speed' doesn't error"
    (check-not-exn (λ () (send track get-loco-speed 'T-3))))
   (test-case
    "check if 'set-loco-speed!' doesn't error"
    (check-not-exn (λ () (send track set-loco-speed! 'T-3 0))))
   (test-case
    "check if 'get-loco-speed' returns '0'"
    (check-eq? 0 (send track get-loco-speed 'T-3)))
   (test-case
    "check if 'get-loco-speed' returns '1' when speed is '1'"
    (send track set-loco-speed! 'T-3 1)
    (check-eq? 1 (send track get-loco-speed 'T-3))
    (send track set-loco-speed! 'T-3 0))))

(define (test-get-occupied-detection-blocks-with-train track)
  (test-case
   "check if 'get-occupied-detection-blocks' returns occupied blocks"
   (check-false (null? (send track get-occupied-detection-blocks)))))

;
; testing all test suites
;
(define (track-interface-test architecture)
  (let* ((track (new track%))
         (config (send track config architecture 'hardware))
         (start (send track start))
         (tests (test-suite
                 "Track interface tests"
                 (test-get-versions track)
                 (test-open-crossing! track)
                 (test-close-crossing! track)
                 (test-set-sign-code! track)
                 (test-get-switch-ids track architecture)
                 (test-get-set-switch-position track architecture)
                 (test-get-detection-block-ids track architecture)
                 (test-get-occupied-detection-blocks-no-train track)
                 (test-add-loco track)
                 (test-get-set-loco-speed track)
                 (test-get-occupied-detection-blocks-with-train track)
                 ))
         (stop (send track stop)))
    tests))