;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                          >>> railway/components/crossing.test.rkt <<<                          ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require rackunit "crossing.rkt")

(provide crossing-test)

;
; aliasses
;
(define id      'id)
(define open    'open)
(define closed  'closed)
(define generic 'generic)
(define timeout  7)

;
; abstrations
;
(define connection%
  (class object%
    (super-new)
    (init-field position)
    (define/public (get-position id) position)
    (define/public (set-crossing-position! id new-position)
      (set! position new-position))
    ))

(define make-connection
  (λ () (make-object connection% generic)))

(define add-to-log (curry (λ (x y z) (void))))

(define (make-crossing-with connection segment-list)
  (make-object crossing% add-to-log id connection segment-list))

(define make-generic-crossing
  (λ () (make-crossing-with (make-connection) '())))

(define make-open-crossing
  (λ () (make-crossing-with (make-object connection% open) '())))

(define make-closed-crossing
  (λ () (make-crossing-with (make-object connection% closed) '())))

;
; test-make-object test suites
;
(define test-make-object
  (test-suite
   "Testing make-object"
   
   (test-case
    "check if 'crossing%' exists"
    (check-not-exn (λ () crossing%)))
   (test-case
    "check if constructor doesn't error"
    (check-not-exn (λ () (make-generic-crossing))))
   (test-case
    "check if constructor returns an object"
    (check-true (object? (make-generic-crossing))))
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
     (object-method-arity-includes? (make-generic-crossing) 'get-id 0)))
   (test-case
    "check if 'get-id' doesn't error"
    (check-not-exn (λ () (send (make-generic-crossing) get-id))))
   (test-case
    "check if 'get-id' returns 'id"
    (check-eq? (send (make-generic-crossing) get-id) id))
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
     (object-method-arity-includes? (make-generic-crossing) 'get-position 0)))
   (test-case
    "check if 'get-position' doesn't error"
    (check-not-exn (λ () (send (make-generic-crossing) get-position))))
   (test-case
    "check if 'get-position' returns 'generic"
    (check-eq? (send (make-generic-crossing) get-position) generic))
   ))

;
; test-get-segments test suites
;
(define test-get-segments
  (test-suite
   "Testing get-segments"

   (test-case
    "check if 'get-segments' exists"
    (check-true
     (object-method-arity-includes? (make-generic-crossing) 'get-segments 0)))
   (test-case
    "check if 'get-segments' doesn't error"
    (check-not-exn (λ () (send (make-generic-crossing) get-segments))))
   (test-case
    "check if 'get-segments' returns a list"
    (check-true (list? (send (make-generic-crossing) get-segments))))
   (test-case
    "check if 'get-segments' returns a empty list when no segements"
    (check-true (null? (send (make-crossing-with (make-connection) '())
                             get-segments))))
   (test-case
    "check if 'get-segments' returns an atomic list when 1 segements"
    (let ((segment-list
           (send (make-crossing-with (make-connection) '(segment))
                 get-segments)))
      (check-true (and (eq? (car segment-list) 'segment)
                       (null? (cdr segment-list))))))
   (test-case
    "check if 'get-segments' returns a list with 2 elements when 2 segements"
    (let ((segment-list
           (send (make-crossing-with (make-connection) '(segment1 segment2))
                 get-segments)))
      (check-true (and (eq? (car segment-list) 'segment1)
                       (eq? (cadr segment-list) 'segment2)
                       (null? (cddr segment-list))))))
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
     (object-method-arity-includes? (make-generic-crossing) 'set-position! 1)))
   (test-case
    "check if 'set-position!' does error when calling with wrong message"
    (check-exn exn:fail?
               (λ () (send (make-generic-crossing) set-position! 'wrong))))

   ;
   ; (test-set-position! 'open) test suites
   ;
   (test-suite
    "check 'set-position!' with argemunt 'open"

    (test-case
     "check if (set-position! 'open) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-crossing) set-position! open) (sleep timeout))))
    
    (test-case
     "check if closed crossing opens"
     (let* ((crossing (make-closed-crossing)))
       (send crossing set-position! open)
       (sleep timeout)
       (check-eq? (send crossing get-position) open)))
    (test-case
     "check if closed connection opens"
     (let* ((connection (make-object connection% closed))
            (crossing (make-crossing-with connection '())))
       (send crossing set-position! open)
       (sleep timeout)
       (check-eq? (send connection get-position id) open)))
    
    (test-case
     "check if open crossing stays open"
     (let* ((crossing (make-open-crossing)))
       (send crossing set-position! open)
       (check-eq? (send crossing get-position) open)))
    (test-case
     "check if open connection stays open"
     (let* ((connection (make-object connection% open))
            (crossing (make-crossing-with connection '())))
       (send crossing set-position! open)
       (check-eq? (send connection get-position id) open)))
    )

   ;
   ; (test-set-position! 'closed) test suites
   ;
   (test-suite
    "check 'set-position!' with argument 'closed"

    (test-case
     "check if (set-position! 'closed) doesn't error"
     (check-not-exn
      (λ () (send (make-generic-crossing) set-position! closed) (sleep timeout))))
    
    (test-case
     "check if open crossing closes"
     (let* ((crossing (make-open-crossing)))
       (send crossing set-position! closed)
       (sleep timeout)
       (check-eq? (send crossing get-position) closed)))
    (test-case
     "check if open connection closes"
     (let* ((connection (make-object connection% open))
            (crossing (make-crossing-with connection '())))
       (send crossing set-position! closed)
       (sleep timeout)
       (check-eq? (send connection get-position id) closed)))
    
    (test-case
     "check if closed crossing stays closed"
     (let* ((crossing (make-closed-crossing)))
       (send crossing set-position! closed)
       (check-eq? (send crossing get-position) closed)))
    (test-case
     "check if closed connection stays closed"
     (let* ((connection (make-object connection% closed))
            (crossing (make-crossing-with connection '())))
       (send crossing set-position! closed)
       (check-eq? (send connection get-position id) closed)))
    )))

;
; running all test suites
;
(define crossing-test
  (test-suite "All crossing% operation tests"
              test-make-object
              test-get-id
              test-get-position
              test-get-segments
              test-set-position!
              ))