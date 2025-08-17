;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                       >>> railway/algorithms/search-railway.test.rkt <<<                       ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require rackunit "search-railway.rkt")
(provide search-railway-test)

;
; abstractions
;
(define railway%
  (class object%
    (super-new)
    (define/public (get-detection-block-ids) '('d-1 'd-2 'd-3))
    (define/public (get-detection-block-next id) '(dblock . d-2))
    (define/public (get-detection-block-prev id) '(dblock . d-3))
     ))
(define railway (new railway%))
(define dblock-id 'd-1)

;
; setting up all individual test suites
;
(define test-search-reachable-dblocks
  (test-suite
   "Testing search-reachable-dblocks"
   (test-case
    "check if 'search-reachable-dblocks' exists"
    (check-not-exn (λ () search-reachable-dblocks)))
   (test-case
    "check if 'search-reachable-dblocks' doesn't error"
    (check-not-exn (λ () (search-reachable-dblocks railway dblock-id))))
   (test-case
    "check if 'search-reachable-dblocks' returns a list"
    (check-true (list? (search-reachable-dblocks railway dblock-id))))
   ))

;
; testing all test-suites
;
(define search-railway-test
  (test-suite
   "All search-railway algorithms test"
   test-search-reachable-dblocks))