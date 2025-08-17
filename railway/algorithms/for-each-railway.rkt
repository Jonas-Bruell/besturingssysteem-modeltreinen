;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                         >>> railway/algorithms/for-each-railway.rkt <<<                        ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(provide for-each-railway-element)

(define (for-each-railway-element dblock-λ switch-λ segment-λ switch3-λ switchX-λ elements-list)
      (for-each
       (λ (element)
         (let ((type (car element))
               (id (cdr element)))
           (cond ((eq? type 'dblock)  (dblock-λ id))
                 ((eq? type 'switch)  (switch-λ id))
                 ((eq? type 'segment) (segment-λ id))
                 ((eq? type 'switch3) (switch3-λ id))
                 ((eq? type 'switchX) (switchX-λ id))
                 (else (error "RAILWAY > algorithms > for-each-element - Doesn't exist: " type)))))
       elements-list))