;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                   >>> railway/algorithms/calculate-reservation-blocks.rkt <<<                  ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "search-railway.rkt")

(provide calculate-reservation-blocks)

(define (cdr/nil id) (if (eq? id 'NIL) 'NIL (cdr id)))

  (define (calculate-reservation-block-from railway dblock-id next-elem)
    (define elems-reachable-from-dblock '())
    (define (save! id)
      (set! elems-reachable-from-dblock (append elems-reachable-from-dblock (list id))))
    (search-reachable-dblocks* railway dblock-id next-elem save! save! save! save! save!)
    (remove-duplicates elems-reachable-from-dblock))

  (define (calculate-reservation-blocks railway)
    (define reservation-blocks '())
    (define (add-to-reservation-blocks-list! curr next elements-list)
      (set! reservation-blocks (append reservation-blocks (list (list curr next elements-list)))))
    (for-each
     (λ (dblock-id)
       (let ((next (send railway get-detection-block-next dblock-id))
             (prev (send railway get-detection-block-prev dblock-id)))
         (add-to-reservation-blocks-list!
          dblock-id (cdr/nil next) (calculate-reservation-block-from railway dblock-id next))
         (add-to-reservation-blocks-list!
          dblock-id (cdr/nil prev) (calculate-reservation-block-from railway dblock-id prev))))
     (send railway get-detection-block-ids))
    reservation-blocks)