;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                           >>> railway/algorithms/search-track.rkt <<<                          ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(provide search-reachable-dblocks
         search-reachable-dblocks*)

(define get-element-type car)
(define get-element-name cdr)

;
; search-reachable-dblocks :: get list of reachable detection blocks.
;
; @param railway :: railway-object
; @param dblock-id :: id of the "current" detectionblock, from which the search should start.
;
; @returns pair :: list of all reachable detection blocks from dblock-id without changing direction.
;
(define (search-reachable-dblocks railway dblock-id)
  (search-reachable-dblocks* railway dblock-id (λ (x) (void)) (λ (x) (void)) (λ (x) (void))))

;
; search-reachable-dblocks* :: get list of reachable detection blocks.
;
; @param railway :: railway-object
; @param dblock-id :: id of the "current" detectionblock, from which the search should start.
; @param dblock-λ :: function called with parameter 'id everytime a detection-block gets passed 
; @param switch-λ :: function called with parameter 'id everytime a switch gets passed
; @param segment-λ :: function called with parameter 'id everytime a segment gets passed
;
; @returns pair :: list of all reachable detection blocks from dblock-id without changing direction.
;
(define (search-reachable-dblocks* railway dblock-id dblock-λ switch-λ segment-λ)
  (define reachable-dblocks-list '())
  (define (search-rec previous current)
    (cond ((eq? current 'NIL) (void))
          ; dblock
          ((eq? (get-element-type current) 'dblock)
           (let* ((id (get-element-name current)))
             (dblock-λ id)
             (set! reachable-dblocks-list (append reachable-dblocks-list (list current)))))
          ; switch
          ((eq? (get-element-type current) 'switch)
           (let* ((id (get-element-name current))
                  (switch-prev (send railway get-switch-prev id))
                  (switch-next-left (send railway get-switch-next-left id))
                  (switch-next-right (send railway get-switch-next-right id)))
             (switch-λ id)
             (when (equal? switch-prev previous)
               (search-rec current switch-next-left)
               (search-rec current switch-next-right))
             (when (equal? switch-next-left previous) (search-rec current switch-prev))
             (when (equal? switch-next-right previous) (search-rec current switch-prev))))
          ; segment
          ((eq? (get-element-type current) 'segment)
           (let* ((id (get-element-name current))
                  (segment-prev (send railway get-segment-prev id))
                  (segment-next (send railway get-segment-next id)))
             (segment-λ id)
             (when (equal? segment-prev previous) (search-rec current segment-next))
             (when (equal? segment-next previous) (search-rec current segment-prev))))
          (else
           (error "RAILWAY > algoritms > search-reachable-dblocks > Doesn't exist : " current))))
  (search-rec (cons 'dblock dblock-id) (send railway get-detection-block-next dblock-id))
  (search-rec (cons 'dblock dblock-id) (send railway get-detection-block-prev dblock-id))
  reachable-dblocks-list)
