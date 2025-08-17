;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                          >>> railway/algorithms/search-railway.rkt <<<                         ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(provide search-reachable-dblocks
         search-reachable-dblocks-only-next
         search-reachable-dblocks-only-prev
         search-reachable-dblocks*)

(define get-element-type car)
(define get-element-name cdr)

;
; search-reachable-dblocks :: get list of all reachable detection blocks.
;
; @param railway   :: railway-object
; @param dblock-id :: id of the "current" detectionblock, from which the search should start.
;
; @returns pair :: list of all reachable detection blocks from dblock-id without changing direction.
;
(define (search-reachable-dblocks railway dblock-id)
  (append
   (search-reachable-dblocks*
    railway dblock-id (send railway get-detection-block-next dblock-id) void void void void void)
   (search-reachable-dblocks*
    railway dblock-id (send railway get-detection-block-prev dblock-id) void void void void void)))

;
; search-reachable-dblocks-only-next :: get list of all reachable detection blocks when only
;                                       following the 'next' path of the first detection block.
;
; @param railway   :: railway-object
; @param dblock-id :: id of the "current" detectionblock, from which the search should start.
;
; @returns pair :: list of all reachable detection blocks from dblock-id without changing direction.
;
(define (search-reachable-dblocks-only-next railway dblock-id)
  (append
   (search-reachable-dblocks*
    railway dblock-id (send railway get-detection-block-next dblock-id) void void void void void)))

;
; search-reachable-dblocks-only-prev :: get list of all reachable detection blocks when only
;                                       following the 'prev' path of the verst detection block.
;
; @param railway   :: railway-object
; @param dblock-id :: id of the "current" detectionblock, from which the search should start.
;
; @returns pair :: list of all reachable detection blocks from dblock-id without changing direction.
;
(define (search-reachable-dblocks-only-prev railway dblock-id)
  (append
   (search-reachable-dblocks*
    railway dblock-id (send railway get-detection-block-prev dblock-id) void void void void void)))

;
; search-reachable-dblocks* :: get list of reachable detection blocks.
;
; @param railway   :: railway-object
; @param dblock-id :: id of the "current" detectionblock, from which the search should start.
; @param next-elem :: the railway element to reference (previous or next) in form '(type . id)
; @param dblock-λ  :: function called with parameter 'id everytime a detection-block gets passed 
; @param switch-λ  :: function called with parameter 'id everytime a switch gets passed
; @param segment-λ :: function called with parameter 'id everytime a segment gets passed
; @param switch3-λ :: function called with parameter 'id everytime a switch-3-way gets passed
; @param switchX-λ :: function called with parameter 'id everytime a switch-cross gets passed
;
; @returns pair :: list of all reachable detection blocks from dblock-id without changing direction.
;
(define (search-reachable-dblocks*
         railway dblock-id next-elem dblock-λ switch-λ segment-λ switch3-λ switchX-λ)
  (define reachable-dblocks-list '())
  (let search-rec ((previous (cons 'dblock dblock-id)) (current next-elem))
    (cond ((eq? current 'NIL) (void))
          ; dblock
          ((eq? (get-element-type current) 'dblock)
           (let* ((id (get-element-name current)))
             (dblock-λ current)
             (set! reachable-dblocks-list (append reachable-dblocks-list (list current)))))
          ; switch
          ((eq? (get-element-type current) 'switch)
           (let* ((id (get-element-name current))
                  (switch-prev       (send railway get-switch-prev       id))
                  (switch-next-left  (send railway get-switch-next-left  id))
                  (switch-next-right (send railway get-switch-next-right id)))
             (switch-λ current)
             (when (equal? switch-prev previous)
               (search-rec current switch-next-left)
               (search-rec current switch-next-right))
             (when (or (equal? switch-next-left  previous)
                       (equal? switch-next-right previous))
               (search-rec current switch-prev))))
          ; segment
          ((eq? (get-element-type current) 'segment)
           (let* ((id (get-element-name current))
                  (segment-prev (send railway get-segment-prev id))
                  (segment-next (send railway get-segment-next id)))
             (segment-λ current)
             (when (equal? segment-prev previous) (search-rec current segment-next))
             (when (equal? segment-next previous) (search-rec current segment-prev))))
          ; switch-three-way-out
          ((eq? (get-element-type current) 'switch3)
           (let* ((id (get-element-name current))
                  (switch3-prev        (send railway get-switch3-prev        id))
                  (switch3-next-left   (send railway get-switch3-next-left   id))
                  (switch3-next-middle (send railway get-switch3-next-middle id))
                  (switch3-next-right  (send railway get-switch3-next-right  id)))
             (switch3-λ current)
             (when (equal? switch3-prev previous)
               (search-rec current switch3-next-left)
               (search-rec current switch3-next-middle)
               (search-rec current switch3-next-right))
             (when (or (equal? switch3-next-left   previous)
                       (equal? switch3-next-middle previous)
                       (equal? switch3-next-right  previous))
               (search-rec current switch3-prev))))
          ; switch-english-cross
          ((eq? (get-element-type current) 'switchX)
           (let* ((id (get-element-name current))
                  (switchX-prev-left  (send railway get-switchX-prev-left  id))
                  (switchX-prev-right (send railway get-switchX-prev-right id))
                  (switchX-next-left  (send railway get-switchX-next-left  id))
                  (switchX-next-right (send railway get-switchX-next-right id)))
             (switchX-λ current)
             (when (or (equal? switchX-prev-left  previous)
                       (equal? switchX-prev-right previous))
               (search-rec current switchX-next-left)
               (search-rec current switchX-next-right))
             (when (or (equal? switchX-next-left  previous)
                       (equal? switchX-next-right previous))
               (search-rec current switchX-prev-left)
               (search-rec current switchX-prev-right))))
          (else
           (error "RAILWAY > algoritms > search-reachable-dblocks* > Doesn't exist : " current))))
  reachable-dblocks-list)
