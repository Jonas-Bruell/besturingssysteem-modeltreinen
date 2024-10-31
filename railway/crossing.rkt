;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> crossing.rkt <<<                            ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide crossing%)

(define crossing%
  (class object%
    (super-new)

    ; This class represents an atomic abstraction of a railroad crossing
    ;
    ; @param connection :: lower-level implementation of railroad crossing
    ; @param id :: the name of the railroad crossing
    ; @param state :: the state of the railroad crossing - open or closed
    ;
    (init-field connection id state)
    (define open 'open)
    (define close 'close)

    ; get-id :: get the id of the railway crossing
    ;
    ; @returns id-field of crossing
    ;
    (define/public (get-id) id)
    
    ; get-state :: get the state of the railway crossing
    ;
    ; @returns state-field of crossing
    ;
    (define/public (get-state) state)
    
    ; set-state! :: change the state of the railway crossing only when it is
    ; not yet in the required state
    ;
    ; @param new-state :: the new state of the crossing - open or closed
    ;
    (define/public (set-state! new-state)
      (cond ((or (and (eq? new-state open) (eq? state open))
                 (and (eq? new-state close) (eq? state close)))
             (void))
            ((or (eq? new-state open) (eq? new-state close))
             (set! state new-state)
             (send connection set-state! new-state)
             ; wait 6 seconds, maybe an 'opening' or 'closing' state?
             ; seperate thread?
             ; check if real life version is really closed?
             )
            (else (error "crossing%: wrong message sent: " new-state))))
    ))
