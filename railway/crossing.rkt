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

    ;
    ; This class represents an atomic abstraction of a railroad crossing
    ;
    ; @param id :: the name of the railroad crossing
    ; @param connection :: lower-level implementation of railroad crossing
    ; @param segment-list :: list of segments over which the crossing goes
    ;
    (init-field id connection segment-list)
    (field (state (send connection get-state)))

    ;
    ; Possible railway crossing states
    ;
    (define open    'open)
    (define pending 'pending)
    (define closed  'closed)

    ;
    ; get-id :: get the id of the railway crossing
    ;
    ; @returns symbol :: id-field of crossing
    ;
    (define/public (get-id) id)

    ;
    ; get-state :: get the state of the railway crossing
    ;
    ; @returns symbol :: state-field of crossing
    ;
    (define/public (get-state) state)

    ;
    ; get-segments :: get the ids of the segments in the crossing
    ;
    ; @returns list :: list of segment ids
    ;
    (define/public (get-segments) segment-list)

    ;
    ; set-state! :: change the state of the railway crossing only when it is
    ;               not yet in the required state
    ;
    ; @param new-state symbol :: the new state of the crossing - open or closed
    ;
    (define/public (set-state! new-state)
      (cond ((eq? new-state state)
             (void))
            ((member new-state (list open closed))
             (send connection set-state! new-state)
             (set! state pending)
             (thread
              (λ ()
                (let check-state ()
                  (sleep 1)
                  (if (eq? (send connection get-state) pending)
                      (check-state)
                      (set! state new-state))))))
            (else (error "crossing%: wrong message sent: " new-state))))
    ))
