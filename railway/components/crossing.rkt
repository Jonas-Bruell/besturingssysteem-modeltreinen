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
    (init-field (position (send connection get-position id)))

    ;
    ; Possible railway crossing positions
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
    ; get-position :: get the position of the railway crossing
    ;
    ; @returns symbol :: position-field of crossing
    ;
    (define/public (get-position) position)

    ;
    ; get-segments :: get the ids of the segments in the crossing
    ;
    ; @returns list :: list of segment ids
    ;
    (define/public (get-segments) segment-list)

    ;
    ; set-position! :: change the position of the railway crossing only when
    ;                  it is not yet in the required position
    ;
    ; @param new-position symbol :: the new position of the crossing
    ;
    (define/public (set-position! new-position)
      (cond ((eq? new-position position)
             (void))
            ((member new-position (list open closed))
             (send connection set-crossing-position! id new-position)
             (set! position pending)
             (thread (λ () (sleep 6) (set! position new-position))))
            (else (error "crossing%: wrong message sent: " new-position))))
    ))
