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
    ; @param add-to-log-callback :: function to add a string to the log callback
    ; @param id :: the name of the railroad crossing
    ; @param connection :: lower-level implementation of railroad crossing
    ; @param segment-list :: list of segments over which the crossing goes
    ;
    (init-field add-to-log id connection segment-list)
    (init-field (position (send connection get-position id)))

    ;
    ; Possible railway crossing positions
    ;
    (define open    'open)
    (define pending 'pending)
    (define closed  'closed)

    ;
    ; add-to-log :: add a message of type string to the log
    ;
    ; @param string :: the string to be added to the log
    ;
    (define log-event (add-to-log (string-append "Crossing '" (symbol->string id)))) ; currying

    ;
    ; get-id :: get the id of the railway crossing
    ;
    ; @returns symbol :: id-field of crossing
    ;
    (define/public (get-id) id)

    ;
    ; get-positions :: get list of possible positions of the crossing
    ;
    ; @returns list :: list of possible positions
    ;
    (define/public (get-positions) (list open closed))

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
             (log-event "Method set-position! called"
                        (string-append "changing position to " (symbol->string new-position)))
             (thread
              (λ () (sleep 6) (set! position new-position)
                (log-event (string-append "completed changing position to '"
                                           (symbol->string new-position))))))
            (else (error "crossing%: wrong message sent: " new-position))))
    ))
