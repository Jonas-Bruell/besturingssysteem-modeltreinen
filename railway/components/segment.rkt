;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> segment.rkt <<<                            ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide segment%)

(define segment%
  (class object%
    (super-new)

    ;
    ; This class represents an atomic abstraction of a railroad segment
    ;
    ; @param add-to-log-callback :: function to add a string to the log callback
    ; @param id :: the name of the railroad segment
    ; @param connection :: lower-level implementation of railroad segment
    ; @param in :: railway element that goes into this segment (clockwise)
    ; @param out :: railway element that exits out of this segment (clockwise)
    ;
    (init-field add-to-log id connection in out)
    (init-field (state (send connection get-state id)))
    (init-field (position (send connection get-position id)))

    ;
    ; add-to-log :: add a message of type string to the log
    ;
    ; @param string :: the string to be added to the log
    ;
    (define log-event (add-to-log (string-append "Segment '" (symbol->string id))))

    ;
    ; Possible railway segment states
    ;
    (define free     'free)
    (define reserved 'reserved)

    ;
    ; get-id :: get the id of the railway segment
    ;
    ; @returns symbol :: id-field of segment
    ;
    (define/public (get-id) id)

    ;
    ; get-state :: get the state of the railway segment
    ;
    ; @returns symbol :: state-field of segment
    ;
    (define/public (get-state) state)

    ;
    ; get-next :: get the next railway element in clockwise direction
    ;
    ; @returns symbol :: out-field of segment
    ;
    (define/public (get-next) out)

    ;
    ; get-prev :: get the previous railway element in clockwise direction
    ;
    ; @returns symbol :: in-field of segment
    ;
    (define/public (get-prev) in)

    ;
    ; set-state! :: change the state of the railway crossing only when it is
    ;               not yet in the required state
    ;
    ; @param new-state symbol :: the new state of the segment
    ; @returns boolean :: #f when trying to reserve a reserved state
    ;                     #t when trying to free a state
    ;
    (define/public (set-state! new-state)
      (cond ((and (eq? new-state state) (eq? reserved state))
             #f)
            ((and (eq? new-state state) (eq? free state))
             #t)
            ((or (and (eq? free new-state) (eq? reserved state))
                 (and (eq? reserved new-state) (eq? free state)))
             (send connection set-state! id new-state)
             (set! state new-state)
             (log-event "Method set-state! called"
                        (string-append "changed State to '" (symbol->string new-state)))
             #t)
            (else (error "segment%: wrong message sent: " new-state))))
    ))